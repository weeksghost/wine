/*
 * Plug and Play support for hid devices found through udev
 *
 * Copyright 2016 CodeWeavers, Aric Stewart
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

#if 0
#pragma makedep unix
#endif

#include "config.h"
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#ifdef HAVE_SYS_POLL_H
# include <sys/poll.h>
#endif
#ifdef HAVE_LIBUDEV_H
# include <libudev.h>
#endif
#ifdef HAVE_LINUX_HIDRAW_H
# include <linux/hidraw.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef HAVE_LINUX_INPUT_H
# include <linux/input.h>
# undef SW_MAX
# if defined(EVIOCGBIT) && defined(EV_ABS) && defined(BTN_PINKIE)
#  define HAS_PROPER_INPUT_HEADER
# endif
# ifndef SYN_DROPPED
#  define SYN_DROPPED 3
# endif
#endif

#include <pthread.h>

#include "ntstatus.h"
#define WIN32_NO_STATUS
#include "windef.h"
#include "winbase.h"
#include "winnls.h"
#include "winternl.h"
#include "ddk/wdm.h"
#include "ddk/hidtypes.h"
#include "ddk/hidsdi.h"
#include "wine/debug.h"
#include "wine/heap.h"
#include "wine/unicode.h"

#ifdef HAS_PROPER_INPUT_HEADER
# include "hidusage.h"
#endif

#ifdef WORDS_BIGENDIAN
#define LE_DWORD(x) RtlUlongByteSwap(x)
#else
#define LE_DWORD(x) (x)
#endif

#include "unix_private.h"

WINE_DEFAULT_DEBUG_CHANNEL(plugplay);

#ifdef HAVE_UDEV

WINE_DECLARE_DEBUG_CHANNEL(hid_report);

static pthread_mutex_t udev_cs = PTHREAD_MUTEX_INITIALIZER;

static struct udev *udev_context = NULL;
static struct udev_monitor *udev_monitor;
static int deviceloop_control[2];
static struct list event_queue = LIST_INIT(event_queue);
static struct list device_list = LIST_INIT(device_list);
static struct udev_bus_options options;

struct base_device
{
    struct unix_device unix_device;
    void (*read_report)(struct unix_device *iface);

    struct udev_device *udev_device;
    int device_fd;
};

struct hidraw_device
{
    struct base_device base;
};

#define HID_REL_MAX (REL_MISC+1)
#define HID_ABS_MAX (ABS_VOLUME+1)

struct lnxev_device
{
    struct base_device base;

    BYTE abs_map[HID_ABS_MAX];
    BYTE rel_map[HID_REL_MAX];
    BYTE hat_map[8];
    BYTE button_map[KEY_MAX];

    int haptic_effect_id;
};

static inline struct base_device *impl_from_unix_device(struct unix_device *iface)
{
    return CONTAINING_RECORD(iface, struct base_device, unix_device);
}

static inline struct hidraw_device *hidraw_impl_from_unix_device(struct unix_device *iface)
{
    return CONTAINING_RECORD(impl_from_unix_device(iface), struct hidraw_device, base);
}

static inline struct lnxev_device *lnxev_impl_from_unix_device(struct unix_device *iface)
{
    return CONTAINING_RECORD(impl_from_unix_device(iface), struct lnxev_device, base);
}

#define MAX_DEVICES 128
static int close_fds[MAX_DEVICES];
static struct pollfd poll_fds[MAX_DEVICES];
static struct base_device *poll_devs[MAX_DEVICES];
static int close_count, poll_count;

static void stop_polling_device(struct unix_device *iface)
{
    struct base_device *impl = impl_from_unix_device(iface);
    int i;

    if (impl->device_fd == -1) return; /* already removed */

    for (i = 2; i < poll_count; ++i)
        if (poll_fds[i].fd == impl->device_fd) break;

    if (i == poll_count)
        ERR("could not find poll entry matching device %p fd\n", iface);
    else
    {
        poll_count--;
        poll_fds[i] = poll_fds[poll_count];
        poll_devs[i] = poll_devs[poll_count];
        close_fds[close_count++] = impl->device_fd;
        impl->device_fd = -1;
    }
}

static void start_polling_device(struct unix_device *iface)
{
    struct base_device *impl = impl_from_unix_device(iface);

    if (poll_count >= ARRAY_SIZE(poll_fds))
        ERR("could not start polling device %p, too many fds\n", iface);
    else
    {
        poll_devs[poll_count] = impl;
        poll_fds[poll_count].fd = impl->device_fd;
        poll_fds[poll_count].events = POLLIN;
        poll_fds[poll_count].revents = 0;
        poll_count++;

        write(deviceloop_control[1], "u", 1);
    }
}

static struct base_device *find_device_from_fd(int fd)
{
    int i;

    for (i = 2; i < poll_count; ++i) if (poll_fds[i].fd == fd) break;
    if (i < poll_count) return  poll_devs[i];

    return NULL;
}

static const char *get_device_syspath(struct udev_device *dev)
{
    struct udev_device *parent;

    if ((parent = udev_device_get_parent_with_subsystem_devtype(dev, "hid", NULL)))
        return udev_device_get_syspath(parent);

    if ((parent = udev_device_get_parent_with_subsystem_devtype(dev, "usb", "usb_device")))
        return udev_device_get_syspath(parent);

    return "";
}

static struct base_device *find_device_from_syspath(const char *path)
{
    struct base_device *impl;

    LIST_FOR_EACH_ENTRY(impl, &device_list, struct base_device, unix_device.entry)
        if (!strcmp(get_device_syspath(impl->udev_device), path)) return impl;

    return NULL;
}

static struct base_device *find_device_from_udev(struct udev_device *dev)
{
    struct base_device *impl;

    LIST_FOR_EACH_ENTRY(impl, &device_list, struct base_device, unix_device.entry)
        if (impl->udev_device == dev) return impl;

    return NULL;
}

#ifdef HAS_PROPER_INPUT_HEADER

static const BYTE ABS_TO_HID_MAP[][2] = {
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_X},              /*ABS_X*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_Y},              /*ABS_Y*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_Z},              /*ABS_Z*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RX},             /*ABS_RX*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RY},             /*ABS_RY*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RZ},             /*ABS_RZ*/
    {HID_USAGE_PAGE_SIMULATION, HID_USAGE_SIMULATION_THROTTLE}, /*ABS_THROTTLE*/
    {HID_USAGE_PAGE_SIMULATION, HID_USAGE_SIMULATION_RUDDER},   /*ABS_RUDDER*/
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_WHEEL},          /*ABS_WHEEL*/
    {HID_USAGE_PAGE_SIMULATION, HID_USAGE_SIMULATION_ACCELERATOR}, /*ABS_GAS*/
    {HID_USAGE_PAGE_SIMULATION, HID_USAGE_SIMULATION_BRAKE},    /*ABS_BRAKE*/
    {0,0},
    {0,0},
    {0,0},
    {0,0},
    {0,0},
    {0,0},                                                      /*ABS_HAT0X*/
    {0,0},                                                      /*ABS_HAT0Y*/
    {0,0},                                                      /*ABS_HAT1X*/
    {0,0},                                                      /*ABS_HAT1Y*/
    {0,0},                                                      /*ABS_HAT2X*/
    {0,0},                                                      /*ABS_HAT2Y*/
    {0,0},                                                      /*ABS_HAT3X*/
    {0,0},                                                      /*ABS_HAT3Y*/
    {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_TIP_PRESSURE}, /*ABS_PRESSURE*/
    {0, 0},                                                     /*ABS_DISTANCE*/
    {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_X_TILT},     /*ABS_TILT_X*/
    {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_Y_TILT},     /*ABS_TILT_Y*/
    {0, 0},                                                     /*ABS_TOOL_WIDTH*/
    {0, 0},
    {0, 0},
    {0, 0},
    {HID_USAGE_PAGE_CONSUMER, HID_USAGE_CONSUMER_VOLUME}        /*ABS_VOLUME*/
};
C_ASSERT(ARRAY_SIZE(ABS_TO_HID_MAP) == HID_ABS_MAX);
#define TOP_ABS_PAGE (HID_USAGE_PAGE_DIGITIZER+1)

static const BYTE REL_TO_HID_MAP[][2] = {
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_X},     /* REL_X */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_Y},     /* REL_Y */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_Z},     /* REL_Z */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RX},    /* REL_RX */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RY},    /* REL_RY */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_RZ},    /* REL_RZ */
    {0, 0},                                            /* REL_HWHEEL */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_DIAL},  /* REL_DIAL */
    {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_WHEEL}, /* REL_WHEEL */
    {0, 0}                                             /* REL_MISC */
};
#define TOP_REL_PAGE (HID_USAGE_PAGE_CONSUMER+1)

#define test_bit(arr,bit) (((BYTE*)(arr))[(bit)>>3]&(1<<((bit)&7)))

static const BYTE* what_am_I(struct udev_device *dev)
{
    static const BYTE Unknown[2]     = {HID_USAGE_PAGE_GENERIC, 0};
    static const BYTE Mouse[2]       = {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_MOUSE};
    static const BYTE Keyboard[2]    = {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_KEYBOARD};
    static const BYTE Gamepad[2]     = {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_GAMEPAD};
    static const BYTE Keypad[2]      = {HID_USAGE_PAGE_GENERIC, HID_USAGE_GENERIC_KEYPAD};
    static const BYTE Tablet[2]      = {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_PEN};
    static const BYTE Touchscreen[2] = {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_TOUCH_SCREEN};
    static const BYTE Touchpad[2]    = {HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_TOUCH_PAD};

    struct udev_device *parent = dev;

    /* Look to the parents until we get a clue */
    while (parent)
    {
        if (udev_device_get_property_value(parent, "ID_INPUT_MOUSE"))
            return Mouse;
        else if (udev_device_get_property_value(parent, "ID_INPUT_KEYBOARD"))
            return Keyboard;
        else if (udev_device_get_property_value(parent, "ID_INPUT_JOYSTICK"))
            return Gamepad;
        else if (udev_device_get_property_value(parent, "ID_INPUT_KEY"))
            return Keypad;
        else if (udev_device_get_property_value(parent, "ID_INPUT_TOUCHPAD"))
            return Touchpad;
        else if (udev_device_get_property_value(parent, "ID_INPUT_TOUCHSCREEN"))
            return Touchscreen;
        else if (udev_device_get_property_value(parent, "ID_INPUT_TABLET"))
            return Tablet;

        parent = udev_device_get_parent_with_subsystem_devtype(parent, "input", NULL);
    }
    return Unknown;
}

static INT count_buttons(int device_fd, BYTE *map)
{
    int i;
    int button_count = 0;
    BYTE keybits[(KEY_MAX+7)/8];

    if (ioctl(device_fd, EVIOCGBIT(EV_KEY, sizeof(keybits)), keybits) == -1)
    {
        WARN("ioctl(EVIOCGBIT, EV_KEY) failed: %d %s\n", errno, strerror(errno));
        return FALSE;
    }

    for (i = BTN_MISC; i < KEY_MAX; i++)
    {
        if (test_bit(keybits, i))
        {
            if (map) map[i] = button_count;
            button_count++;
        }
    }
    return button_count;
}

static INT count_abs_axis(int device_fd)
{
    BYTE absbits[(ABS_MAX+7)/8];
    int abs_count = 0;
    int i;

    if (ioctl(device_fd, EVIOCGBIT(EV_ABS, sizeof(absbits)), absbits) == -1)
    {
        WARN("ioctl(EVIOCGBIT, EV_ABS) failed: %d %s\n", errno, strerror(errno));
        return 0;
    }

    for (i = 0; i < HID_ABS_MAX; i++)
        if (test_bit(absbits, i) &&
            (ABS_TO_HID_MAP[i][1] >= HID_USAGE_GENERIC_X &&
             ABS_TO_HID_MAP[i][1] <= HID_USAGE_GENERIC_WHEEL))
                abs_count++;
    return abs_count;
}

static NTSTATUS build_report_descriptor(struct unix_device *iface, struct udev_device *dev)
{
    struct input_absinfo abs_info[HID_ABS_MAX];
    BYTE absbits[(ABS_MAX+7)/8];
    BYTE relbits[(REL_MAX+7)/8];
    BYTE ffbits[(FF_MAX+7)/8];
    struct ff_effect effect;
    USAGE_AND_PAGE usage;
    INT i, button_count, abs_count, rel_count, hat_count;
    const BYTE *device_usage = what_am_I(dev);
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);

    if (ioctl(impl->base.device_fd, EVIOCGBIT(EV_REL, sizeof(relbits)), relbits) == -1)
    {
        WARN("ioctl(EVIOCGBIT, EV_REL) failed: %d %s\n", errno, strerror(errno));
        memset(relbits, 0, sizeof(relbits));
    }
    if (ioctl(impl->base.device_fd, EVIOCGBIT(EV_ABS, sizeof(absbits)), absbits) == -1)
    {
        WARN("ioctl(EVIOCGBIT, EV_ABS) failed: %d %s\n", errno, strerror(errno));
        memset(absbits, 0, sizeof(absbits));
    }
    if (ioctl(impl->base.device_fd, EVIOCGBIT(EV_FF, sizeof(ffbits)), ffbits) == -1)
    {
        WARN("ioctl(EVIOCGBIT, EV_FF) failed: %d %s\n", errno, strerror(errno));
        memset(ffbits, 0, sizeof(ffbits));
    }

    if (!hid_device_begin_report_descriptor(iface, device_usage[0], device_usage[1]))
        return STATUS_NO_MEMORY;

    if (!hid_device_begin_input_report(iface))
        return STATUS_NO_MEMORY;

    abs_count = 0;
    for (i = 0; i < HID_ABS_MAX; i++)
    {
        if (!test_bit(absbits, i)) continue;
        ioctl(impl->base.device_fd, EVIOCGABS(i), abs_info + i);

        if (!(usage.UsagePage = ABS_TO_HID_MAP[i][0])) continue;
        if (!(usage.Usage = ABS_TO_HID_MAP[i][1])) continue;

        if (!hid_device_add_axes(iface, 1, usage.UsagePage, &usage.Usage, FALSE,
                                 LE_DWORD(abs_info[i].minimum), LE_DWORD(abs_info[i].maximum)))
            return STATUS_NO_MEMORY;

        impl->abs_map[i] = abs_count++;
    }

    rel_count = 0;
    for (i = 0; i < HID_REL_MAX; i++)
    {
        if (!test_bit(relbits, i)) continue;
        if (!(usage.UsagePage = REL_TO_HID_MAP[i][0])) continue;
        if (!(usage.Usage = REL_TO_HID_MAP[i][1])) continue;

        if (!hid_device_add_axes(iface, 1, usage.UsagePage, &usage.Usage, TRUE,
                                 INT32_MIN, INT32_MAX))
            return STATUS_NO_MEMORY;

        impl->rel_map[i] = rel_count++;
    }

    hat_count = 0;
    for (i = ABS_HAT0X; i <= ABS_HAT3X; i += 2)
    {
        if (!test_bit(absbits, i)) continue;
        impl->hat_map[i - ABS_HAT0X] = hat_count;
        impl->hat_map[i - ABS_HAT0X + 1] = hat_count++;
    }

    if (hat_count && !hid_device_add_hatswitch(iface, hat_count))
        return STATUS_NO_MEMORY;

    /* For now lump all buttons just into incremental usages, Ignore Keys */
    button_count = count_buttons(impl->base.device_fd, impl->button_map);
    if (button_count && !hid_device_add_buttons(iface, HID_USAGE_PAGE_BUTTON, 1, button_count))
        return STATUS_NO_MEMORY;

    if (!hid_device_end_input_report(iface))
        return STATUS_NO_MEMORY;

    impl->haptic_effect_id = -1;
    if (test_bit(ffbits, FF_RUMBLE))
    {
        effect.id = -1;
        effect.type = FF_RUMBLE;
        effect.replay.length = 0;
        effect.u.rumble.strong_magnitude = 0;
        effect.u.rumble.weak_magnitude = 0;

        if (ioctl(impl->base.device_fd, EVIOCSFF, &effect) == -1)
            WARN("couldn't allocate rumble effect for haptics: %d %s\n", errno, strerror(errno));
        else if (!hid_device_add_haptics(iface))
            return FALSE;
        else
            impl->haptic_effect_id = effect.id;
    }

    if (!hid_device_end_report_descriptor(iface))
        return STATUS_NO_MEMORY;

    /* Initialize axis in the report */
    for (i = 0; i < HID_ABS_MAX; i++)
    {
        if (!test_bit(absbits, i)) continue;
        if (i < ABS_HAT0X || i > ABS_HAT3Y)
            hid_device_set_abs_axis(iface, impl->abs_map[i], abs_info[i].value);
        else if ((i - ABS_HAT0X) % 2)
            hid_device_set_hatswitch_y(iface, impl->hat_map[i - ABS_HAT0X], abs_info[i].value);
        else
            hid_device_set_hatswitch_x(iface, impl->hat_map[i - ABS_HAT0X], abs_info[i].value);
    }

    return STATUS_SUCCESS;
}

static BOOL set_report_from_event(struct unix_device *iface, struct input_event *ie)
{
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);

    switch (ie->type)
    {
#ifdef EV_SYN
    case EV_SYN:
        switch (ie->code)
        {
        case SYN_REPORT: return hid_device_sync_report(iface);
        case SYN_DROPPED: hid_device_drop_report(iface); break;
        }
        return FALSE;
#endif
#ifdef EV_MSC
    case EV_MSC:
        return FALSE;
#endif
    case EV_KEY:
        hid_device_set_button(iface, impl->button_map[ie->code], ie->value);
        return FALSE;
    case EV_ABS:
        if (ie->code < ABS_HAT0X || ie->code > ABS_HAT3Y)
            hid_device_set_abs_axis(iface, impl->abs_map[ie->code], ie->value);
        else if ((ie->code - ABS_HAT0X) % 2)
            hid_device_set_hatswitch_y(iface, impl->hat_map[ie->code - ABS_HAT0X], ie->value);
        else
            hid_device_set_hatswitch_x(iface, impl->hat_map[ie->code - ABS_HAT0X], ie->value);
        return FALSE;
    case EV_REL:
        hid_device_set_rel_axis(iface, impl->rel_map[ie->code], ie->value);
        return FALSE;
    default:
        ERR("TODO: Process Report (%i, %i)\n",ie->type, ie->code);
        return FALSE;
    }
}
#endif

static void hidraw_device_destroy(struct unix_device *iface)
{
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);

    udev_device_unref(impl->base.udev_device);
}

static NTSTATUS hidraw_device_start(struct unix_device *iface)
{
    pthread_mutex_lock(&udev_cs);
    start_polling_device(iface);
    pthread_mutex_unlock(&udev_cs);
    return STATUS_SUCCESS;
}

static void hidraw_device_stop(struct unix_device *iface)
{
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);

    pthread_mutex_lock(&udev_cs);
    stop_polling_device(iface);
    list_remove(&impl->base.unix_device.entry);
    pthread_mutex_unlock(&udev_cs);
}

static NTSTATUS hidraw_device_get_report_descriptor(struct unix_device *iface, BYTE *buffer,
                                                    DWORD length, DWORD *out_length)
{
#ifdef HAVE_LINUX_HIDRAW_H
    struct hidraw_report_descriptor descriptor;
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);

    if (ioctl(impl->base.device_fd, HIDIOCGRDESCSIZE, &descriptor.size) == -1)
    {
        WARN("ioctl(HIDIOCGRDESCSIZE) failed: %d %s\n", errno, strerror(errno));
        return STATUS_UNSUCCESSFUL;
    }

    *out_length = descriptor.size;

    if (length < descriptor.size)
        return STATUS_BUFFER_TOO_SMALL;
    if (!descriptor.size)
        return STATUS_SUCCESS;

    if (ioctl(impl->base.device_fd, HIDIOCGRDESC, &descriptor) == -1)
    {
        WARN("ioctl(HIDIOCGRDESC) failed: %d %s\n", errno, strerror(errno));
        return STATUS_UNSUCCESSFUL;
    }

    memcpy(buffer, descriptor.value, descriptor.size);
    return STATUS_SUCCESS;
#else
    return STATUS_NOT_IMPLEMENTED;
#endif
}

static void hidraw_device_read_report(struct unix_device *iface)
{
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);
    BYTE report_buffer[1024];

    int size = read(impl->base.device_fd, report_buffer, sizeof(report_buffer));
    if (size == -1)
        TRACE_(hid_report)("Read failed. Likely an unplugged device %d %s\n", errno, strerror(errno));
    else if (size == 0)
        TRACE_(hid_report)("Failed to read report\n");
    else
        bus_event_queue_input_report(&event_queue, iface, report_buffer, size);
}

static void hidraw_device_set_output_report(struct unix_device *iface, HID_XFER_PACKET *packet, IO_STATUS_BLOCK *io)
{
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);
    ULONG length = packet->reportBufferLen;
    BYTE buffer[8192];
    int count = 0;

    if ((buffer[0] = packet->reportId))
        count = write(impl->base.device_fd, packet->reportBuffer, length);
    else if (length > sizeof(buffer) - 1)
        ERR_(hid_report)("id %d length %u >= 8192, cannot write\n", packet->reportId, length);
    else
    {
        memcpy(buffer + 1, packet->reportBuffer, length);
        count = write(impl->base.device_fd, buffer, length + 1);
    }

    if (count > 0)
    {
        io->Information = count;
        io->Status = STATUS_SUCCESS;
    }
    else
    {
        ERR_(hid_report)("id %d write failed error: %d %s\n", packet->reportId, errno, strerror(errno));
        io->Information = 0;
        io->Status = STATUS_UNSUCCESSFUL;
    }
}

static void hidraw_device_get_feature_report(struct unix_device *iface, HID_XFER_PACKET *packet,
                                             IO_STATUS_BLOCK *io)
{
#if defined(HAVE_LINUX_HIDRAW_H) && defined(HIDIOCGFEATURE)
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);
    ULONG length = packet->reportBufferLen;
    BYTE buffer[8192];
    int count = 0;

    if ((buffer[0] = packet->reportId) && length <= 0x1fff)
        count = ioctl(impl->base.device_fd, HIDIOCGFEATURE(length), packet->reportBuffer);
    else if (length > sizeof(buffer) - 1)
        ERR_(hid_report)("id %d length %u >= 8192, cannot read\n", packet->reportId, length);
    else
    {
        count = ioctl(impl->base.device_fd, HIDIOCGFEATURE(length + 1), buffer);
        memcpy(packet->reportBuffer, buffer + 1, length);
    }

    if (count > 0)
    {
        io->Information = count;
        io->Status = STATUS_SUCCESS;
    }
    else
    {
        ERR_(hid_report)("id %d read failed, error: %d %s\n", packet->reportId, errno, strerror(errno));
        io->Information = 0;
        io->Status = STATUS_UNSUCCESSFUL;
    }
#else
    io->Information = 0;
    io->Status = STATUS_NOT_IMPLEMENTED;
#endif
}

static void hidraw_device_set_feature_report(struct unix_device *iface, HID_XFER_PACKET *packet,
                                             IO_STATUS_BLOCK *io)
{
#if defined(HAVE_LINUX_HIDRAW_H) && defined(HIDIOCSFEATURE)
    struct hidraw_device *impl = hidraw_impl_from_unix_device(iface);
    ULONG length = packet->reportBufferLen;
    BYTE buffer[8192];
    int count = 0;

    if ((buffer[0] = packet->reportId) && length <= 0x1fff)
        count = ioctl(impl->base.device_fd, HIDIOCSFEATURE(length), packet->reportBuffer);
    else if (length > sizeof(buffer) - 1)
        ERR_(hid_report)("id %d length %u >= 8192, cannot write\n", packet->reportId, length);
    else
    {
        memcpy(buffer + 1, packet->reportBuffer, length);
        count = ioctl(impl->base.device_fd, HIDIOCSFEATURE(length + 1), buffer);
    }

    if (count > 0)
    {
        io->Information = count;
        io->Status = STATUS_SUCCESS;
    }
    else
    {
        ERR_(hid_report)("id %d write failed, error: %d %s\n", packet->reportId, errno, strerror(errno));
        io->Information = 0;
        io->Status = STATUS_UNSUCCESSFUL;
    }
#else
    io->Information = 0;
    io->Status = STATUS_NOT_IMPLEMENTED;
#endif
}

static const struct raw_device_vtbl hidraw_device_vtbl =
{
    hidraw_device_destroy,
    hidraw_device_start,
    hidraw_device_stop,
    hidraw_device_get_report_descriptor,
    hidraw_device_set_output_report,
    hidraw_device_get_feature_report,
    hidraw_device_set_feature_report,
};

#ifdef HAS_PROPER_INPUT_HEADER

static void lnxev_device_destroy(struct unix_device *iface)
{
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);
    udev_device_unref(impl->base.udev_device);
}

static NTSTATUS lnxev_device_start(struct unix_device *iface)
{
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);
    NTSTATUS status;

    if ((status = build_report_descriptor(iface, impl->base.udev_device)))
        return status;

    pthread_mutex_lock(&udev_cs);
    start_polling_device(iface);
    pthread_mutex_unlock(&udev_cs);
    return STATUS_SUCCESS;
}

static void lnxev_device_stop(struct unix_device *iface)
{
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);

    pthread_mutex_lock(&udev_cs);
    stop_polling_device(iface);
    list_remove(&impl->base.unix_device.entry);
    pthread_mutex_unlock(&udev_cs);
}

static void lnxev_device_read_report(struct unix_device *iface)
{
    struct hid_device_state *state = &iface->hid_device_state;
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);
    struct input_event ie;
    int size;

    size = read(impl->base.device_fd, &ie, sizeof(ie));
    if (size == -1)
        TRACE_(hid_report)("Read failed. Likely an unplugged device\n");
    else if (size == 0)
        TRACE_(hid_report)("Failed to read report\n");
    else if (set_report_from_event(iface, &ie))
        bus_event_queue_input_report(&event_queue, iface, state->report_buf, state->report_len);
}

static NTSTATUS lnxev_device_haptics_start(struct unix_device *iface, DWORD duration_ms,
                                           USHORT rumble_intensity, USHORT buzz_intensity)
{
    struct lnxev_device *impl = lnxev_impl_from_unix_device(iface);
    struct ff_effect effect =
    {
        .id = impl->haptic_effect_id,
        .type = FF_RUMBLE,
    };
    struct input_event event;

    TRACE("iface %p, duration_ms %u, rumble_intensity %u, buzz_intensity %u stub!\n", iface,
          duration_ms, rumble_intensity, buzz_intensity);

    effect.replay.length = duration_ms;
    effect.u.rumble.strong_magnitude = rumble_intensity;
    effect.u.rumble.weak_magnitude = buzz_intensity;

    if (ioctl(impl->base.device_fd, EVIOCSFF, &effect) == -1)
    {
        effect.id = -1;
        if (ioctl(impl->base.device_fd, EVIOCSFF, &effect) == 1)
        {
            WARN("couldn't re-allocate rumble effect for haptics: %d %s\n", errno, strerror(errno));
            return STATUS_UNSUCCESSFUL;
        }
        impl->haptic_effect_id = effect.id;
    }

    event.type = EV_FF;
    event.code = effect.id;
    event.value = 1;
    if (write(impl->base.device_fd, &event, sizeof(event)) == -1)
    {
        WARN("couldn't start haptics rumble effect: %d %s\n", errno, strerror(errno));
        return STATUS_UNSUCCESSFUL;
    }

    return STATUS_SUCCESS;
}

static const struct hid_device_vtbl lnxev_device_vtbl =
{
    lnxev_device_destroy,
    lnxev_device_start,
    lnxev_device_stop,
    lnxev_device_haptics_start,
};
#endif

static void get_device_subsystem_info(struct udev_device *dev, char const *subsystem, struct device_desc *desc)
{
    struct udev_device *parent = NULL;
    const char *ptr, *next, *tmp;
    DWORD bus = 0;

    if (!(parent = udev_device_get_parent_with_subsystem_devtype(dev, subsystem, NULL))) return;

    if ((next = udev_device_get_sysattr_value(parent, "uevent")))
    {
        while ((ptr = next) && *ptr)
        {
            if ((next = strchr(next, '\n'))) next += 1;
            else next = ptr + strlen(ptr);
            TRACE("%s uevent %s\n", subsystem, debugstr_an(ptr, next - ptr - 1));

            if (!strncmp(ptr, "HID_UNIQ=", 9))
            {
                if (desc->serialnumber[0]) continue;
                sscanf(ptr, "HID_UNIQ=%256s\n", desc->serialnumber);
            }
            if (!strncmp(ptr, "HID_PHYS=", 9) || !strncmp(ptr, "PHYS=\"", 6))
            {
                if (!(tmp = strstr(ptr, "/input")) || tmp >= next) continue;
                if (desc->input == -1) sscanf(tmp, "/input%d\n", &desc->input);
            }
            if (!strncmp(ptr, "HID_ID=", 7))
            {
                if (bus || desc->vid || desc->pid) continue;
                sscanf(ptr, "HID_ID=%x:%x:%x\n", &bus, &desc->vid, &desc->pid);
            }
            if (!strncmp(ptr, "PRODUCT=", 8))
            {
                if (desc->version) continue;
                if (!strcmp(subsystem, "usb"))
                    sscanf(ptr, "PRODUCT=%x/%x/%x\n", &desc->vid, &desc->pid, &desc->version);
                else
                    sscanf(ptr, "PRODUCT=%x/%x/%x/%x\n", &bus, &desc->vid, &desc->pid, &desc->version);
            }
        }
    }

    if (!desc->manufacturer[0] && (tmp = udev_device_get_sysattr_value(dev, "manufacturer")))
        lstrcpynA(desc->manufacturer, tmp, sizeof(desc->manufacturer));

    if (!desc->product[0] && (tmp = udev_device_get_sysattr_value(dev, "product")))
        lstrcpynA(desc->product, tmp, sizeof(desc->product));

    if (!desc->serialnumber[0] && (tmp = udev_device_get_sysattr_value(dev, "serial")))
        lstrcpynA(desc->serialnumber, tmp, sizeof(desc->serialnumber));
}

static void udev_add_device(struct udev_device *dev)
{
    struct device_desc desc =
    {
        .input = -1,
    };
    struct base_device *impl;
    const char *subsystem;
    const char *devnode;
    int fd;

    if (!(devnode = udev_device_get_devnode(dev)))
        return;

    if ((fd = open(devnode, O_RDWR)) == -1)
    {
        WARN("Unable to open udev device %s: %s\n", debugstr_a(devnode), strerror(errno));
        return;
    }

    TRACE("udev %s syspath %s\n", debugstr_a(devnode), udev_device_get_syspath(dev));

#ifdef HAS_PROPER_INPUT_HEADER
    if ((impl = find_device_from_syspath(get_device_syspath(dev))))
    {
        TRACE("duplicate device found, not adding the new one\n");
        close(fd);
        return;
    }
#endif

    get_device_subsystem_info(dev, "hid", &desc);
    get_device_subsystem_info(dev, "input", &desc);
    get_device_subsystem_info(dev, "usb", &desc);

    subsystem = udev_device_get_subsystem(dev);
    if (!strcmp(subsystem, "hidraw"))
    {
        if (!desc.manufacturer[0]) strcpy(desc.manufacturer, "hidraw");

#ifdef HAVE_LINUX_HIDRAW_H
        if (!desc.product[0] && ioctl(fd, HIDIOCGRAWNAME(sizeof(desc.product) - 1), desc.product) < 0)
            desc.product[0] = 0;
#endif
    }
#ifdef HAS_PROPER_INPUT_HEADER
    else if (!strcmp(subsystem, "input"))
    {
        struct input_id device_id = {0};

        if (ioctl(fd, EVIOCGID, &device_id) < 0)
            WARN("ioctl(EVIOCGID) failed: %d %s\n", errno, strerror(errno));
        else
        {
            desc.vid = device_id.vendor;
            desc.pid = device_id.product;
            desc.version = device_id.version;
        }

        if (!desc.manufacturer[0]) strcpy(desc.manufacturer, "evdev");

        if (!desc.product[0] && ioctl(fd, EVIOCGNAME(sizeof(desc.product) - 1), desc.product) <= 0)
            desc.product[0] = 0;

        if (!desc.serialnumber[0] && ioctl(fd, EVIOCGUNIQ(sizeof(desc.serialnumber)), desc.serialnumber) < 0)
            desc.serialnumber[0] = 0;
    }
#endif

    if (!desc.serialnumber[0]) strcpy(desc.serialnumber, "0000");

    if (is_xbox_gamepad(desc.vid, desc.pid))
        desc.is_gamepad = TRUE;
#ifdef HAS_PROPER_INPUT_HEADER
    else
    {
        int axes=0, buttons=0;
        axes = count_abs_axis(fd);
        buttons = count_buttons(fd, NULL);
        desc.is_gamepad = (axes == 6 && buttons >= 14);
    }
#endif

    TRACE("dev %p, node %s, desc %s.\n", dev, debugstr_a(devnode), debugstr_device_desc(&desc));

    if (strcmp(subsystem, "hidraw") == 0)
    {
        if (!(impl = raw_device_create(&hidraw_device_vtbl, sizeof(struct hidraw_device)))) return;
        list_add_tail(&device_list, &impl->unix_device.entry);
        impl->read_report = hidraw_device_read_report;
        impl->udev_device = udev_device_ref(dev);
        impl->device_fd = fd;

        bus_event_queue_device_created(&event_queue, &impl->unix_device, &desc);
    }
#ifdef HAS_PROPER_INPUT_HEADER
    else if (strcmp(subsystem, "input") == 0)
    {
        if (!(impl = hid_device_create(&lnxev_device_vtbl, sizeof(struct lnxev_device)))) return;
        list_add_tail(&device_list, &impl->unix_device.entry);
        impl->read_report = lnxev_device_read_report;
        impl->udev_device = udev_device_ref(dev);
        impl->device_fd = fd;

        bus_event_queue_device_created(&event_queue, &impl->unix_device, &desc);
    }
#endif
}

static void build_initial_deviceset(void)
{
    struct udev_enumerate *enumerate;
    struct udev_list_entry *devices, *dev_list_entry;

    enumerate = udev_enumerate_new(udev_context);
    if (!enumerate)
    {
        WARN("Unable to create udev enumeration object\n");
        return;
    }

    if (!options.disable_hidraw)
        if (udev_enumerate_add_match_subsystem(enumerate, "hidraw") < 0)
            WARN("Failed to add subsystem 'hidraw' to enumeration\n");
#ifdef HAS_PROPER_INPUT_HEADER
    if (!options.disable_input)
    {
        if (udev_enumerate_add_match_subsystem(enumerate, "input") < 0)
            WARN("Failed to add subsystem 'input' to enumeration\n");
    }
#endif

    if (udev_enumerate_scan_devices(enumerate) < 0)
        WARN("Enumeration scan failed\n");

    devices = udev_enumerate_get_list_entry(enumerate);
    udev_list_entry_foreach(dev_list_entry, devices)
    {
        struct udev_device *dev;
        const char *path;

        path = udev_list_entry_get_name(dev_list_entry);
        if ((dev = udev_device_new_from_syspath(udev_context, path)))
        {
            udev_add_device(dev);
            udev_device_unref(dev);
        }
    }

    udev_enumerate_unref(enumerate);
}

static struct udev_monitor *create_monitor(int *fd)
{
    struct udev_monitor *monitor;
    int systems = 0;

    monitor = udev_monitor_new_from_netlink(udev_context, "udev");
    if (!monitor)
    {
        WARN("Unable to get udev monitor object\n");
        return NULL;
    }

    if (!options.disable_hidraw)
    {
        if (udev_monitor_filter_add_match_subsystem_devtype(monitor, "hidraw", NULL) < 0)
            WARN("Failed to add 'hidraw' subsystem to monitor\n");
        else
            systems++;
    }
#ifdef HAS_PROPER_INPUT_HEADER
    if (!options.disable_input)
    {
        if (udev_monitor_filter_add_match_subsystem_devtype(monitor, "input", NULL) < 0)
            WARN("Failed to add 'input' subsystem to monitor\n");
        else
            systems++;
    }
#endif
    if (systems == 0)
    {
        WARN("No subsystems added to monitor\n");
        goto error;
    }

    if (udev_monitor_enable_receiving(monitor) < 0)
        goto error;

    if ((*fd = udev_monitor_get_fd(monitor)) >= 0)
        return monitor;

error:
    WARN("Failed to start monitoring\n");
    udev_monitor_unref(monitor);
    return NULL;
}

static void process_monitor_event(struct udev_monitor *monitor)
{
    struct base_device *impl;
    struct udev_device *dev;
    const char *action;

    dev = udev_monitor_receive_device(monitor);
    if (!dev)
    {
        FIXME("Failed to get device that has changed\n");
        return;
    }

    action = udev_device_get_action(dev);
    TRACE("Received action %s for udev device %s\n", debugstr_a(action),
          debugstr_a(udev_device_get_devnode(dev)));

    if (!action)
        WARN("No action received\n");
    else if (strcmp(action, "add") == 0)
        udev_add_device(dev);
    else if (strcmp(action, "remove") == 0)
    {
        impl = find_device_from_udev(dev);
        if (impl) bus_event_queue_device_removed(&event_queue, &impl->unix_device);
        else WARN("failed to find device for udev device %p\n", dev);
    }
    else
        WARN("Unhandled action %s\n", debugstr_a(action));

    udev_device_unref(dev);
}

NTSTATUS udev_bus_init(void *args)
{
    int monitor_fd;

    TRACE("args %p\n", args);

    options = *(struct udev_bus_options *)args;

    if (pipe(deviceloop_control) != 0)
    {
        ERR("UDEV control pipe creation failed\n");
        return STATUS_UNSUCCESSFUL;
    }

    if (!(udev_context = udev_new()))
    {
        ERR("UDEV object creation failed\n");
        goto error;
    }

    if (!(udev_monitor = create_monitor(&monitor_fd)))
    {
        ERR("UDEV monitor creation failed\n");
        goto error;
    }

    poll_fds[0].fd = monitor_fd;
    poll_fds[0].events = POLLIN;
    poll_fds[0].revents = 0;
    poll_fds[1].fd = deviceloop_control[0];
    poll_fds[1].events = POLLIN;
    poll_fds[1].revents = 0;
    poll_count = 2;

    build_initial_deviceset();
    return STATUS_SUCCESS;

error:
    if (udev_context) udev_unref(udev_context);
    udev_context = NULL;
    close(deviceloop_control[0]);
    close(deviceloop_control[1]);
    return STATUS_UNSUCCESSFUL;
}

NTSTATUS udev_bus_wait(void *args)
{
    struct bus_event *result = args;
    struct pollfd pfd[MAX_DEVICES];
    struct base_device *impl;
    char ctrl = 0;
    int i, count;

    /* cleanup previously returned event */
    bus_event_cleanup(result);

    while (ctrl != 'q')
    {
        if (bus_event_queue_pop(&event_queue, result)) return STATUS_PENDING;

        pthread_mutex_lock(&udev_cs);
        while (close_count--) close(close_fds[close_count]);
        memcpy(pfd, poll_fds, poll_count * sizeof(*pfd));
        count = poll_count;
        close_count = 0;
        pthread_mutex_unlock(&udev_cs);

        while (poll(pfd, count, -1) <= 0) {}

        pthread_mutex_lock(&udev_cs);
        if (pfd[0].revents) process_monitor_event(udev_monitor);
        if (pfd[1].revents) read(deviceloop_control[0], &ctrl, 1);
        for (i = 2; i < count; ++i)
        {
            if (!pfd[i].revents) continue;
            impl = find_device_from_fd(pfd[i].fd);
            if (impl) impl->read_report(&impl->unix_device);
        }
        pthread_mutex_unlock(&udev_cs);
    }

    TRACE("UDEV main loop exiting\n");
    bus_event_queue_destroy(&event_queue);
    udev_monitor_unref(udev_monitor);
    udev_unref(udev_context);
    udev_context = NULL;
    close(deviceloop_control[0]);
    close(deviceloop_control[1]);
    return STATUS_SUCCESS;
}

NTSTATUS udev_bus_stop(void *args)
{
    if (!udev_context) return STATUS_SUCCESS;
    write(deviceloop_control[1], "q", 1);
    return STATUS_SUCCESS;
}

#else

NTSTATUS udev_bus_init(void *args)
{
    WARN("UDEV support not compiled in!\n");
    return STATUS_NOT_IMPLEMENTED;
}

NTSTATUS udev_bus_wait(void *args)
{
    WARN("UDEV support not compiled in!\n");
    return STATUS_NOT_IMPLEMENTED;
}

NTSTATUS udev_bus_stop(void *args)
{
    WARN("UDEV support not compiled in!\n");
    return STATUS_NOT_IMPLEMENTED;
}

#endif /* HAVE_UDEV */
