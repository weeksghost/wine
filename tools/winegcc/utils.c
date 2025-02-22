/*
 * Useful functions for winegcc
 *
 * Copyright 2000 Francois Gouget
 * Copyright 2002 Dimitrie O. Paun
 * Copyright 2003 Richard Cohen
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
#include "config.h"
#include "wine/port.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>

#include "utils.h"

#if !defined(min)
# define min(x,y) (((x) < (y)) ? (x) : (y))
#endif

int verbose = 0;

void error(const char* s, ...)
{
    va_list ap;
    
    va_start(ap, s);
    fprintf(stderr, "winegcc: ");
    vfprintf(stderr, s, ap);
    va_end(ap);
    exit(2);
}

void* xmalloc(size_t size)
{
    void* p;

    if ((p = malloc (size)) == NULL)
	error("Could not malloc %d bytes\n", size);

    return p;
}

void *xrealloc(void* p, size_t size)
{
    void* p2 = realloc (p, size);
    if (size && !p2)
	error("Could not realloc %d bytes\n", size);

    return p2;
}

char *xstrdup( const char *str )
{
    char *res = strdup( str );
    if (!res) error("Virtual memory exhausted.\n");
    return res;
}

int strendswith(const char* str, const char* end)
{
    int l = strlen(str);
    int m = strlen(end);

    return l >= m && strcmp(str + l - m, end) == 0;
}

char* strmake(const char* fmt, ...)
{
    int n;
    size_t size = 100;
    va_list ap;

    while (1)
    {
        char *p = xmalloc (size);
        va_start(ap, fmt);
	n = vsnprintf (p, size, fmt, ap);
	va_end(ap);
        if (n == -1) size *= 2;
        else if ((size_t)n >= size) size = n + 1;
        else return p;
        free(p);
    }
}

void strarray_add( struct strarray *array, const char *str )
{
    if (array->count == array->size)
    {
	if (array->size) array->size *= 2;
        else array->size = 16;
	array->str = xrealloc( array->str, sizeof(array->str[0]) * array->size );
    }
    array->str[array->count++] = str;
}

void strarray_addall( struct strarray *array, struct strarray added )
{
    unsigned int i;

    for (i = 0; i < added.count; i++) strarray_add( array, added.str[i] );
}

struct strarray strarray_fromstring( const char *str, const char *delim )
{
    struct strarray array = empty_strarray;
    char *buf = xstrdup( str );
    const char *tok;

    for (tok = strtok( buf, delim ); tok; tok = strtok( NULL, delim ))
        strarray_add( &array, xstrdup( tok ));
    free( buf );
    return array;
}

static struct strarray strarray_frompath( const char *path )
{
    if (!path) return empty_strarray;
#if defined(_WIN32) && !defined(__CYGWIN__)
    return strarray_fromstring( path, ";" );
#else
    return strarray_fromstring( path, ":" );
#endif
}

char *strarray_tostring( struct strarray array, const char *sep )
{
    char *str;
    unsigned int i, len = 1 + (array.count - 1) * strlen(sep);

    if (!array.count) return xstrdup("");
    for (i = 0; i < array.count; i++) len += strlen( array.str[i] );
    str = xmalloc( len );
    strcpy( str, array.str[0] );
    for (i = 1; i < array.count; i++)
    {
        strcat( str, sep );
        strcat( str, array.str[i] );
    }
    return str;
}

char* get_basename(const char* file)
{
    const char* name;
    char *base_name, *p;

    if ((name = strrchr(file, '/'))) name++;
    else name = file;

    base_name = strdup(name);
    if ((p = strrchr(base_name, '.'))) *p = 0;

    return base_name;
}

void create_file(const char* name, int mode, const char* fmt, ...)
{
    va_list ap;
    FILE *file;

    if (verbose) printf("Creating file %s\n", name);
    va_start(ap, fmt);
    if ( !(file = fopen(name, "w")) )
	error("Unable to open %s for writing\n", name);
    vfprintf(file, fmt, ap);
    va_end(ap);
    fclose(file);
    chmod(name, mode);
}

file_type get_file_type(const char* filename)
{
    /* see tools/winebuild/res32.c: check_header for details */
    static const char res_sig[] = { 0,0,0,0, 32,0,0,0, 0xff,0xff, 0,0, 0xff,0xff, 0,0, 0,0,0,0, 0,0, 0,0, 0,0,0,0, 0,0,0,0 };
    static const char elf_sig[4] = "\177ELF";
    char buf[sizeof(res_sig)];
    int fd, cnt;

    fd = open( filename, O_RDONLY );
    if (fd == -1) return file_na;
    cnt = read(fd, buf, sizeof(buf));
    close( fd );
    if (cnt == -1) return file_na;

    if (cnt == sizeof(res_sig) && !memcmp(buf, res_sig, sizeof(res_sig))) return file_res;
    if (strendswith(filename, ".o")) return file_obj;
    if (strendswith(filename, ".obj")) return file_obj;
    if (strendswith(filename, ".a")) return file_arh;
    if (strendswith(filename, ".res")) return file_res;
    if (strendswith(filename, ".so")) return file_so;
    if (strendswith(filename, ".dylib")) return file_so;
    if (strendswith(filename, ".def")) return file_def;
    if (strendswith(filename, ".spec")) return file_spec;
    if (strendswith(filename, ".rc")) return file_rc;
    if (cnt >= sizeof(elf_sig) && !memcmp(buf, elf_sig, sizeof(elf_sig))) return file_so;  /* ELF lib */
    if (cnt >= sizeof(unsigned int) &&
        (*(unsigned int *)buf == 0xfeedface || *(unsigned int *)buf == 0xcefaedfe ||
         *(unsigned int *)buf == 0xfeedfacf || *(unsigned int *)buf == 0xcffaedfe))
        return file_so; /* Mach-O lib */

    return file_other;
}

static char* try_lib_path(const char* dir, const char* pre, 
			  const char* library, const char* ext,
			  file_type expected_type)
{
    char *fullname;
    file_type type;

    /* first try a subdir named from the library we are looking for */
    fullname = strmake("%s/%s/%s%s%s", dir, library, pre, library, ext);
    if (verbose > 1) fprintf(stderr, "Try %s...", fullname);
    type = get_file_type(fullname);
    if (verbose > 1) fprintf(stderr, type == expected_type ? "FOUND!\n" : "no\n");
    if (type == expected_type) return fullname;
    free( fullname );

    fullname = strmake("%s/%s%s%s", dir, pre, library, ext);
    if (verbose > 1) fprintf(stderr, "Try %s...", fullname);
    type = get_file_type(fullname);
    if (verbose > 1) fprintf(stderr, type == expected_type ? "FOUND!\n" : "no\n");
    if (type == expected_type) return fullname;
    free( fullname );
    return 0; 
}

static file_type guess_lib_type(enum target_platform platform, const char* dir,
                                const char* library, const char *prefix, const char *suffix, char** file)
{
    if (platform != PLATFORM_WINDOWS && platform != PLATFORM_MINGW && platform != PLATFORM_CYGWIN)
    {
        /* Unix shared object */
        if ((*file = try_lib_path(dir, prefix, library, ".so", file_so)))
            return file_so;

        /* Mach-O (Darwin/Mac OS X) Dynamic Library behaves mostly like .so */
        if ((*file = try_lib_path(dir, prefix, library, ".dylib", file_so)))
            return file_so;

        /* Windows DLL */
        if ((*file = try_lib_path(dir, prefix, library, ".def", file_def)))
            return file_dll;
    }

    /* static archives */
    if ((*file = try_lib_path(dir, prefix, library, suffix, file_arh)))
	return file_arh;

    return file_na;
}

file_type get_lib_type(enum target_platform platform, struct strarray path, const char *library,
                       const char *prefix, const char *suffix, char** file)
{
    unsigned int i;

    if (!suffix) suffix = ".a";
    for (i = 0; i < path.count; i++)
    {
        file_type type = guess_lib_type(platform, path.str[i], library, prefix, suffix, file);
	if (type != file_na) return type;
    }
    return file_na;
}

const char *find_binary( struct strarray prefix, const char *name )
{
    char *file_name, *args;
    struct strarray dirs = empty_strarray;
    static struct strarray path;
    unsigned int i;

    if (strchr( name, '/' )) return name;

    file_name = xstrdup( name );
    if ((args = strchr( file_name, ' ' ))) *args++ = 0;

    if (!path.count) strarray_addall( &path, strarray_frompath( getenv( "PATH" )));

    strarray_addall( &dirs, prefix );
    strarray_addall( &dirs, path );
    for (i = 0; i < dirs.count; i++)
    {
        struct stat st;
        char *prog = strmake( "%s/%s%s", dirs.str[i], file_name, EXEEXT );
        if (stat( prog, &st ) == 0 && S_ISREG( st.st_mode ) && (st.st_mode & 0111))
            return args ? strmake( "%s %s", prog, args ) : prog;
        free( prog );
    }
    return NULL;
}

int spawn(struct strarray prefix, struct strarray args, int ignore_errors)
{
    unsigned int i;
    int status;
    const char** argv;

    strarray_add( &args, NULL);
    argv = args.str;
    argv[0] = find_binary( prefix, argv[0] );

    if (verbose)
    {
	for(i = 0; argv[i]; i++)
	{
	    if (strpbrk(argv[i], " \t\n\r")) printf("\"%s\" ", argv[i]);
	    else printf("%s ", argv[i]);
	}
	printf("\n");
    }

    if ((status = _spawnvp( _P_WAIT, argv[0], argv)) && !ignore_errors)
    {
	if (status > 0) error("%s failed\n", argv[0]);
	else perror("winegcc");
	exit(3);
    }

    return status;
}
