/*** Autogenerated by WIDL 0.1 from wtypes.idl - Do not edit ***/
#include <rpc.h>
#include <rpcndr.h>

#ifndef __WIDL_WTYPES_H
#define __WIDL_WTYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <basetsd.h>
#include <guiddef.h>
#if 0 /* winnt.h */
typedef unsigned char BYTE;

typedef unsigned short WORD;

typedef unsigned long DWORD;

typedef long BOOL;

typedef unsigned char UCHAR;

typedef int INT;

typedef unsigned int UINT;

typedef short SHORT;

typedef unsigned short USHORT;

typedef long LONG;

typedef unsigned long ULONG;

typedef float FLOAT;

typedef void *PVOID, *LPVOID;

typedef char CHAR;

typedef CHAR *LPSTR;

typedef const CHAR *LPCSTR;

typedef wchar_t WCHAR;

typedef WCHAR *LPWSTR;

typedef const WCHAR *LPCWSTR;

typedef boolean BOOLEAN;

typedef DWORD COLORREF;

typedef void *HANDLE;

typedef void *HACCEL;

typedef void *HDC;

typedef void *HFONT;

typedef void *HWND;

typedef void *HMENU;

typedef void *HMODULE;

typedef void *HINSTANCE;

typedef void *HRGN;

typedef void *HTASK;

typedef void *HKEY;

typedef void *HICON;

typedef LONG_PTR LRESULT;

typedef LONG HRESULT;

typedef DWORD LCID;

typedef unsigned __int64 DWORDLONG;

typedef __int64 LONGLONG;

typedef unsigned __int64 ULONGLONG;

typedef struct _LARGE_INTEGER {
    LONGLONG QuadPart;
} LARGE_INTEGER;

typedef struct _ULARGE_INTEGER {
    ULONGLONG QuadPart;
} ULARGE_INTEGER;

typedef struct _SID_IDENTIFIER_AUTHORITY {
    UCHAR Value[6];
} SID_IDENTIFIER_AUTHORITY, *PSID_IDENTIFIER_AUTHORITY;

typedef struct _SID {
    UCHAR Revision;
    UCHAR SubAuthorityCount;
    SID_IDENTIFIER_AUTHORITY IdentifierAuthority;
    ULONG SubAuthority[1];
} SID, *PSID;

typedef USHORT SECURITY_DESCRIPTOR_CONTROL, *PSECURITY_DESCRIPTOR_CONTROL;

typedef struct _ACL {
    UCHAR AclRevision;
    UCHAR Sbz1;
    USHORT AclSize;
    USHORT AceCount;
    USHORT Sbz2;
} ACL;

typedef ACL *PACL;

typedef struct _SECURITY_DESCRIPTOR {
    UCHAR Revision;
    UCHAR Sbz1;
    SECURITY_DESCRIPTOR_CONTROL Control;
    PSID Owner;
    PSID Group;
    PACL Sacl;
    PACL Dacl;
} SECURITY_DESCRIPTOR, *PSECURITY_DESCRIPTOR;

typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

typedef struct tagSIZE {
    LONG cx;
    LONG cy;
} SIZE, *PSIZE, *LPSIZE;

typedef SIZE SIZEL, *PSIZEL, *LPSIZEL;

typedef struct tagPOINT {
    LONG x;
    LONG y;
} POINT, *PPOINT, *LPPOINT;

typedef struct _POINTL {
    LONG x;
    LONG y;
} POINTL;

typedef struct tagRECT {
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT, *PRECT, *LPRECT;

typedef const RECT *LPCRECT;

typedef struct _RECTL {
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECTL, *PRECTL, *LPRECTL;

typedef const RECTL *LPCRECTL;

typedef UINT_PTR WPARAM;

typedef LONG_PTR LPARAM;

typedef struct tagMSG {
    HWND hwnd;
    UINT message;
    WPARAM wParam;
    LPARAM lParam;
    DWORD time;
    POINT pt;
} MSG, *PMSG, *NPMSG, *LPMSG;

#endif /* winnt.h */
#if 0
typedef double DOUBLE;

#else
typedef double DECLSPEC_ALIGN(8) DOUBLE;
#endif
#ifndef _PALETTEENTRY_DEFINED
#define _PALETTEENTRY_DEFINED
typedef struct tagPALETTEENTRY {
    BYTE peRed;
    BYTE peGreen;
    BYTE peBlue;
    BYTE peFlags;
} PALETTEENTRY, *PPALETTEENTRY, *LPPALETTEENTRY;

#endif
#ifndef _LOGPALETTE_DEFINED
#define _LOGPALETTE_DEFINED
typedef struct tagLOGPALETTE {
    WORD palVersion;
    WORD palNumEntries;
    PALETTEENTRY palPalEntry[1];
} LOGPALETTE, *PLOGPALETTE, *LPLOGPALETTE;

#endif
#ifndef _FILETIME_
#define _FILETIME_
typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME, *PFILETIME, *LPFILETIME;

#endif
#ifndef _TEXTMETRIC_DEFINED
#define _TEXTMETRIC_DEFINED
typedef struct {
    LONG tmHeight;
    LONG tmAscent;
    LONG tmDescent;
    LONG tmInternalLeading;
    LONG tmExternalLeading;
    LONG tmAveCharWidth;
    LONG tmMaxCharWidth;
    LONG tmWeight;
    LONG tmOverhang;
    LONG tmDigitizedAspectX;
    LONG tmDigitizedAspectY;
    BYTE tmFirstChar;
    BYTE tmLastChar;
    BYTE tmDefaultChar;
    BYTE tmBreakChar;
    BYTE tmItalic;
    BYTE tmUnderlined;
    BYTE tmStruckOut;
    BYTE tmPitchAndFamily;
    BYTE tmCharSet;
} TEXTMETRICA, *LPTEXTMETRICA, *PTEXTMETRICA;

typedef struct {
    LONG tmHeight;
    LONG tmAscent;
    LONG tmDescent;
    LONG tmInternalLeading;
    LONG tmExternalLeading;
    LONG tmAveCharWidth;
    LONG tmMaxCharWidth;
    LONG tmWeight;
    LONG tmOverhang;
    LONG tmDigitizedAspectX;
    LONG tmDigitizedAspectY;
    WCHAR tmFirstChar;
    WCHAR tmLastChar;
    WCHAR tmDefaultChar;
    WCHAR tmBreakChar;
    BYTE tmItalic;
    BYTE tmUnderlined;
    BYTE tmStruckOut;
    BYTE tmPitchAndFamily;
    BYTE tmCharSet;
} TEXTMETRICW, *LPTEXTMETRICW, *PTEXTMETRICW;

#endif
typedef WCHAR OLECHAR;

typedef OLECHAR *LPOLESTR;

typedef const OLECHAR *LPCOLESTR;

#ifndef __WINESRC__
#define OLESTR(str) L##str
#endif
typedef LONG SCODE;

typedef struct _COAUTHIDENTITY {
    USHORT *User;
    ULONG UserLength;
    USHORT *Domain;
    ULONG DomainLength;
    USHORT *Password;
    ULONG PasswordLength;
    ULONG Flags;
} COAUTHIDENTITY;

typedef struct _COAUTHINFO {
    DWORD dwAuthnSvc;
    DWORD dwAuthzSvc;
    LPWSTR pwszServerPrincName;
    DWORD dwAuthnLevel;
    DWORD dwImpersonationLevel;
    COAUTHIDENTITY *pAuthIdentityData;
    DWORD dwCapabilities;
} COAUTHINFO;

typedef struct _COSERVERINFO {
    DWORD dwReserved1;
    LPWSTR pwszName;
    COAUTHINFO *pAuthInfo;
    DWORD dwReserved2;
} COSERVERINFO;

typedef enum tagMEMCTX {
    MEMCTX_TASK = 1,
    MEMCTX_SHARED = 2,
    MEMCTX_MACSYSTEM = 3,
    MEMCTX_UNKNOWN = -1,
    MEMCTX_SAME = -2
} MEMCTX;

#ifndef _ROTFLAGS_DEFINED
#define _ROTFLAGS_DEFINED
#define ROTFLAGS_REGISTRATIONKEEPSALIVE 0x1
#define ROTFLAGS_ALLOWANYCLIENT 0x2
#endif
typedef enum tagCLSCTX {
    CLSCTX_INPROC_SERVER = 0x1,
    CLSCTX_INPROC_HANDLER = 0x2,
    CLSCTX_LOCAL_SERVER = 0x4,
    CLSCTX_INPROC_SERVER16 = 0x8,
    CLSCTX_REMOTE_SERVER = 0x10,
    CLSCTX_INPROC_HANDLER16 = 0x20,
    CLSCTX_INPROC_SERVERX86 = 0x40,
    CLSCTX_INPROC_HANDLERX86 = 0x80,
    CLSCTX_ESERVER_HANDLER = 0x100,
    CLSCTX_NO_CODE_DOWNLOAD = 0x400,
    CLSCTX_NO_CUSTOM_MARSHAL = 0x1000,
    CLSCTX_ENABLE_CODE_DOWNLOAD = 0x2000,
    CLSCTX_NO_FAILURE_LOG = 0x4000,
    CLSCTX_DISABLE_AAA = 0x8000,
    CLSCTX_ENABLE_AAA = 0x10000,
    CLSCTX_FROM_DEFAULT_CONTEXT = 0x20000
} CLSCTX;

#define CLSCTX_INPROC (CLSCTX_INPROC_SERVER | CLSCTX_INPROC_HANDLER)
#define CLSCTX_ALL (CLSCTX_INPROC_SERVER | CLSCTX_INPROC_HANDLER | CLSCTX_LOCAL_SERVER | CLSCTX_REMOTE_SERVER)
#define CLSCTX_SERVER (CLSCTX_INPROC_SERVER | CLSCTX_LOCAL_SERVER | CLSCTX_REMOTE_SERVER)
typedef enum tagMSHLFLAGS {
    MSHLFLAGS_NORMAL = 0,
    MSHLFLAGS_TABLESTRONG = 1,
    MSHLFLAGS_TABLEWEAK = 2,
    MSHLFLAGS_NOPING = 4
} MSHLFLAGS;

typedef enum tagMSHCTX {
    MSHCTX_LOCAL = 0,
    MSHCTX_NOSHAREDMEM = 1,
    MSHCTX_DIFFERENTMACHINE = 2,
    MSHCTX_INPROC = 3,
    MSHCTX_CROSSCTX = 4
} MSHCTX;

typedef struct _BYTE_BLOB {
    unsigned long clSize;
    byte abData[1];
} BYTE_BLOB;

typedef BYTE_BLOB *UP_BYTE_BLOB;

typedef struct _FLAGGED_BYTE_BLOB {
    unsigned long fFlags;
    unsigned long clSize;
    byte abData[1];
} FLAGGED_BYTE_BLOB;

typedef FLAGGED_BYTE_BLOB *UP_FLAGGED_BYTE_BLOB;

typedef struct _FLAGGED_WORD_BLOB {
    unsigned long fFlags;
    unsigned long clSize;
    unsigned short asData[1];
} FLAGGED_WORD_BLOB;

typedef FLAGGED_WORD_BLOB *UP_FLAGGED_WORD_BLOB;

typedef struct _BYTE_SIZEDARR {
    unsigned long clSize;
    byte *pData;
} BYTE_SIZEDARR;

typedef struct _SHORT_SIZEDARR {
    unsigned long clSize;
    unsigned short *pData;
} WORD_SIZEDARR;

typedef struct _LONG_SIZEDARR {
    unsigned long clSize;
    unsigned long *pData;
} DWORD_SIZEDARR;

typedef struct _HYPER_SIZEDARR {
    unsigned long clSize;
    hyper *pData;
} HYPER_SIZEDARR;

#define WDT_INPROC_CALL (0x48746457)

#define WDT_REMOTE_CALL (0x52746457)

typedef struct _userCLIPFORMAT {
    long fContext;
    union {
        DWORD dwValue;
        LPWSTR pwszName;
    } u;
} userCLIPFORMAT;

typedef userCLIPFORMAT *wireCLIPFORMAT;

typedef WORD CLIPFORMAT;
unsigned long   __RPC_USER CLIPFORMAT_UserSize     (unsigned long *, unsigned long,   CLIPFORMAT *);
unsigned char * __RPC_USER CLIPFORMAT_UserMarshal  (unsigned long *, unsigned char *, CLIPFORMAT *);
unsigned char * __RPC_USER CLIPFORMAT_UserUnmarshal(unsigned long *, unsigned char *, CLIPFORMAT *);
void            __RPC_USER CLIPFORMAT_UserFree     (unsigned long *, CLIPFORMAT *);

typedef struct tagRemHGLOBAL {
    long fNullHGlobal;
    unsigned long cbData;
    byte data[1];
} RemHGLOBAL;

typedef struct _userHGLOBAL {
    long fContext;
    union {
        long hInproc;
        FLAGGED_BYTE_BLOB *hRemote;
        long hGlobal;
    } u;
} userHGLOBAL;

typedef userHGLOBAL *wireHGLOBAL;

typedef struct tagRemHMETAFILEPICT {
    long mm;
    long xExt;
    long yExt;
    unsigned long cbData;
    byte data[1];
} RemHMETAFILEPICT;

typedef struct _userHMETAFILE {
    long fContext;
    union {
        long hInproc;
        BYTE_BLOB *hRemote;
        long hGlobal;
    } u;
} userHMETAFILE;

typedef userHMETAFILE *wireHMETAFILE;

typedef struct _remoteMETAFILEPICT {
    long mm;
    long xExt;
    long yExt;
    userHMETAFILE *hMF;
} remoteMETAFILEPICT;

typedef struct _userHMETAFILEPICT {
    long fContext;
    union {
        long hInproc;
        remoteMETAFILEPICT *hRemote;
        long hGlobal;
    } u;
} userHMETAFILEPICT;

typedef userHMETAFILEPICT *wireHMETAFILEPICT;

typedef struct tagRemHENHMETAFILE {
    unsigned long cbData;
    byte data[1];
} RemHENHMETAFILE;

typedef struct _userHENHMETAFILE {
    long fContext;
    union {
        long hInproc;
        BYTE_BLOB *hRemote;
        long hGlobal;
    } u;
} userHENHMETAFILE;

typedef userHENHMETAFILE *wireHENHMETAFILE;

typedef struct tagRemHBITMAP {
    unsigned long cbData;
    byte data[1];
} RemHBITMAP;

typedef struct _userBITMAP {
    LONG bmType;
    LONG bmWidth;
    LONG bmHeight;
    LONG bmWidthBytes;
    WORD bmPlanes;
    WORD bmBitsPixel;
    ULONG cbSize;
    byte pBuffer[1];
} userBITMAP;

typedef struct _userHBITMAP {
    long fContext;
    union {
        long hInproc;
        userBITMAP *hRemote;
        long hGlobal;
    } u;
} userHBITMAP;

typedef userHBITMAP *wireHBITMAP;

typedef struct tagRemHPALETTE {
    unsigned long cbData;
    byte data[1];
} RemHPALETTE;

typedef struct tagrpcLOGPALETTE {
    WORD palVersion;
    WORD palNumEntries;
    PALETTEENTRY palPalEntry[1];
} rpcLOGPALETTE;

typedef struct _userHPALETTE {
    long fContext;
    union {
        long hInproc;
        rpcLOGPALETTE *hRemote;
        long hGlobal;
    } u;
} userHPALETTE;

typedef userHPALETTE *wireHPALETTE;

#if 0
typedef void *HGLOBAL;
unsigned long   __RPC_USER HGLOBAL_UserSize     (unsigned long *, unsigned long,   HGLOBAL *);
unsigned char * __RPC_USER HGLOBAL_UserMarshal  (unsigned long *, unsigned char *, HGLOBAL *);
unsigned char * __RPC_USER HGLOBAL_UserUnmarshal(unsigned long *, unsigned char *, HGLOBAL *);
void            __RPC_USER HGLOBAL_UserFree     (unsigned long *, HGLOBAL *);

typedef HGLOBAL HLOCAL;

typedef void *HBITMAP;
unsigned long   __RPC_USER HBITMAP_UserSize     (unsigned long *, unsigned long,   HBITMAP *);
unsigned char * __RPC_USER HBITMAP_UserMarshal  (unsigned long *, unsigned char *, HBITMAP *);
unsigned char * __RPC_USER HBITMAP_UserUnmarshal(unsigned long *, unsigned char *, HBITMAP *);
void            __RPC_USER HBITMAP_UserFree     (unsigned long *, HBITMAP *);

typedef void *HPALETTE;
unsigned long   __RPC_USER HPALETTE_UserSize     (unsigned long *, unsigned long,   HPALETTE *);
unsigned char * __RPC_USER HPALETTE_UserMarshal  (unsigned long *, unsigned char *, HPALETTE *);
unsigned char * __RPC_USER HPALETTE_UserUnmarshal(unsigned long *, unsigned char *, HPALETTE *);
void            __RPC_USER HPALETTE_UserFree     (unsigned long *, HPALETTE *);

typedef void *HENHMETAFILE;
unsigned long   __RPC_USER HENHMETAFILE_UserSize     (unsigned long *, unsigned long,   HENHMETAFILE *);
unsigned char * __RPC_USER HENHMETAFILE_UserMarshal  (unsigned long *, unsigned char *, HENHMETAFILE *);
unsigned char * __RPC_USER HENHMETAFILE_UserUnmarshal(unsigned long *, unsigned char *, HENHMETAFILE *);
void            __RPC_USER HENHMETAFILE_UserFree     (unsigned long *, HENHMETAFILE *);

typedef void *HMETAFILE;
unsigned long   __RPC_USER HMETAFILE_UserSize     (unsigned long *, unsigned long,   HMETAFILE *);
unsigned char * __RPC_USER HMETAFILE_UserMarshal  (unsigned long *, unsigned char *, HMETAFILE *);
unsigned char * __RPC_USER HMETAFILE_UserUnmarshal(unsigned long *, unsigned char *, HMETAFILE *);
void            __RPC_USER HMETAFILE_UserFree     (unsigned long *, HMETAFILE *);

#endif
typedef void *HMETAFILEPICT;
unsigned long   __RPC_USER HMETAFILEPICT_UserSize     (unsigned long *, unsigned long,   HMETAFILEPICT *);
unsigned char * __RPC_USER HMETAFILEPICT_UserMarshal  (unsigned long *, unsigned char *, HMETAFILEPICT *);
unsigned char * __RPC_USER HMETAFILEPICT_UserUnmarshal(unsigned long *, unsigned char *, HMETAFILEPICT *);
void            __RPC_USER HMETAFILEPICT_UserFree     (unsigned long *, HMETAFILEPICT *);

#if 0
typedef GUID *REFGUID;

typedef IID *REFIID;

typedef CLSID *REFCLSID;

typedef FMTID *REFFMTID;

#endif
typedef enum tagDVASPECT {
    DVASPECT_CONTENT = 1,
    DVASPECT_THUMBNAIL = 2,
    DVASPECT_ICON = 4,
    DVASPECT_DOCPRINT = 8
} DVASPECT;

typedef enum tagSTGC {
    STGC_DEFAULT = 0,
    STGC_OVERWRITE = 1,
    STGC_ONLYIFCURRENT = 2,
    STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4,
    STGC_CONSOLIDATE = 8
} STGC;

typedef enum tagSTGMOVE {
    STGMOVE_MOVE = 0,
    STGMOVE_COPY = 1,
    STGMOVE_SHALLOWCOPY = 2
} STGMOVE;

typedef enum tagSTATFLAG {
    STATFLAG_DEFAULT = 0,
    STATFLAG_NONAME = 1,
    STATFLAG_NOOPEN = 2
} STATFLAG;

#if 0
typedef double DATE;

#else
typedef double DECLSPEC_ALIGN(8) DATE;
#endif
#if 0
typedef struct tagCY {
    LONGLONG int64;
} CY;

#else
#ifndef _tagCY_DEFINED
#define _tagCY_DEFINED
typedef union tagCY {
    struct {
#ifdef WORDS_BIGENDIAN
        LONG  Hi;
        ULONG Lo;
#else
        ULONG Lo;
        LONG  Hi;
#endif
    } DUMMYSTRUCTNAME;
    LONGLONG int64;
} CY;
#endif
#endif
typedef CY *LPCY;

#if 0
typedef struct tagDEC {
    USHORT wReserved;
    BYTE scale;
    BYTE sign;
    ULONG Hi32;
    ULONGLONG Lo64;
} DECIMAL;

#else
typedef struct tagDEC {
  USHORT wReserved;
  union {
    struct {
      BYTE scale;
      BYTE sign;
    } DUMMYSTRUCTNAME;
    USHORT signscale;
  } DUMMYUNIONNAME;
  ULONG Hi32;
  union {
    struct {
#ifdef WORDS_BIGENDIAN
      ULONG Mid32;
      ULONG Lo32;
#else
      ULONG Lo32;
      ULONG Mid32;
#endif
    } DUMMYSTRUCTNAME1;
    ULONGLONG Lo64;
  } DUMMYUNIONNAME1;
} DECIMAL;
#endif
#define DECIMAL_NEG ((BYTE)0x80)
#define DECIMAL_SETZERO(d) do{ memset(((char*)(d)) + sizeof(USHORT), 0, sizeof(ULONG) * 3u + sizeof(USHORT)); }while (0)
typedef DECIMAL *LPDECIMAL;

typedef FLAGGED_WORD_BLOB *wireBSTR;

typedef OLECHAR *BSTR;
unsigned long   __RPC_USER BSTR_UserSize     (unsigned long *, unsigned long,   BSTR *);
unsigned char * __RPC_USER BSTR_UserMarshal  (unsigned long *, unsigned char *, BSTR *);
unsigned char * __RPC_USER BSTR_UserUnmarshal(unsigned long *, unsigned char *, BSTR *);
void            __RPC_USER BSTR_UserFree     (unsigned long *, BSTR *);

typedef BSTR *LPBSTR;

typedef short VARIANT_BOOL;

typedef VARIANT_BOOL _VARIANT_BOOL;

#define VARIANT_TRUE  ((VARIANT_BOOL)0xFFFF)
#define VARIANT_FALSE ((VARIANT_BOOL)0x0000)
typedef struct tagBSTRBLOB {
    ULONG cbSize;
    BYTE *pData;
} BSTRBLOB, *LPBSTRBLOB;

#ifndef _tagBLOB_DEFINED
#define _tagBLOB_DEFINED
#define _BLOB_DEFINED
#define _LPBLOB_DEFINED
typedef struct tagBLOB {
    ULONG cbSize;
    BYTE *pBlobData;
} BLOB, *LPBLOB;

#endif
typedef struct tagCLIPDATA {
    ULONG cbSize;
    long ulClipFmt;
    BYTE *pClipData;
} CLIPDATA;

#define CBPCLIPDATA(cb) ((cb).cbSize - sizeof((cb).ulClipFmt))
typedef ULONG PROPID;

typedef unsigned short VARTYPE;

enum VARENUM {
    VT_EMPTY = 0,
    VT_NULL = 1,
    VT_I2 = 2,
    VT_I4 = 3,
    VT_R4 = 4,
    VT_R8 = 5,
    VT_CY = 6,
    VT_DATE = 7,
    VT_BSTR = 8,
    VT_DISPATCH = 9,
    VT_ERROR = 10,
    VT_BOOL = 11,
    VT_VARIANT = 12,
    VT_UNKNOWN = 13,
    VT_DECIMAL = 14,
    VT_I1 = 16,
    VT_UI1 = 17,
    VT_UI2 = 18,
    VT_UI4 = 19,
    VT_I8 = 20,
    VT_UI8 = 21,
    VT_INT = 22,
    VT_UINT = 23,
    VT_VOID = 24,
    VT_HRESULT = 25,
    VT_PTR = 26,
    VT_SAFEARRAY = 27,
    VT_CARRAY = 28,
    VT_USERDEFINED = 29,
    VT_LPSTR = 30,
    VT_LPWSTR = 31,
    VT_RECORD = 36,
    VT_INT_PTR = 37,
    VT_UINT_PTR = 38,
    VT_FILETIME = 64,
    VT_BLOB = 65,
    VT_STREAM = 66,
    VT_STORAGE = 67,
    VT_STREAMED_OBJECT = 68,
    VT_STORED_OBJECT = 69,
    VT_BLOB_OBJECT = 70,
    VT_CF = 71,
    VT_CLSID = 72,
    VT_BSTR_BLOB = 0xfff,
    VT_VECTOR = 0x1000,
    VT_ARRAY = 0x2000,
    VT_BYREF = 0x4000,
    VT_RESERVED = 0x8000,
    VT_ILLEGAL = 0xffff,
    VT_ILLEGALMASKED = 0xfff,
    VT_TYPEMASK = 0xfff
};

#ifdef __cplusplus
}
#endif
#endif /* __WIDL_WTYPES_H */
