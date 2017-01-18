#ifndef LINKER_PE_I386_H
#define LINKER_PE_I386_H

#include "Rts.h"
#include "LinkerInternals.h"
#include "PathUtils.h"
#include <windows.h>
#include <stdbool.h>

#include "BeginPrivate.h"

#if defined(x86_64_HOST_ARCH)
#define PEi386_IMAGE_OFFSET 4
#else
#define PEi386_IMAGE_OFFSET 0
#endif

void initLinker_PEi386( void );
const char * addDLL_PEi386( pathchar *dll_name );
void freePreloadObjectFile_PEi386( ObjectCode *oc );

bool findAndLoadImportLibrary( ObjectCode* oc );
bool checkAndLoadImportLibrary( pathchar* arch_name, char* member_name, FILE* f);

pathchar* findSystemLibrary_PEi386( pathchar* dll_name );
HsPtr addLibrarySearchPath_PEi386( pathchar* dll_path );
bool removeLibrarySearchPath_PEi386( HsPtr dll_path_index );

bool ocResolve_PEi386     ( ObjectCode* oc );
bool ocRunInit_PEi386     ( ObjectCode *oc );
bool ocGetNames_PEi386    ( ObjectCode* oc );
bool ocVerifyImage_PEi386 ( ObjectCode* oc );
SymbolAddr *lookupSymbol_PEi386(SymbolName *lbl);
bool ocAllocateSymbolExtras_PEi386 ( ObjectCode* oc );
void *lookupSymbolInDLLs ( unsigned char *lbl );
/* See Note [mingw-w64 name decoration scheme] */

char *
allocateImageAndTrampolines (
    pathchar* arch_name, char* member_name,
    FILE* f,
    int size,
    int isThin);

/********************************************
 * COFF/PE headers
 ********************************************/
typedef IMAGE_FILE_HEADER COFF_header;
#define sizeof_COFF_header sizeof(COFF_header)

/* Section 7.1 PE Specification */
typedef IMPORT_OBJECT_HEADER COFF_import_header;
#define sizeof_COFF_import_Header sizeof(COFF_import_header)

typedef IMAGE_SECTION_HEADER COFF_section;
#define sizeof_COFF_section sizeof(COFF_section)


typedef IMAGE_SYMBOL COFF_symbol;
#define sizeof_COFF_symbol sizeof(COFF_symbol)


typedef IMAGE_RELOCATION COFF_reloc;
#define sizeof_COFF_reloc sizeof(COFF_reloc)

// MingW-w64 is missing these from the implementation. So we have to look them up
typedef DLL_DIRECTORY_COOKIE(WINAPI *LPAddDLLDirectory)(PCWSTR NewDirectory);
typedef WINBOOL(WINAPI *LPRemoveDLLDirectory)(DLL_DIRECTORY_COOKIE Cookie);

/* A record for storing handles into DLLs. */
typedef
struct _OpenedDLL {
    pathchar*          name;
    struct _OpenedDLL* next;
    HINSTANCE instance;
} OpenedDLL;

/* A record for storing indirectly linked functions from DLLs. */
typedef
struct _IndirectAddr {
    SymbolAddr*           addr;
    struct _IndirectAddr* next;
} IndirectAddr;

/* See Note [mingw-w64 name decoration scheme] */
#ifndef x86_64_HOST_ARCH
#define STRIP_LEADING_UNDERSCORE 1
#else
#define STRIP_LEADING_UNDERSCORE 0
#endif

/*
Note [mingw-w64 name decoration scheme]

What's going on with name decoration? Well, original code
have some crufty and ad-hocish paths related mostly to very old
mingw gcc/binutils/runtime combinations. Now mingw-w64 offers pretty
uniform and MS-compatible decoration scheme across its tools and runtime.

The scheme is pretty straightforward: on 32 bit objects symbols are exported
with underscore prepended (and @ + stack size suffix appended for stdcall
functions), on 64 bits no underscore is prepended and no suffix is appended
because we have no stdcall convention on 64 bits.

See #9218
*/


#include "EndPrivate.h"

#endif /* LINKER_PE_I386_H */
