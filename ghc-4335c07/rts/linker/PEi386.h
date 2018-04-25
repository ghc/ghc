#pragma once

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

/********************************************
 * COFF/PE types
 ********************************************/

typedef enum _COFF_OBJ_TYPE {
    COFF_IMAGE,
    COFF_ANON_OBJ,
    COFF_IMPORT_LIB,
    COFF_ANON_BIG_OBJ,
    COFF_UNKNOWN
} COFF_OBJ_TYPE;

typedef struct _COFF_HEADER_INFO {
   COFF_OBJ_TYPE type;
   uint16_t sizeOfOptionalHeader;
   uint16_t sizeOfHeader;
   uint32_t pointerToSymbolTable;
   uint32_t numberOfSymbols;
   uint32_t numberOfSections;
} COFF_HEADER_INFO;

/********************************************
 * COFF/PE prototypes
 ********************************************/

void initLinker_PEi386( void );
const char * addDLL_PEi386( pathchar *dll_name, HINSTANCE *instance  );
void freePreloadObjectFile_PEi386( ObjectCode *oc );

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
SymbolAddr *lookupSymbolInDLLs ( unsigned char *lbl );
/* See Note [mingw-w64 name decoration scheme] */
pathchar* resolveSymbolAddr_PEi386 ( pathchar* buffer, int size,
                                     SymbolAddr* symbol, uintptr_t* top );

char *
allocateImageAndTrampolines (
    pathchar* arch_name, char* member_name,
    FILE* f,
    int size,
    int isThin);

/********************************************
 * COFF/PE headers
 ********************************************/
/* Section 7.1 PE Specification */
typedef IMPORT_OBJECT_HEADER COFF_import_header;
#define sizeof_COFF_import_Header sizeof(COFF_import_header)

typedef IMAGE_SECTION_HEADER COFF_section;
#define sizeof_COFF_section sizeof(COFF_section)


typedef IMAGE_SYMBOL COFF_symbol_og;
#define sizeof_COFF_symbol_og sizeof(COFF_symbol_og)

typedef IMAGE_SYMBOL_EX COFF_symbol_ex;
#define sizeof_COFF_symbol_ex sizeof(COFF_symbol_ex)

typedef IMAGE_RELOCATION COFF_reloc;
#define sizeof_COFF_reloc sizeof(COFF_reloc)

// MingW-w64 is missing these from the implementation. So we have to look them up
typedef DLL_DIRECTORY_COOKIE(WINAPI *LPAddDLLDirectory)(PCWSTR NewDirectory);
typedef WINBOOL(WINAPI *LPRemoveDLLDirectory)(DLL_DIRECTORY_COOKIE Cookie);

/* Combine union of possible symbol types.  */
typedef
union _COFF_symbol {
    COFF_symbol_og og;
    COFF_symbol_ex ex;
} COFF_symbol;

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

/* Util symbol handling functions.  */
COFF_OBJ_TYPE getObjectType ( char* image, pathchar* fileName );
COFF_HEADER_INFO* getHeaderInfo ( ObjectCode* oc );
size_t getSymbolSize ( COFF_HEADER_INFO *info );
int32_t getSymSectionNumber ( COFF_HEADER_INFO *info, COFF_symbol* sym );
uint32_t getSymValue ( COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t getSymStorageClass ( COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t getSymNumberOfAuxSymbols ( COFF_HEADER_INFO *info, COFF_symbol* sym );
uint16_t getSymType ( COFF_HEADER_INFO *info, COFF_symbol* sym );
uint8_t* getSymShortName ( COFF_HEADER_INFO *info, COFF_symbol* sym );

/* See Note [mingw-w64 name decoration scheme] */
#if !defined(x86_64_HOST_ARCH)
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
