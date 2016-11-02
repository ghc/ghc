#ifndef LINKER_PE_I386_H
#define LINKER_PE_I386_H

#include "Rts.h"
#include "LinkerInternals.h"
#include "PathUtils.h"

#include "BeginPrivate.h"

#if defined(x86_64_HOST_ARCH)
#define PEi386_IMAGE_OFFSET 4
#else
#define PEi386_IMAGE_OFFSET 0
#endif

void initLinker_PEi386(void);
const char * addDLL_PEi386(pathchar *dll_name);
void freePreloadObjectFile_PEi386(ObjectCode *oc);

int findAndLoadImportLibrary(ObjectCode* oc);
int checkAndLoadImportLibrary( pathchar* arch_name, char* member_name, FILE* f);

pathchar* findSystemLibrary_PEi386(pathchar* dll_name);
HsPtr addLibrarySearchPath_PEi386(pathchar* dll_path);
HsBool removeLibrarySearchPath_PEi386(HsPtr dll_path_index);

int ocResolve_PEi386     ( ObjectCode* oc );
int ocRunInit_PEi386     ( ObjectCode *oc );
int ocGetNames_PEi386    ( ObjectCode* oc );
int ocVerifyImage_PEi386 ( ObjectCode* oc );
SymbolAddr *lookupSymbol_PEi386(SymbolName *lbl);
int ocAllocateSymbolExtras_PEi386 ( ObjectCode* oc );
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
typedef unsigned char          UChar;
typedef unsigned short         UInt16;
typedef short                  Int16;
typedef unsigned int           UInt32;
typedef          int           Int32;
typedef unsigned long long int UInt64;


typedef
struct {
    UInt16 Machine;
    UInt16 NumberOfSections;
    UInt32 TimeDateStamp;
    UInt32 PointerToSymbolTable;
    UInt32 NumberOfSymbols;
    UInt16 SizeOfOptionalHeader;
    UInt16 Characteristics;
}
COFF_header;

#define sizeof_COFF_header 20

/* Section 7.1 PE Specification */
typedef
struct {
    UInt16 Sig1;
    UInt16 Sig2;
    UInt16 Version;
    UInt16 Machine;
    UInt32 TimeDateStamp;
    UInt32 SizeOfData;
    UInt16 Ordinal;
    UInt16 Type_NameType_Reserved;
}
COFF_import_header;

#define sizeof_COFF_import_Header 20

typedef
struct {
    UChar  Name[8];
    UInt32 VirtualSize;
    UInt32 VirtualAddress;
    UInt32 SizeOfRawData;
    UInt32 PointerToRawData;
    UInt32 PointerToRelocations;
    UInt32 PointerToLinenumbers;
    UInt16 NumberOfRelocations;
    UInt16 NumberOfLineNumbers;
    UInt32 Characteristics;
}
COFF_section;

#define sizeof_COFF_section 40


typedef
struct {
    UChar  Name[8];
    UInt32 Value;
    Int16  SectionNumber;
    UInt16 Type;
    UChar  StorageClass;
    UChar  NumberOfAuxSymbols;
}
COFF_symbol;

#define sizeof_COFF_symbol 18


typedef
struct {
    UInt32 VirtualAddress;
    UInt32 SymbolTableIndex;
    UInt16 Type;
}
COFF_reloc;

#define sizeof_COFF_reloc 10

/* From PE spec doc, section 3.3.2 */
/* Note use of MYIMAGE_* since IMAGE_* are already defined in
windows.h -- for the same purpose, but I want to know what I'm
getting, here. */
#define MYIMAGE_FILE_RELOCS_STRIPPED        0x0001
#define MYIMAGE_FILE_EXECUTABLE_IMAGE       0x0002
#define MYIMAGE_FILE_DLL                    0x2000
#define MYIMAGE_FILE_SYSTEM                 0x1000
#define MYIMAGE_FILE_BYTES_REVERSED_HI      0x8000
#define MYIMAGE_FILE_BYTES_REVERSED_LO      0x0080
#define MYIMAGE_FILE_32BIT_MACHINE          0x0100

/* From PE spec doc, section 5.4.2 and 5.4.4 */
#define MYIMAGE_SYM_CLASS_EXTERNAL          2
#define MYIMAGE_SYM_CLASS_STATIC            3
#define MYIMAGE_SYM_UNDEFINED               0
#define MYIMAGE_SYM_CLASS_SECTION           104
#define MYIMAGE_SYM_CLASS_WEAK_EXTERNAL     105

/* From PE spec doc, section 3.1 */
#define MYIMAGE_SCN_CNT_CODE                0x00000020
#define MYIMAGE_SCN_CNT_INITIALIZED_DATA    0x00000040
#define MYIMAGE_SCN_CNT_UNINITIALIZED_DATA  0x00000080
#define MYIMAGE_SCN_LNK_COMDAT              0x00001000
#define MYIMAGE_SCN_LNK_NRELOC_OVFL         0x01000000
#define MYIMAGE_SCN_LNK_REMOVE              0x00000800
#define MYIMAGE_SCN_MEM_DISCARDABLE         0x02000000

/* From PE spec doc, section 5.2.1 */
#define MYIMAGE_REL_I386_DIR32              0x0006
#define MYIMAGE_REL_I386_DIR32NB            0x0007
#define MYIMAGE_REL_I386_REL32              0x0014

#include "EndPrivate.h"

#endif /* LINKER_PE_I386_H */
