
/* --------------------------------------------------------------------------
 * Machinery for dynamic loading and linking of object code.  Should be 
 * completely independent from the rest of Hugs so we can use it in
 * other applications if desired.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "config.h"                             /* for linux_TARGET_OS etc */
#include "object.h"


#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
static int ocVerifyImage_ELF    ( ObjectCode* oc, int verb );
static int ocGetNames_ELF       ( ObjectCode* oc, int verb );
static int ocResolve_ELF        ( ObjectCode* oc, int verb );
#elif defined(cygwin32_TARGET_OS)
static int ocVerifyImage_PEi386 ( ObjectCode* oc, int verb );
static int ocGetNames_PEi386    ( ObjectCode* oc, int verb );
static int ocResolve_PEi386     ( ObjectCode* oc, int verb );
#endif

static char* hackyAppend ( char* s1, char* s2 );
static int   sortSymbols ( ObjectCode* oc );


/* --------------------------------------------------------------------------
 * Arch-independent interface to the runtime linker
 * ------------------------------------------------------------------------*/

ObjectCode*  ocNew ( void  (*errMsg)(char*),
                     void* (*clientLookup)(char*),
                     char*  objFileName,
                     int    objFileSize )
{
   ObjectCode* oc     = malloc(sizeof(ObjectCode));
   if (!oc) {
      errMsg("ocNew: can't allocate memory for object code record");
      return NULL;
   }

#  if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   oc->formatName = "ELF";
#  elif defined(cygwin32_TARGET_OS)
   oc->formatName = "PEi386";
#  else
   free(oc);
   errMsg("ocNew: not implemented on this platform");
   return NULL;
#  endif

   oc->status         = OBJECT_NOTINUSE;
   oc->objFileName    = objFileName;
   oc->objFileSize    = objFileSize;
   oc->errMsg         = errMsg;
   oc->clientLookup   = clientLookup;

   oc->oImage         = malloc ( objFileSize );
   if (!oc->oImage) {
      free(oc);
      errMsg("ocNew: can't allocate memory for object code");
      return NULL;
   }
   oc->oTab           = NULL;
   oc->sizeoTab       = 0;
   oc->usedoTab       = 0;
   oc->sectionTab     = NULL;
   oc->sizesectionTab = 0;
   oc->usedsectionTab = 0;
   oc->next           = NULL;

   return oc;
}
                            

int ocLoadImage ( ObjectCode* oc, int verb )
{
   int   n;
   FILE* f;
   assert (oc && oc->status==OBJECT_NOTINUSE);
   if (verb) fprintf(stderr, "ocLoadImage %s\n", oc->objFileName );
   f = fopen(oc->objFileName, "rb");
   if (!f) {
       (oc->errMsg(hackyAppend("ocLoadImage: can't read: ",
                               oc->objFileName)));
       return 0;
   }
   n = fread ( oc->oImage, 1, oc->objFileSize, f );
   if (n != oc->objFileSize) {
      fclose(f);
      oc->errMsg(hackyAppend("ocLoadImage: I/O error whilst reading: ",
                             oc->objFileName));
      return 0;
   }
   oc->status = OBJECT_OIMAGE;
   if (verb) fprintf(stderr, "ocLoadImage %s: read %d bytes\n", 
                     oc->objFileName, oc->objFileSize );
   return 1;
}


/* returns 1 if ok, 0 if error */
int ocVerifyImage ( ObjectCode* oc, int verb )
{
   int ret;
   assert (oc && oc->status==OBJECT_OIMAGE);
   if (verb) fprintf(stderr, "ocVerifyImage: begin\n");
#  if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   ret = ocVerifyImage_ELF ( oc, verb );
#  elif defined(cygwin32_TARGET_OS)
   ret = ocVerifyImage_PEi386 ( oc, verb );
#  else
   oc->errMsg("ocVerifyImage: not implemented on this platform");
   return 0;
#  endif
   if (verb) fprintf(stderr, "ocVerifyImage: done, status = %d", ret);

   if (ret) oc->status = OBJECT_VERIFIED;
   return ret;
}


/* returns 1 if ok, 0 if error */
int ocGetNames ( ObjectCode* oc, int verb )
{
   int ret;
   assert (oc && oc->status==OBJECT_VERIFIED);
   if (verb) fprintf(stderr, "ocGetNames: begin\n");
#  if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   ret = ocGetNames_ELF ( oc, verb );
#  elif defined(cygwin32_TARGET_OS)
   ret = ocGetNames_PEi386 ( oc, verb );
#  else
   oc->errMsg("ocGetNames: not implemented on this platform");
   return 0;
#  endif
   if (verb) fprintf(stderr, "ocGetNames: done, status = %d\n", ret);
   if (ret) ret = sortSymbols(oc);
   if (ret) oc->status = OBJECT_HAVENAMES;
   return ret;
}


/* returns 1 if ok, 0 if error */
int ocResolve ( ObjectCode* oc, int verb )
{
   int ret;
   assert (oc && oc->status==OBJECT_HAVENAMES);
   if (verb) fprintf(stderr, "ocResolve: begin\n");
#  if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   ret = ocResolve_ELF ( oc, verb );
#  elif defined(cygwin32_TARGET_OS)
   ret = ocResolve_PEi386 ( oc, verb );
#  else
   oc->errMsg("ocResolve: not implemented on this platform");
   return 0;
#  endif
   if (verb) fprintf(stderr, "ocResolve: done, status = %d\n", ret);
   if (ret) oc->status = OBJECT_RESOLVED;
   return ret;
}


void ocFree ( ObjectCode* oc )
{
   if (oc) {
      if (oc->oImage)     free(oc->oImage);
      if (oc->oTab)       free(oc->oTab);
      if (oc->sectionTab) free(oc->sectionTab);
      free(oc);
   }
}


/* --------------------------------------------------------------------------
 * Simple, dynamically expandable association tables
 * ------------------------------------------------------------------------*/

/* A bit tricky.  Assumes that if tab==NULL, then 
   currUsed and *currSize must be zero.
   Returns NULL if expansion failed.
*/
static void* genericExpand ( void* tab, 
                             int*  currSize, int  currUsed,
                             int   initSize, int  elemSize )
{
   int   size2;
   void* tab2;
   if (currUsed < *currSize) return tab;
   size2 = (*currSize == 0) ? initSize : (2 * *currSize);
   tab2 = malloc ( size2 * elemSize );
   if (!tab2) return NULL;
   if (*currSize > 0)
      memcpy ( tab2, tab, elemSize * *currSize );
   *currSize = size2;
   if (tab) free ( tab );
   return tab2;
}


/* returns 1 if success, 0 if error */
static int addSymbol ( ObjectCode* oc, char* nm, void* ad )
{
   OSym* newTab
      = genericExpand ( oc->oTab, 
                        &(oc->sizeoTab),
                        oc->usedoTab,
                        8, sizeof(OSym) );

   if (!newTab) {
      oc->errMsg("addSymbol: malloc failed whilst expanding table");
      return 0;
   }
   oc->oTab = newTab;
   oc->oTab[ oc->usedoTab ].nm = nm;
   oc->oTab[ oc->usedoTab ].ad = ad;
   oc->usedoTab++;
   return 1;
}


/* Reorder symbol table so that symbols are in alphabetical order.
   Detects an error if, after sorting, any two symbols are the same,
   since this would imply that the same symbol has been inserted more 
   than once.  Returns 1 if success, 0 if error.
*/
static int sortSymbols ( ObjectCode* oc )
{
   static int incs[14] 
      = { 1, 4, 13, 40, 121, 364, 1093, 3280,
          9841, 29524, 88573, 265720, 797161, 2391484 };

   int lo = 0;
   int hi = oc->usedoTab-1;
   int i, j, h, bigN, hp;
   OSym v;

   bigN = hi - lo + 1; if (bigN < 2) return 1;
   hp = 0; while (incs[hp] < bigN) hp++; hp--;

   for (; hp >= 0; hp--) {
      h = incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = oc->oTab[i];
         j = i;
         while (strcmp(oc->oTab[j-h].nm, v.nm) > 0) {
            oc->oTab[j] = oc->oTab[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         oc->oTab[j] = v;
         i++;
      }
   }

   for (i = 1; i < oc->usedoTab; i++) {
      j = strcmp(oc->oTab[i-1].nm, oc->oTab[i].nm);
      if (j  > 0) { 
         oc->errMsg("sortSymbols: sorting failed"); 
         return 0;
      }
      if (j == 0) {
         oc->errMsg("sortSymbols: duplicate symbols in object file");
         return 0;
      }
   }

   return 1;
}


/* returns 1 if success, 0 if error */
static int addSection ( ObjectCode* oc, void* start, void* end, OSectionKind sect )
{
   OSection* newTab
      = genericExpand ( oc->sectionTab,
                        &(oc->sizesectionTab),
                        oc->usedsectionTab,
                        4, sizeof(OSection) );
   if (!newTab) {
      oc->errMsg("addSection: malloc failed whilst expanding table");
      return 0;
   }
   oc->sectionTab = newTab;
   oc->sectionTab[ oc->usedsectionTab ].start = start;
   oc->sectionTab[ oc->usedsectionTab ].end   = end;
   oc->sectionTab[ oc->usedsectionTab ].kind  = sect;
   oc->usedsectionTab++;
   return 1;
}


void* ocLookupSym ( ObjectCode* oc, char* sym )
{
   int lo, hi, mid, cmp;

   assert(oc);
   if (oc->status != OBJECT_HAVENAMES 
       && oc->status != OBJECT_RESOLVED) {
      oc->errMsg("ocLookupSym: no symbols available");
      return NULL;
   }

   /* Originally used a sequential search; should still work
   for (i = 0; i < oc->usedoTab; i++) {
      if (0)
         fprintf ( stderr, 
                   "ocLookupSym: request %s, table has %s\n",
                   sym, oc->oTab[i].nm );
      if (0==strcmp(sym,oc->oTab[i].nm))
         return oc->oTab[i].ad;
   }
   */

   lo = 0; 
   hi = oc->usedoTab-1;
   while (1) {
      /* Invariant: the unsearched area is oc->oTab[lo .. hi] inclusive. */
      if (hi < lo) return NULL;
      mid = (hi + lo) / 2;
      cmp = strcmp(sym, oc->oTab[mid].nm);
      if (cmp == 0) return oc->oTab[mid].ad;
      if (cmp < 0) hi = mid-1;
      if (cmp > 0) lo = mid+1;
   }
}


char* ocLookupAddr ( ObjectCode* oc, void* addr )
{
   int i;

   assert(oc);
   if (oc->status != OBJECT_HAVENAMES 
       && oc->status != OBJECT_RESOLVED) {
      oc->errMsg("ocLookupAddr: no symbols available");
      return NULL;
   }

   for (i = 0; i < oc->usedoTab; i++) {
      if (addr == oc->oTab[i].ad)
         return oc->oTab[i].nm;
   }
   return NULL;
}


OSectionKind ocLookupSection ( ObjectCode* oc, void* addr )
{
   int i;

   assert(oc);
   if (oc->status != OBJECT_HAVENAMES 
       && oc->status != OBJECT_RESOLVED) {
      oc->errMsg("ocLookupSection: no symbols available");
      return HUGS_SECTIONKIND_NOINFOAVAIL;
   }


   for (i = 0; i < oc->usedsectionTab; i++) {
      if (oc->sectionTab[i].start <= addr 
          && addr <= oc->sectionTab[i].end)
         return oc->sectionTab[i].kind;
   }

   return HUGS_SECTIONKIND_NOINFOAVAIL;
}


/* Ghastly append which leaks space.  But we only use it for
   error messages -- that's my excuse.
*/
static char* hackyAppend ( char* s1, char* s2 )
{
   char* res = malloc ( 4 + strlen(s1) + strlen(s2) );
   if (!res) {
      fprintf ( stderr, "hugs: fatal: hackyAppend\n\t%s\n\t%s\n", s1, s2 );
      assert(res);
   }
   strcpy(res,s1);
   strcat(res,s2);
   return res;
}

/* --------------------------------------------------------------------------
 * PEi386 specifics (cygwin32)
 * ------------------------------------------------------------------------*/

/* The information for this linker comes from 
      Microsoft Portable Executable 
      and Common Object File Format Specification
      revision 5.1 January 1998
   which SimonM says comes from the MS Developer Network CDs.
*/
      

#if defined(cygwin32_TARGET_OS)

#define FALSE 0
#define TRUE  1


typedef unsigned char  UChar;
typedef unsigned short UInt16;
typedef unsigned int   UInt32;
typedef          int   Int32;


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
      UInt16 SectionNumber;
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
#define IMAGE_FILE_RELOCS_STRIPPED     0x0001
#define IMAGE_FILE_EXECUTABLE_IMAGE    0x0002
#define IMAGE_FILE_DLL                 0x2000
#define IMAGE_FILE_SYSTEM              0x1000
#define IMAGE_FILE_BYTES_REVERSED_HI   0x8000
#define IMAGE_FILE_BYTES_REVERSED_LO   0x0080
#define IMAGE_FILE_32BIT_MACHINE       0x0100

/* From PE spec doc, section 5.4.2 and 5.4.4 */
#define IMAGE_SYM_CLASS_EXTERNAL       2
#define IMAGE_SYM_CLASS_STATIC         3
#define IMAGE_SYM_UNDEFINED            0

/* From PE spec doc, section 4.1 */
#define IMAGE_SCN_CNT_CODE             0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040

/* From PE spec doc, section 5.2.1 */
#define IMAGE_REL_I386_DIR32           0x0006
#define IMAGE_REL_I386_REL32           0x0014


/* We use myindex to calculate array addresses, rather than
   simply doing the normal subscript thing.  That's because
   some of the above structs have sizes which are not 
   a whole number of words.  GCC rounds their sizes up to a
   whole number of words, which means that the address calcs
   arising from using normal C indexing or pointer arithmetic
   are just plain wrong.  Sigh.
*/
static UChar* myindex ( int scale, int index, void* base )
{
   return
      ((UChar*)base) + scale * index;
}


static void printName ( UChar* name, UChar* strtab )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      fprintf ( stderr, "%s", strtab + strtab_offset );
   } else {
      int i;
      for (i = 0; i < 8; i++) {
         if (name[i] == 0) break;
         fprintf ( stderr, "%c", name[i] );
      }
   }
}


static void copyName ( UChar* name, UChar* strtab, 
                       UChar* dst, int dstSize )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      strncpy ( dst, strtab+strtab_offset, dstSize );
      dst[dstSize-1] = 0;
   } else {
      int i = 0;
      while (1) {
         if (i >= 8) break;
         if (name[i] == 0) break;
         dst[i] = name[i];
         i++;
      }
      dst[i] = 0;
   }
}


static UChar* cstring_from_COFF_symbol_name ( UChar* name, 
                                              UChar* strtab )
{
   UChar* newstr;
   /* If the string is longer than 8 bytes, look in the
      string table for it -- this will be correctly zero terminated. 
   */
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      return ((UChar*)strtab) + strtab_offset;
   }
   /* Otherwise, if shorter than 8 bytes, return the original,
      which by defn is correctly terminated.
   */
   if (name[7]==0) return name;
   /* The annoying case: 8 bytes.  Copy into a temporary
      (which is never freed ...)
   */
   newstr = malloc(9);
   if (newstr) {
      strncpy(newstr,name,8);
      newstr[8] = 0;
   }
   return newstr;
}


/* Just compares the short names (first 8 chars) */
static COFF_section* findPEi386SectionCalled ( ObjectCode* oc,
                                               char* name )
{
   int i;
   COFF_header* hdr 
      = (COFF_header*)(oc->oImage);
   COFF_section* sectab 
      = (COFF_section*) (
           ((UChar*)(oc->oImage)) 
           + sizeof_COFF_header + hdr->SizeOfOptionalHeader
        );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* n1;
      UChar* n2;
      COFF_section* section_i 
         = (COFF_section*)
           myindex ( sizeof_COFF_section, i, sectab );
      n1 = (UChar*) &(section_i->Name);
      n2 = name;
      if (n1[0]==n2[0] && n1[1]==n2[1] && n1[2]==n2[2] && 
          n1[3]==n2[3] && n1[4]==n2[4] && n1[5]==n2[5] && 
          n1[6]==n2[6] && n1[7]==n2[7])
         return section_i;
   }

   return NULL;
}


static void zapTrailingAtSign ( UChar* sym )
{
   int i, j;
   if (sym[0] == 0) return;
   i = 0; 
   while (sym[i] != 0) i++;
   i--;
   j = i;
   while (j > 0 && isdigit(sym[j])) j--;
   if (j > 0 && sym[j] == '@' && j != i) sym[j] = 0;
}


static int ocVerifyImage_PEi386 ( ObjectCode* oc, int verb )
{
   int i, j;
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   hdr = (COFF_header*)(oc->oImage);
   sectab = (COFF_section*) (
               ((UChar*)(oc->oImage)) 
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->oImage))
               + hdr->PointerToSymbolTable 
            );
   strtab = ((UChar*)(oc->oImage))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   if (hdr->Machine != 0x14c) {
      oc->errMsg("Not x86 PEi386");
      return FALSE;
   }
   if (hdr->SizeOfOptionalHeader != 0) {
      oc->errMsg("PEi386 with nonempty optional header");
      return FALSE;
   }
   if ( /* (hdr->Characteristics & IMAGE_FILE_RELOCS_STRIPPED) || */
        (hdr->Characteristics & IMAGE_FILE_EXECUTABLE_IMAGE) ||
        (hdr->Characteristics & IMAGE_FILE_DLL) ||
        (hdr->Characteristics & IMAGE_FILE_SYSTEM) ) {
      oc->errMsg("Not a PEi386 object file");
      return FALSE;
   }
   if ( (hdr->Characteristics & IMAGE_FILE_BYTES_REVERSED_HI) ||
        !(hdr->Characteristics & IMAGE_FILE_32BIT_MACHINE) ) {
      oc->errMsg("Invalid PEi386 word size or endiannness");
      return FALSE;
   }

   if (!verb) return TRUE;
   /* No further verification after this point; only debug printing. */

   fprintf ( stderr, 
             "sectab offset = %d\n", ((UChar*)sectab) - ((UChar*)hdr) );
   fprintf ( stderr, 
             "symtab offset = %d\n", ((UChar*)symtab) - ((UChar*)hdr) );
   fprintf ( stderr, 
             "strtab offset = %d\n", ((UChar*)strtab) - ((UChar*)hdr) );

   fprintf ( stderr, "\n" );
   fprintf ( stderr, 
             "Machine:           0x%x\n", (UInt32)(hdr->Machine) );
   fprintf ( stderr, 
             "# sections:        %d\n",   (UInt32)(hdr->NumberOfSections) );
   fprintf ( stderr,
             "time/date:         0x%x\n", (UInt32)(hdr->TimeDateStamp) );
   fprintf ( stderr,
             "symtab offset:     %d\n",   (UInt32)(hdr->PointerToSymbolTable) );
   fprintf ( stderr, 
             "# symbols:         %d\n",   (UInt32)(hdr->NumberOfSymbols) );
   fprintf ( stderr, 
             "sz of opt hdr:     %d\n",   (UInt32)(hdr->SizeOfOptionalHeader) );
   fprintf ( stderr,
             "characteristics:   0x%x\n", (UInt32)(hdr->Characteristics) );

   fprintf ( stderr, "\n" );
   fprintf ( stderr, "string table has size 0x%x\n", * (UInt32*)strtab );
   fprintf ( stderr, "---START of string table---\n");
   for (i = 4; i < *(UInt32*)strtab; i++) {
      if (strtab[i] == 0) 
         fprintf ( stderr, "\n"); else 
         fprintf( stderr, "%c", strtab[i] );
   }
   fprintf ( stderr, "--- END  of string table---\n");

   fprintf ( stderr, "\n" );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_reloc* reltab;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, i, sectab );
      fprintf ( stderr, 
                "\n"
                "section %d\n"
                "     name `",
                i 
              );
      printName ( sectab_i->Name, strtab );
      fprintf ( stderr, 
                "'\n"
                "    vsize %d\n"
                "    vaddr %d\n"
                "  data sz %d\n"
                " data off %d\n"
                "  num rel %d\n"
                "  off rel %d\n",
                sectab_i->VirtualSize,
                sectab_i->VirtualAddress,
                sectab_i->SizeOfRawData,
                sectab_i->PointerToRawData,
                sectab_i->NumberOfRelocations,
                sectab_i->PointerToRelocations
              );
      reltab = (COFF_reloc*) (
                  ((UChar*)(oc->oImage)) + sectab_i->PointerToRelocations
               );
      for (j = 0; j < sectab_i->NumberOfRelocations; j++) {
         COFF_symbol* sym;
         COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, j, reltab );
         fprintf ( stderr, 
                   "        type 0x%-4x   vaddr 0x%-8x   name `",
                   (UInt32)rel->Type, 
                   rel->VirtualAddress );
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol, rel->SymbolTableIndex, symtab );
         printName ( sym->Name, strtab );
         fprintf ( stderr, "'\n" );
      }
      fprintf ( stderr, "\n" );
   }


   fprintf ( stderr, "\n" );
   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= hdr->NumberOfSymbols) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, i, symtab );
      fprintf ( stderr, 
                "symbol %d\n"
                "     name `",
                i 
              );
      printName ( symtab_i->Name, strtab );
      fprintf ( stderr, 
                "'\n"
                "    value 0x%x\n"
                "     sec# %d\n"
                "     type 0x%x\n"
                "   sclass 0x%x\n"
                "     nAux %d\n",
                symtab_i->Value,
                (Int32)(symtab_i->SectionNumber) - 1,
                (UInt32)symtab_i->Type,
                (UInt32)symtab_i->StorageClass,
                (UInt32)symtab_i->NumberOfAuxSymbols 
              );
      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   fprintf ( stderr, "\n" );

   return TRUE;
}


static int ocGetNames_PEi386 ( ObjectCode* oc, int verb )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UChar* sname;
   void*  addr;
   int    i;
   
   hdr = (COFF_header*)(oc->oImage);
   sectab = (COFF_section*) (
               ((UChar*)(oc->oImage)) 
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->oImage))
               + hdr->PointerToSymbolTable 
            );
   strtab = ((UChar*)(oc->oImage))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   /* Copy exported symbols into the ObjectCode. */
   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= hdr->NumberOfSymbols) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, i, symtab );

      if (symtab_i->StorageClass == IMAGE_SYM_CLASS_EXTERNAL &&
          symtab_i->SectionNumber != IMAGE_SYM_UNDEFINED) {

         /* This symbol is global and defined, viz, exported */
         COFF_section* sectabent;

         sname = cstring_from_COFF_symbol_name ( 
                    symtab_i->Name, strtab 
                 );
         if (!sname) {
            oc->errMsg("Out of memory when copying PEi386 symbol");
            return FALSE;
         }

         /* for IMAGE_SYMCLASS_EXTERNAL 
                && !IMAGE_SYM_UNDEFINED,
            the address of the symbol is: 
                address of relevant section + offset in section
         */
         sectabent = (COFF_section*)
                     myindex ( sizeof_COFF_section, 
                               symtab_i->SectionNumber-1,
                               sectab );
         addr = ((UChar*)(oc->oImage))
                + (sectabent->PointerToRawData
                   + symtab_i->Value);
         /* fprintf ( stderr, "addSymbol %p `%s'\n", addr,sname); */
         if (!addSymbol(oc,sname,addr)) return FALSE;
      }
      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   /* Copy section information into the ObjectCode. */
   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* start;
      UChar* end;

      OSectionKind kind 
         = HUGS_SECTIONKIND_OTHER;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, i, sectab );
      /* fprintf ( stderr, "section name = %s\n", sectab_i->Name ); */

#if 0
      /* I'm sure this is the Right Way to do it.  However, the 
         alternative of testing the sectab_i->Name field seems to
         work ok with Cygwin.
      */
      if (sectab_i->Characteristics & IMAGE_SCN_CNT_CODE || 
          sectab_i->Characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA)
         kind = HUGS_SECTIONKIND_CODE_OR_RODATA;
#endif

      if (0==strcmp(".text",sectab_i->Name))
         kind = HUGS_SECTIONKIND_CODE_OR_RODATA;
      if (0==strcmp(".data",sectab_i->Name) ||
          0==strcmp(".bss",sectab_i->Name))
         kind = HUGS_SECTIONKIND_RWDATA;

      start = ((UChar*)(oc->oImage)) 
              + sectab_i->PointerToRawData;
      end   = start 
              + sectab_i->SizeOfRawData - 1;

      if (kind != HUGS_SECTIONKIND_OTHER) {
         addSection ( oc, start, end, kind );
      } else {
         fprintf ( stderr, "unknown section name = `%s'\n", 
                   sectab_i->Name);
         oc->errMsg("Unknown PEi386 section name");
         return FALSE;
      }
   }

   return TRUE;   
}


static int ocResolve_PEi386 ( ObjectCode* oc, int verb )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UInt32        A;
   UInt32        S;
   UInt32*       pP;

   int i, j;
   char symbol[1000]; // ToDo
   
   hdr = (COFF_header*)(oc->oImage);
   sectab = (COFF_section*) (
               ((UChar*)(oc->oImage)) 
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->oImage))
               + hdr->PointerToSymbolTable 
            );
   strtab = ((UChar*)(oc->oImage))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, i, sectab );
      COFF_reloc* reltab
         = (COFF_reloc*) (
              ((UChar*)(oc->oImage)) + sectab_i->PointerToRelocations
           );
      for (j = 0; j < sectab_i->NumberOfRelocations; j++) {
         COFF_symbol* sym;
         COFF_reloc* reltab_j 
            = (COFF_reloc*)
              myindex ( sizeof_COFF_reloc, j, reltab );

         /* the location to patch */
         pP = (UInt32*)(
                 ((UChar*)(oc->oImage)) 
                 + (sectab_i->PointerToRawData 
                    + reltab_j->VirtualAddress)
              );
         /* the existing contents of pP */
         A = *pP;
         /* the symbol to connect to */
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol, 
                         reltab_j->SymbolTableIndex, symtab );
         if (verb) {
            fprintf ( stderr, 
                   "reloc sec %2d num %3d:  type 0x%-4x   "
                   "vaddr 0x%-8x   name `",
                   i, j,
                   (UInt32)reltab_j->Type, 
                   reltab_j->VirtualAddress );
            printName ( sym->Name, strtab );
            fprintf ( stderr, "'\n" );
         }

         if (sym->StorageClass == IMAGE_SYM_CLASS_STATIC) {
            COFF_section* section_sym 
               = findPEi386SectionCalled ( oc, sym->Name );
            if (!section_sym) {
               fprintf ( stderr, "bad section = `%s'\n", sym->Name );
               oc->errMsg("Can't find abovementioned PEi386 section");
               return FALSE;
            }
            S = ((UInt32)(oc->oImage))
                + (section_sym->PointerToRawData
                   + sym->Value);
         } else {
         copyName ( sym->Name, strtab, symbol, 1000 );
         zapTrailingAtSign ( symbol );
         S = (UInt32) ocLookupSym ( oc, symbol );
         if (S == 0) 
            S = (UInt32)(oc->clientLookup ( symbol ));
         if (S == 0) {
            char errtxt[2000];
            strcpy(errtxt,oc->objFileName);
            strcat(errtxt,": unresolvable reference to: ");
            strcat(errtxt,symbol);
            oc->errMsg(errtxt);
            return FALSE;
         }
         }

         switch (reltab_j->Type) {
            case IMAGE_REL_I386_DIR32: 
               *pP = A + S; 
               break;
            case IMAGE_REL_I386_REL32:
               /* Tricky.  We have to insert a displacement at
                  pP which, when added to the PC for the _next_
                  insn, gives the address of the target (S).
                  Problem is to know the address of the next insn
                  when we only know pP.  We assume that this
                  literal field is always the last in the insn,
                  so that the address of the next insn is pP+4
                  -- hence the constant 4.
                  Also I don't know if A should be added, but so
                  far it has always been zero.
	       */
               assert(A==0);
               *pP = S - ((UInt32)pP) - 4;
               break;
            default: 
               fprintf(stderr, 
                       "unhandled PEi386 relocation type %d\n",
                       reltab_j->Type);
               oc->errMsg("unhandled PEi386 relocation type");
               return FALSE;
         }

      }
   }
   
   return TRUE;
}

#endif /* defined(cygwin32_TARGET_OS) */


/* --------------------------------------------------------------------------
 * ELF specifics (Linux, Solaris)
 * ------------------------------------------------------------------------*/

#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)

#define FALSE 0
#define TRUE  1

#include <elf.h>

static char* findElfSection ( void* objImage, Elf32_Word sh_type )
{
   int i;
   char* ehdrC = (char*)objImage;
   Elf32_Ehdr* ehdr = ( Elf32_Ehdr*)ehdrC;
   Elf32_Shdr* shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   char* ptr = NULL;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == sh_type &&
          i !=  ehdr->e_shstrndx) {
         ptr = ehdrC + shdr[i].sh_offset;
         break;
      }
   }
   return ptr;
}


static int ocVerifyImage_ELF ( ObjectCode* oc, int verb )
{
   Elf32_Shdr* shdr;
   Elf32_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;
   char* strtab;

   char*       ehdrC = (char*)(oc->oImage);
   Elf32_Ehdr* ehdr  = ( Elf32_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      oc->errMsg("Not an ELF header");
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is an ELF header\n" );

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS32) {
      oc->errMsg("Not 32 bit ELF" );
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is 32 bit ELF\n" );

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
      if (verb) fprintf ( stderr, "Is little-endian\n" );
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
      if (verb) fprintf ( stderr, "Is big-endian\n" );
   } else {
      oc->errMsg("Unknown endiannness");
      return FALSE;
   }

   if (ehdr->e_type != ET_REL) {
      oc->errMsg("Not a relocatable object (.o) file");
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is a relocatable object (.o) file\n" );

   if (verb) fprintf ( stderr, "Architecture is " );
   switch (ehdr->e_machine) {
      case EM_386:   if (verb) fprintf ( stderr, "x86\n" ); break;
      case EM_SPARC: if (verb) fprintf ( stderr, "sparc\n" ); break;
      default:       if (verb) fprintf ( stderr, "unknown\n" ); 
                     oc->errMsg("Unknown architecture");
                     return FALSE;
   }

   if (verb) 
   fprintf ( stderr,
             "\nSection header table: start %d, n_entries %d, ent_size %d\n", 
             ehdr->e_shoff, ehdr->e_shnum, ehdr->e_shentsize  );

   assert (ehdr->e_shentsize == sizeof(Elf32_Shdr));

   shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);

   if (ehdr->e_shstrndx == SHN_UNDEF) {
      oc->errMsg("No section header string table");
      return FALSE;
   } else {
      if (verb) fprintf (  stderr,"Section header string table is section %d\n", 
                          ehdr->e_shstrndx);
      sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (verb) fprintf ( stderr, "%2d:  ", i );
      if (verb) fprintf ( stderr, "type=%2d  ", shdr[i].sh_type );
      if (verb) fprintf ( stderr, "size=%4d  ", shdr[i].sh_size );
      if (verb) fprintf ( stderr, "offs=%4d  ", shdr[i].sh_offset );
      if (verb) fprintf ( stderr, "  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset, 
               ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);

      if (shdr[i].sh_type == SHT_REL  && verb) fprintf ( stderr, "Rel  " ); else
      if (shdr[i].sh_type == SHT_RELA && verb) fprintf ( stderr, "RelA " ); else
      if (verb)                                fprintf ( stderr, "     " );
      if (sh_strtab && verb) 
         fprintf ( stderr, "sname=%s", sh_strtab + shdr[i].sh_name );
      if (verb) fprintf ( stderr, "\n" );
   }

   if (verb) fprintf ( stderr, "\n\nString tables\n" );
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB &&
          i !=  ehdr->e_shstrndx) {
         if (verb) 
            fprintf ( stderr, "   section %d is a normal string table\n", i );
         strtab = ehdrC + shdr[i].sh_offset;
         nstrtab++;
      }
   }  
   if (nstrtab != 1) {
      oc->errMsg("WARNING: no string tables, or too many");
      return FALSE;
   }

   nsymtabs = 0;
   if (verb) fprintf ( stderr, "\n\nSymbol tables\n" ); 
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      if (verb) fprintf ( stderr, "section %d is a symbol table\n", i );
      nsymtabs++;
      stab = (Elf32_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf32_Sym);
      if (verb) fprintf ( stderr, "   number of entries is apparently %d (%d rem)\n",
               nent,
               shdr[i].sh_size % sizeof(Elf32_Sym)
             );
      if (0 != shdr[i].sh_size % sizeof(Elf32_Sym)) {
         oc->errMsg("non-integral number of symbol table entries");
         return FALSE;
      }
      for (j = 0; j < nent; j++) {
         if (verb) fprintf ( stderr, "   %2d  ", j );
         if (verb) fprintf ( stderr, "  sec=%-5d  size=%-3d  val=%-5p  ", 
                             (int)stab[j].st_shndx,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value );

         if (verb) fprintf ( stderr, "type=" );
         switch (ELF32_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  if (verb) fprintf ( stderr, "notype " ); break;
            case STT_OBJECT:  if (verb) fprintf ( stderr, "object " ); break;
            case STT_FUNC  :  if (verb) fprintf ( stderr, "func   " ); break;
            case STT_SECTION: if (verb) fprintf ( stderr, "section" ); break;
            case STT_FILE:    if (verb) fprintf ( stderr, "file   " ); break;
            default:          if (verb) fprintf ( stderr, "?      " ); break;
         }
         if (verb) fprintf ( stderr, "  " );

         if (verb) fprintf ( stderr, "bind=" );
         switch (ELF32_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  if (verb) fprintf ( stderr, "local " ); break;
            case STB_GLOBAL:  if (verb) fprintf ( stderr, "global" ); break;
            case STB_WEAK  :  if (verb) fprintf ( stderr, "weak  " ); break;
            default:          if (verb) fprintf ( stderr, "?     " ); break;
         }
         if (verb) fprintf ( stderr, "  " );

         if (verb) fprintf ( stderr, "name=%s\n", strtab + stab[j].st_name );
      }
   }

   if (nsymtabs == 0) {
      oc->errMsg("Didn't find any symbol tables");
      return FALSE;
   }

   return TRUE;
}


static int ocGetNames_ELF ( ObjectCode* oc, int verb )
{
   int i, j, k, nent;
   Elf32_Sym* stab;

   char*       ehdrC      = (char*)(oc->oImage);
   Elf32_Ehdr* ehdr       = (Elf32_Ehdr*)ehdrC;
   char*       strtab     = findElfSection ( ehdrC, SHT_STRTAB );
   Elf32_Shdr* shdr       = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   char*       sh_strtab  = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;

   if (!strtab) {
      oc->errMsg("ELF: no strtab!");
      return FALSE;
   }

   k = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {

      /* make a HugsDLSection entry for relevant sections */
      OSectionKind kind = HUGS_SECTIONKIND_OTHER;
      if (0==strcmp(".data",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".data1",sh_strtab+shdr[i].sh_name))
         kind = HUGS_SECTIONKIND_RWDATA;
      if (0==strcmp(".text",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".rodata",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".rodata1",sh_strtab+shdr[i].sh_name))
         kind = HUGS_SECTIONKIND_CODE_OR_RODATA;
      if (kind != HUGS_SECTIONKIND_OTHER)
         addSection (
            oc,
            ehdrC + shdr[i].sh_offset, 
            ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1,
            kind
         );

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf32_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf32_Sym);
      for (j = 0; j < nent; j++) {
         if ( ( ELF32_ST_BIND(stab[j].st_info)==STB_GLOBAL ||
                ELF32_ST_BIND(stab[j].st_info)==STB_LOCAL
              )
              &&
              ( ELF32_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF32_ST_TYPE(stab[j].st_info)==STT_OBJECT)
	      /* || ELF32_ST_TYPE(stab[j].st_info)==STT_NOTYPE */
	      ) {
            char* nm = strtab + stab[j].st_name;
            char* ad = ehdrC 
                       + shdr[ stab[j].st_shndx ].sh_offset
                       + stab[j].st_value;
            assert(nm);
            assert(ad);
            if (verb)
               fprintf(stderr, "addOTabName: %10p  %s %s\n",
                       ad, oc->objFileName, nm );
            if (!addSymbol ( oc, nm, ad )) return FALSE;
         }
	 else 
         if (verb)
            fprintf(stderr, "skipping `%s'\n", strtab + stab[j].st_name );
      }
   }

   return TRUE;
}


static int ocResolve_ELF ( ObjectCode* oc, int verb )
{
   char symbol[1000]; // ToDo
   char* strtab;
   int   i, j;
   Elf32_Sym*  stab = NULL;
   char*       ehdrC = (char*)(oc->oImage);
   Elf32_Ehdr* ehdr = (Elf32_Ehdr*) ehdrC;
   Elf32_Shdr* shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   Elf32_Word* targ;

   /* first find "the" symbol table */
   stab = (Elf32_Sym*) findElfSection ( ehdrC, SHT_SYMTAB );

   /* also go find the string table */
   strtab = findElfSection ( ehdrC, SHT_STRTAB );

   if (!stab || !strtab) {
      oc->errMsg("can't find string or symbol table");
      return FALSE; 
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_REL ) {
         Elf32_Rel*  rtab = (Elf32_Rel*) (ehdrC + shdr[i].sh_offset);
         int         nent = shdr[i].sh_size / sizeof(Elf32_Rel);
         int target_shndx = shdr[i].sh_info;
         int symtab_shndx = shdr[i].sh_link;
         stab  = (Elf32_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
         targ  = (Elf32_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
         if (verb)
         fprintf ( stderr,
                  "relocations for section %d using symtab %d\n",
                  target_shndx, symtab_shndx );
         for (j = 0; j < nent; j++) {
            Elf32_Addr offset = rtab[j].r_offset;
            Elf32_Word info   = rtab[j].r_info;

            Elf32_Addr  P = ((Elf32_Addr)targ) + offset;
            Elf32_Word* pP = (Elf32_Word*)P;
            Elf32_Addr  A = *pP;
            Elf32_Addr  S;

            if (verb) fprintf ( stderr, "Rel entry %3d is raw(%6p %6p)   ", 
                                j, (void*)offset, (void*)info );
            if (!info) {
               if (verb) fprintf ( stderr, " ZERO\n" );
               S = 0;
            } else {
               /* First see if it is a nameless local symbol. */
               if (stab[ ELF32_R_SYM(info)].st_name == 0) {
                  if (verb) fprintf ( stderr, "(noname)  ");
                  S = (Elf32_Addr)(ehdrC
                                   + shdr[stab[ELF32_R_SYM(info)].st_shndx ].sh_offset
                                   + stab[ELF32_R_SYM(info)].st_value
                                  );
                  strcpy ( symbol, "(noname)");
               } else {
                  /* No?  Perhaps it's a named symbol in this file. */
                  strcpy ( symbol, strtab+stab[ ELF32_R_SYM(info)].st_name );
                  if (verb) fprintf ( stderr, "`%s'  ", symbol );
                  S = (Elf32_Addr)ocLookupSym ( oc, symbol );
                  if (!S) {
                     /* No?  Ok, too hard.  Hand the problem to the client. 
                        And if that fails, we're outta options.
                     */
                     S = (Elf32_Addr)(oc->clientLookup ( symbol ) );
                  }
               }
               if (verb) fprintf ( stderr, "resolves to %p\n", (void*)S );
               if (!S) {
                  char errtxt[2000];
                  strcpy(errtxt,oc->objFileName);
                  strcat(errtxt,": unresolvable reference to: ");
                  strcat(errtxt,symbol);
                  oc->errMsg(errtxt);
                  return FALSE;
               }
	    }
            /* fprintf ( stderr, "Reloc: P = %p   S = %p   A = %p\n\n",
                         (void*)P, (void*)S, (void*)A ); 
            */
            switch (ELF32_R_TYPE(info)) {
               case R_386_32:   *pP = S + A;     break;
               case R_386_PC32: *pP = S + A - P; break;
               default: fprintf(stderr, 
                                "unhandled ELF relocation type %d\n",
                                ELF32_R_TYPE(info));
                        oc->errMsg("unhandled ELF relocation type");
                        return FALSE;
	    }

         }
      }
      else
      if (shdr[i].sh_type == SHT_RELA) {
         oc->errMsg("RelA style reloc table -- not yet done");
         return FALSE;
      }
   }

   return TRUE;
}


#endif /* defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS) */



/*-------------------------------------------------------------------------*/
