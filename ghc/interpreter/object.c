
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
#include <assert.h>
#include "config.h"                             /* for linux_TARGET_OS etc */
#include "object.h"


#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
static int ocVerifyImage_ELF ( ObjectCode* oc, int verb );
static int ocGetNames_ELF    ( ObjectCode* oc, int verb );
static int ocResolve_ELF     ( ObjectCode* oc, int verb );
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
 * ELF specifics
 * ------------------------------------------------------------------------*/

#define FALSE 0
#define TRUE  1

#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)

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
      oc->errMsg("no strtab!");
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
#if 0
	 else fprintf(stderr, "skipping `%s'\n", strtab + stab[j].st_name );
#endif
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
