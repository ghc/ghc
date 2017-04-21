#include "Rts.h"

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
|| defined(linux_android_HOST_OS) \
|| defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) \
|| defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) \
|| defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)

#include "RtsUtils.h"
#include "RtsSymbolInfo.h"
#include "linker/Elf.h"
#include "linker/CacheFlush.h"
#include "linker/M32Alloc.h"
#include "linker/SymbolExtras.h"
#include "sm/OSMem.h"
#include "GetEnv.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

/* on x86_64 we have a problem with relocating symbol references in
 * code that was compiled without -fPIC.  By default, the small memory
 * model is used, which assumes that symbol references can fit in a
 * 32-bit slot.  The system dynamic linker makes this work for
 * references to shared libraries by either (a) allocating a jump
 * table slot for code references, or (b) moving the symbol at load
 * time (and copying its contents, if necessary) for data references.
 *
 * We unfortunately can't tell whether symbol references are to code
 * or data.  So for now we assume they are code (the vast majority
 * are), and allocate jump-table slots.  Unfortunately this will
 * SILENTLY generate crashing code for data references.  This hack is
 * enabled by X86_64_ELF_NONPIC_HACK.
 *
 * One workaround is to use shared Haskell libraries.  This is
 * coming.  Another workaround is to keep the static libraries but
 * compile them with -fPIC, because that will generate PIC references
 * to data which can be relocated.  The PIC code is still too green to
 * do this systematically, though.
 *
 * See bug #781
 * See thread http://www.haskell.org/pipermail/cvs-ghc/2007-September/038458.html
 *
 * Naming Scheme for Symbol Macros
 *
 * SymI_*: symbol is internal to the RTS. It resides in an object
 *         file/library that is statically.
 * SymE_*: symbol is external to the RTS library. It might be linked
 *         dynamically.
 *
 * Sym*_HasProto  : the symbol prototype is imported in an include file
 *                  or defined explicitly
 * Sym*_NeedsProto: the symbol is undefined and we add a dummy
 *                  default proto extern void sym(void);
 */
#define X86_64_ELF_NONPIC_HACK 1

#if defined(sparc_HOST_ARCH)
#  define ELF_TARGET_SPARC  /* Used inside <elf.h> */
#elif defined(i386_HOST_ARCH)
#  define ELF_TARGET_386    /* Used inside <elf.h> */
#elif defined(x86_64_HOST_ARCH)
#  define ELF_TARGET_X64_64
#  define ELF_64BIT
#  define ELF_TARGET_AMD64 /* Used inside <elf.h> on Solaris 11 */
#elif defined(powerpc64_HOST_ARCH) || defined(powerpc64le_HOST_ARCH)
#  define ELF_64BIT
#elif defined(ia64_HOST_ARCH)
#  define ELF_64BIT
#elif defined(aarch64_HOST_ARCH)
#  define ELF_64BIT
#endif

#if !defined(openbsd_HOST_OS)
#  include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#  include <elf_abi.h>
#  include <machine/reloc.h>
#  define R_386_32    RELOC_32
#  define R_386_PC32  RELOC_PC32
#endif

/* If elf.h doesn't define it */
#  ifndef R_X86_64_PC64
#    define R_X86_64_PC64 24
#  endif

/*
 * Workaround for libc implementations (e.g. eglibc) with incomplete
 * relocation lists
 */
#ifndef R_ARM_THM_CALL
#  define R_ARM_THM_CALL      10
#endif
#ifndef R_ARM_CALL
#  define R_ARM_CALL      28
#endif
#ifndef R_ARM_JUMP24
#  define R_ARM_JUMP24      29
#endif
#ifndef R_ARM_THM_JUMP24
#  define R_ARM_THM_JUMP24      30
#endif
#ifndef R_ARM_TARGET1
#  define R_ARM_TARGET1      38
#endif
#ifndef R_ARM_MOVW_ABS_NC
#  define R_ARM_MOVW_ABS_NC      43
#endif
#ifndef R_ARM_MOVT_ABS
#  define R_ARM_MOVT_ABS      44
#endif
#ifndef R_ARM_THM_MOVW_ABS_NC
#  define R_ARM_THM_MOVW_ABS_NC   47
#endif
#ifndef R_ARM_THM_MOVT_ABS
#  define R_ARM_THM_MOVT_ABS      48
#endif
#ifndef R_ARM_THM_JUMP11
#  define R_ARM_THM_JUMP11      102
#endif
#ifndef R_ARM_THM_JUMP8
#  define R_ARM_THM_JUMP8      103
#endif

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#ifdef ELF_64BIT
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Half    Elf64_Half
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF64_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF64_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF64_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF64_R_SYM
#endif
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Half    Elf32_Half
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF32_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF32_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF32_R_SYM
#endif
#endif


/*
 * Functions to allocate entries in dynamic sections.  Currently we simply
 * preallocate a large number, and we don't check if a entry for the given
 * target already exists (a linear search is too slow).  Ideally these
 * entries would be associated with symbols.
 */

/* These sizes sufficient to load HSbase + HShaskell98 + a few modules */
#define GOT_SIZE            0x20000
#define FUNCTION_TABLE_SIZE 0x10000
#define PLT_SIZE            0x08000

#ifdef ELF_NEED_GOT
static Elf_Addr got[GOT_SIZE];
static unsigned int gotIndex;
static Elf_Addr gp_val = (Elf_Addr)got;

static Elf_Addr
allocateGOTEntry(Elf_Addr target)
{
   Elf_Addr *entry;

   if (gotIndex >= GOT_SIZE)
      barf("Global offset table overflow");

   entry = &got[gotIndex++];
   *entry = target;
   return (Elf_Addr)entry;
}
#endif

#ifdef ELF_FUNCTION_DESC
typedef struct {
   Elf_Addr ip;
   Elf_Addr gp;
} FunctionDesc;

static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
static unsigned int functionTableIndex;

static Elf_Addr
allocateFunctionDesc(Elf_Addr target)
{
   FunctionDesc *entry;

   if (functionTableIndex >= FUNCTION_TABLE_SIZE)
      barf("Function table overflow");

   entry = &functionTable[functionTableIndex++];
   entry->ip = target;
   entry->gp = (Elf_Addr)gp_val;
   return (Elf_Addr)entry;
}

static Elf_Addr
copyFunctionDesc(Elf_Addr target)
{
   FunctionDesc *olddesc = (FunctionDesc *)target;
   FunctionDesc *newdesc;

   newdesc = (FunctionDesc *)allocateFunctionDesc(olddesc->ip);
   newdesc->gp = olddesc->gp;
   return (Elf_Addr)newdesc;
}
#endif

#ifdef ELF_NEED_PLT

typedef struct {
   unsigned char code[sizeof(plt_code)];
} PLTEntry;

static Elf_Addr
allocatePLTEntry(Elf_Addr target, ObjectCode *oc)
{
   PLTEntry *plt = (PLTEntry *)oc->plt;
   PLTEntry *entry;

   if (oc->pltIndex >= PLT_SIZE)
      barf("Procedure table overflow");

   entry = &plt[oc->pltIndex++];
   memcpy(entry->code, plt_code, sizeof(entry->code));
   PLT_RELOC(entry->code, target);
   return (Elf_Addr)entry;
}

static unsigned int
PLTSize(void)
{
   return (PLT_SIZE * sizeof(PLTEntry));
}
#endif

/*

   Note [Many ELF Sections]

   The normal section number fields in ELF are limited to 16 bits, which runs
   out of bits when you try to cram in more sections than that.

   To solve this, the fields e_shnum and e_shstrndx in the ELF header have an
   escape value (different for each case), and the actual section number is
   stashed into unused fields in the first section header.

   For symbols, there seems to have been no place in the actual symbol table
   for the extra bits, so the indexes have been moved into an auxiliary
   section instead.
   For symbols in sections beyond 0xff00, the symbol's st_shndx will be an
   escape value (SHN_XINDEX), and the actual 32-bit section number for symbol N
   is stored at index N in the SHT_SYMTAB_SHNDX table.

   These extensions seem to be undocumented in version 4.1 of the ABI and only
   appear in the drafts for the "next" version:
      https://refspecs.linuxfoundation.org/elf/gabi4+/contents.html

*/

static Elf_Word elf_shnum(Elf_Ehdr* ehdr)
{
   Elf_Shdr* shdr = (Elf_Shdr*) ((char*)ehdr + ehdr->e_shoff);
   Elf_Half shnum = ehdr->e_shnum;
   return shnum != SHN_UNDEF ? shnum : shdr[0].sh_size;
}

static Elf_Word elf_shstrndx(Elf_Ehdr* ehdr)
{
   Elf_Shdr* shdr = (Elf_Shdr*) ((char*)ehdr + ehdr->e_shoff);
   Elf_Half shstrndx = ehdr->e_shstrndx;
#if defined(SHN_XINDEX)
   return shstrndx != SHN_XINDEX ? shstrndx : shdr[0].sh_link;
#else
   // some OSes do not support SHN_XINDEX yet, let's revert to
   // old way
   return shstrndx;
#endif
}

#if defined(SHN_XINDEX)
static Elf_Word*
get_shndx_table(Elf_Ehdr* ehdr)
{
   Elf_Word  i;
   char*     ehdrC    = (char*)ehdr;
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   const Elf_Word shnum = elf_shnum(ehdr);

   for (i = 0; i < shnum; i++) {
     if (shdr[i].sh_type == SHT_SYMTAB_SHNDX) {
       return (Elf32_Word*)(ehdrC + shdr[i].sh_offset);
     }
   }
   return NULL;
}
#endif

/*
 * Generic ELF functions
 */

int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int j, nent, nstrtab, nsymtabs;
   Elf_Word i, shnum, shstrndx;
   char* sh_strtab;

   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      errorBelch("%s: not an ELF object", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS) {
      errorBelch("%s: unsupported ELF format", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
       IF_DEBUG(linker,debugBelch( "Is little-endian\n" ));
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
       IF_DEBUG(linker,debugBelch( "Is big-endian\n" ));
   } else {
       errorBelch("%s: unknown endiannness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      errorBelch("%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   IF_DEBUG(linker, debugBelch( "Is a relocatable object (.o) file\n" ));

   IF_DEBUG(linker,debugBelch( "Architecture is " ));
   switch (ehdr->e_machine) {
#ifdef EM_ARM
      case EM_ARM:   IF_DEBUG(linker,debugBelch( "arm" )); break;
#endif
      case EM_386:   IF_DEBUG(linker,debugBelch( "x86" )); break;
#ifdef EM_SPARC32PLUS
      case EM_SPARC32PLUS:
#endif
      case EM_SPARC: IF_DEBUG(linker,debugBelch( "sparc" )); break;
#ifdef EM_IA_64
      case EM_IA_64: IF_DEBUG(linker,debugBelch( "ia64" )); break;
#endif
      case EM_PPC:   IF_DEBUG(linker,debugBelch( "powerpc32" )); break;
#ifdef EM_PPC64
      case EM_PPC64: IF_DEBUG(linker,debugBelch( "powerpc64" ));
          errorBelch("%s: RTS linker not implemented on PowerPC 64-bit",
                     oc->fileName);
          return 0;
#endif
#ifdef EM_X86_64
      case EM_X86_64: IF_DEBUG(linker,debugBelch( "x86_64" )); break;
#elif defined(EM_AMD64)
      case EM_AMD64: IF_DEBUG(linker,debugBelch( "amd64" )); break;
#endif
      default:       IF_DEBUG(linker,debugBelch( "unknown" ));
                     errorBelch("%s: unknown architecture (e_machine == %d)"
                                , oc->fileName, ehdr->e_machine);
                     return 0;
   }

   shnum = elf_shnum(ehdr);
   IF_DEBUG(linker,debugBelch(
             "\nSection header table: start %ld, n_entries %d, ent_size %d\n",
             (long)ehdr->e_shoff, shnum, ehdr->e_shentsize  ));

   ASSERT(ehdr->e_shentsize == sizeof(Elf_Shdr));

   shdr = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   shstrndx = elf_shstrndx(ehdr);
   if (shstrndx == SHN_UNDEF) {
      errorBelch("%s: no section header string table", oc->fileName);
      return 0;
   } else {
      IF_DEBUG(linker,debugBelch( "Section header string table is section %d\n",
                          shstrndx));
      sh_strtab = ehdrC + shdr[shstrndx].sh_offset;
   }

   for (i = 0; i < shnum; i++) {
      IF_DEBUG(linker,debugBelch("%2d:  ", i ));
      IF_DEBUG(linker,debugBelch("type=%2d  ", (int)shdr[i].sh_type ));
      IF_DEBUG(linker,debugBelch("size=%4d  ", (int)shdr[i].sh_size ));
      IF_DEBUG(linker,debugBelch("offs=%4d  ", (int)shdr[i].sh_offset ));
      IF_DEBUG(linker,debugBelch("  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset,
                      ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1));

#define SECTION_INDEX_VALID(ndx) (ndx > SHN_UNDEF && ndx < shnum)

      switch (shdr[i].sh_type) {

        case SHT_REL:
        case SHT_RELA:
          IF_DEBUG(linker,debugBelch( shdr[i].sh_type == SHT_REL ? "Rel  " : "RelA "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            if (shdr[i].sh_link == SHN_UNDEF)
              errorBelch("\n%s: relocation section #%d has no symbol table\n"
                         "This object file has probably been fully striped. "
                         "Such files cannot be linked.\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            else
              errorBelch("\n%s: relocation section #%d has an invalid link field (%d)\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                         i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_SYMTAB) {
            errorBelch("\n%s: relocation section #%d does not link to a symbol table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            return 0;
          }
          if (!SECTION_INDEX_VALID(shdr[i].sh_info)) {
            errorBelch("\n%s: relocation section #%d has an invalid info field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_info);
            return 0;
          }

          break;
        case SHT_SYMTAB:
          IF_DEBUG(linker,debugBelch("Sym  "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            errorBelch("\n%s: symbol table section #%d has an invalid link field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_STRTAB) {
            errorBelch("\n%s: symbol table section #%d does not link to a string table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);

            return 0;
          }
          break;
        case SHT_STRTAB: IF_DEBUG(linker,debugBelch("Str  ")); break;
        default:         IF_DEBUG(linker,debugBelch("     ")); break;
      }
      if (sh_strtab) {
          IF_DEBUG(linker,debugBelch("sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   IF_DEBUG(linker,debugBelch( "\nString tables\n" ));
   nstrtab = 0;
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != shstrndx
          /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         IF_DEBUG(linker,debugBelch("   section %d is a normal string table\n", i ));
         nstrtab++;
      }
   }
   if (nstrtab == 0) {
      IF_DEBUG(linker,debugBelch("   no normal string tables (potentially, but not necessarily a problem)\n"));
   }
#if defined(SHN_XINDEX)
   Elf_Word* shndxTable = get_shndx_table(ehdr);
#endif
   nsymtabs = 0;
   IF_DEBUG(linker,debugBelch( "Symbol tables\n" ));
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      IF_DEBUG(linker,debugBelch( "section %d is a symbol table\n", i ));
      nsymtabs++;
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);
      IF_DEBUG(linker,debugBelch( "   number of entries is apparently %d (%ld rem)\n",
               nent,
               (long)shdr[i].sh_size % sizeof(Elf_Sym)
             ));
      if (0 != shdr[i].sh_size % sizeof(Elf_Sym)) {
         errorBelch("%s: non-integral number of symbol table entries", oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         Elf_Word secno = stab[j].st_shndx;
#if defined(SHN_XINDEX)
         /* See Note [Many ELF Sections] */
         if (secno == SHN_XINDEX) {
            ASSERT(shndxTable);
            secno = shndxTable[j];
         }
#endif
         IF_DEBUG(linker,debugBelch("   %2d  ", j ));
         IF_DEBUG(linker,debugBelch("  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)secno,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value ));

         IF_DEBUG(linker,debugBelch("type=" ));
         switch (ELF_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  IF_DEBUG(linker,debugBelch("notype " )); break;
            case STT_OBJECT:  IF_DEBUG(linker,debugBelch("object " )); break;
            case STT_FUNC  :  IF_DEBUG(linker,debugBelch("func   " )); break;
            case STT_SECTION: IF_DEBUG(linker,debugBelch("section" )); break;
            case STT_FILE:    IF_DEBUG(linker,debugBelch("file   " )); break;
            default:          IF_DEBUG(linker,debugBelch("?      " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("bind=" ));
         switch (ELF_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  IF_DEBUG(linker,debugBelch("local " )); break;
            case STB_GLOBAL:  IF_DEBUG(linker,debugBelch("global" )); break;
            case STB_WEAK  :  IF_DEBUG(linker,debugBelch("weak  " )); break;
            default:          IF_DEBUG(linker,debugBelch("?     " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("other=%2x ", stab[j].st_other ));
         IF_DEBUG(linker,debugBelch("name=%s [%x]\n",
                        ehdrC + shdr[shdr[i].sh_link].sh_offset
                              + stab[j].st_name, stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
     // Not having a symbol table is not in principle a problem.
     // When an object file has no symbols then the 'strip' program
     // typically will remove the symbol table entirely.
     IF_DEBUG(linker,debugBelch("   no symbol tables (potentially, but not necessarily a problem)\n"));
   }

   return 1;
}

/* Figure out what kind of section it is.  Logic derived from
   Figure 1.14 ("Special Sections") of the ELF document
   ("Portable Formats Specification, Version 1.1"). */
static int getSectionKind_ELF( Elf_Shdr *hdr, int *is_bss )
{
    *is_bss = false;

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_EXECINSTR)) {
        /* .text-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
            && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
            /* .data-style section */
            return SECTIONKIND_RWDATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && !(hdr->sh_flags & SHF_WRITE)) {
        /* .rodata-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }
#ifdef SHT_INIT_ARRAY
    if (hdr->sh_type == SHT_INIT_ARRAY
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
       /* .init_array section */
        return SECTIONKIND_INIT_ARRAY;
    }
#endif /* not SHT_INIT_ARRAY */
    if (hdr->sh_type == SHT_NOBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
        /* .bss-style section */
        *is_bss = true;
        return SECTIONKIND_RWDATA;
    }

    return SECTIONKIND_OTHER;
}

static void *
mapObjectFileSection (int fd, Elf_Word offset, Elf_Word size,
                      void **mapped_start, StgWord *mapped_size,
                      StgWord *mapped_offset)
{
    void *p;
    size_t pageOffset, pageSize;

    pageOffset = roundDownToPage(offset);
    pageSize = roundUpToPage(offset-pageOffset+size);
    p = mmapForLinker(pageSize, 0, fd, pageOffset);
    if (p == NULL) return NULL;
    *mapped_size = pageSize;
    *mapped_offset = pageOffset;
    *mapped_start = p;
    return (void*)((StgWord)p + offset - pageOffset);
}

int
ocGetNames_ELF ( ObjectCode* oc )
{
   Elf_Word i;
   int j, nent, result, fd = -1;
   Elf_Sym* stab;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;
   char*     strtab;
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   Section * sections;
#if defined(SHN_XINDEX)
   Elf_Word* shndxTable = get_shndx_table(ehdr);
#endif
   const Elf_Word shnum = elf_shnum(ehdr);

   ASSERT(symhash != NULL);

   sections = (Section*)stgCallocBytes(sizeof(Section), shnum,
                                       "ocGetNames_ELF(sections)");
   oc->sections = sections;
   oc->n_sections = shnum;


   if (oc->imageMapped) {
#if defined(openbsd_HOST_OS)
       fd = open(oc->fileName, O_RDONLY, S_IRUSR);
#else
       fd = open(oc->fileName, O_RDONLY);
#endif
       if (fd == -1) {
           errorBelch("loadObj: can't open %" PATH_FMT, oc->fileName);
           return 0;
       }
   }

   for (i = 0; i < shnum; i++) {
      int         is_bss = false;
      SectionKind kind   = getSectionKind_ELF(&shdr[i], &is_bss);
      SectionAlloc alloc = SECTION_NOMEM;
      void *start = NULL, *mapped_start = NULL;
      StgWord mapped_size = 0, mapped_offset = 0;
      StgWord size = shdr[i].sh_size;
      StgWord offset = shdr[i].sh_offset;

      if (is_bss && size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
          alloc = SECTION_MALLOC;
          start = stgCallocBytes(1, size, "ocGetNames_ELF(BSS)");
          mapped_start = start;
         /*
         debugBelch("BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
         */
      }

      else if (kind != SECTIONKIND_OTHER && size > 0) {
          if (USE_CONTIGUOUS_MMAP) {
              // already mapped.
              start = oc->image + offset;
              alloc = SECTION_NOMEM;
          }
          // use the m32 allocator if either the image is not mapped
          // (i.e. we cannot map the secions separately), or if the section
          // size is small.
          else if (!oc->imageMapped || size < getPageSize() / 3) {
              start = m32_alloc(size, 8);
              if (start == NULL) goto fail;
              memcpy(start, oc->image + offset, size);
              alloc = SECTION_M32;
          } else {
              start = mapObjectFileSection(fd, offset, size,
                                           &mapped_start, &mapped_size,
                                           &mapped_offset);
              if (start == NULL) goto fail;
              alloc = SECTION_MMAP;
          }
          addProddableBlock(oc, start, size);
      }

      addSection(&sections[i], kind, alloc, start, size,
                 mapped_offset, mapped_start, mapped_size);

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf_Sym*) (ehdrC + offset);
      strtab = ehdrC + shdr[shdr[i].sh_link].sh_offset;
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = stgCallocBytes(oc->n_symbols, sizeof(SymbolName*),
                                   "ocGetNames_ELF(oc->symbols)");
      // Note calloc: if we fail partway through initializing symbols, we need
      // to undo the additions to the symbol table so far. We know which ones
      // have been added by whether the entry is NULL or not.

      //TODO: we ignore local symbols anyway right? So we can use the
      //      shdr[i].sh_info to get the index of the first non-local symbol
      // ie we should use j = shdr[i].sh_info
      for (j = 0; j < nent; j++) {

         char  isLocal  = false; /* avoids uninit-var warning */
         HsBool isWeak  = HS_BOOL_FALSE;
         SymbolAddr* ad  = NULL;
         SymbolName* nm  = strtab + stab[j].st_name;
         unsigned short shndx = stab[j].st_shndx;
         Elf_Word secno;

         /* See Note [Many ELF Sections] */
         /* Note that future checks for special SHN_* numbers should check the
          * shndx variable, not the section number in secno. Sections with the
          * real number in the SHN_LORESERVE..HIRESERVE range will have shndx
          * SHN_XINDEX and a secno with one of the reserved values. */
         secno = shndx;
#if defined(SHN_XINDEX)
         if (shndx == SHN_XINDEX) {
            ASSERT(shndxTable);
            secno = shndxTable[j];
         }
#endif
         /* Figure out if we want to add it; if so, set ad to its
            address.  Otherwise leave ad == NULL. */

         if (shndx == SHN_COMMON) {
            isLocal = false;
            ad = stgCallocBytes(1, stab[j].st_size, "ocGetNames_ELF(COMMON)");
            /*
            debugBelch("COMMON symbol, size %d name %s\n",
                            stab[j].st_size, nm);
            */
            /* Pointless to do addProddableBlock() for this area,
               since the linker should never poke around in it. */
         }
         else
         if ( ( ELF_ST_BIND(stab[j].st_info)==STB_GLOBAL
                || ELF_ST_BIND(stab[j].st_info)==STB_LOCAL
                || ELF_ST_BIND(stab[j].st_info)==STB_WEAK
              )
              /* and not an undefined symbol */
              && shndx != SHN_UNDEF
              /* and not in a "special section" */
              && (shndx < SHN_LORESERVE
#if defined(SHN_XINDEX)
                  || shndx == SHN_XINDEX
#endif
                 )
              &&
              /* and it's a not a section or string table or anything silly */
              ( ELF_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF_ST_TYPE(stab[j].st_info)==STT_NOTYPE
              )
            ) {
            /* Section 0 is the undefined section, hence > and not >=. */
            ASSERT(secno > 0 && secno < shnum);
            /*
            if (shdr[secno].sh_type == SHT_NOBITS) {
               debugBelch("   BSS symbol, size %d off %d name %s\n",
                               stab[j].st_size, stab[j].st_value, nm);
            }
            */
            ad = (SymbolAddr*)((intptr_t)sections[secno].start +
                         (intptr_t)stab[j].st_value);
            if (ELF_ST_BIND(stab[j].st_info)==STB_LOCAL) {
               isLocal = true;
               isWeak = false;
            } else { /* STB_GLOBAL or STB_WEAK */
#ifdef ELF_FUNCTION_DESC
               /* dlsym() and the initialisation table both give us function
                * descriptors, so to be consistent we store function descriptors
                * in the symbol table */
               if (ELF_ST_TYPE(stab[j].st_info) == STT_FUNC)
                   ad = (SymbolAddr*)allocateFunctionDesc((Elf_Addr)ad);
#endif
               IF_DEBUG(linker,debugBelch( "addOTabName(GLOB): %10p  %s %s\n",
                                      ad, oc->fileName, nm ));
               isLocal = false;
               isWeak = (ELF_ST_BIND(stab[j].st_info)==STB_WEAK);
            }
         }

         /* And the decision is ... */

         oc->symbols[j] = nm;

         if (ad != NULL) {
            ASSERT(nm != NULL);
            /* Acquire! */
            if (isLocal) {
                /* Ignore entirely. */
                oc->symbols[j] = NULL;
            } else {

                if (isWeak == HS_BOOL_TRUE) {
                    setWeakSymbol(oc, nm);
                }

                if (! ghciInsertSymbolTable(oc->fileName, symhash,
                                            nm, ad, isWeak, oc)) {
                    goto fail;
                }
            }
         } else {
            /* Skip. */
            IF_DEBUG(linker,debugBelch( "skipping `%s'\n",
                                   nm ));

            /* We're skipping the symbol, but if we ever load this
               object file we'll want to skip it then too. */
            oc->symbols[j] = NULL;

            /*
            debugBelch(
                    "skipping   bind = %d,  type = %d,  secno = %d   `%s'\n",
                    (int)ELF_ST_BIND(stab[j].st_info),
                    (int)ELF_ST_TYPE(stab[j].st_info),
                    (int)secno,
                    nm
                   );
            */
         }

      }
   }

   result = 1;
   goto end;

fail:
   result = 0;
   goto end;

end:
   if (fd >= 0) close(fd);
   return result;
}

#ifdef arm_HOST_ARCH
// TODO: These likely belong in a library somewhere

// Signed extend a number to a 32-bit int.
static inline StgInt32 sign_extend32(uint32_t bits, StgWord32 x) {
    return ((StgInt32) (x << (32 - bits))) >> (32 - bits);
}

// Does the given signed integer fit into the given bit width?
static inline StgBool is_int(uint32_t bits, StgInt32 x) {
    return bits > 32 || (-(1 << (bits-1)) <= x
                         && x < (1 << (bits-1)));
}
#endif

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   and arm-linux relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum )
{
   int j;
   SymbolName* symbol;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
#if defined(SHN_XINDEX)
   Elf_Word* shndx_table = get_shndx_table((Elf_Ehdr*)ehdrC);
#endif

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);
   targ  = (Elf_Word*)oc->sections[target_shndx].start;
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d and strtab %d\n",
                          target_shndx, symtab_shndx, strtab_shndx ));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
           IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
           return 1;
   }

   for (j = 0; j < nent; j++) {
      Elf_Addr offset = rtab[j].r_offset;
      Elf_Addr info   = rtab[j].r_info;

      Elf_Addr  P  = ((Elf_Addr)targ) + offset;
      Elf_Word* pP = (Elf_Word*)P;
#if defined(i386_HOST_ARCH) || defined(DEBUG)
      Elf_Addr  A  = *pP;
#endif
      Elf_Addr  S;
      void*     S_tmp;
#ifdef i386_HOST_ARCH
      Elf_Addr  value;
#endif
#ifdef arm_HOST_ARCH
      int is_target_thm=0, T=0;
#endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p): ",
                             j, (void*)offset, (void*)info ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            /* See Note [Many ELF Sections] */
            Elf_Word secno = sym.st_shndx;
#if defined(SHN_XINDEX)
            if (secno == SHN_XINDEX) {
               ASSERT(shndx_table);
               secno = shndx_table[ELF_R_SYM(info)];
            }
#endif
            S = (Elf_Addr)oc->sections[ secno ].start +
                stab[ELF_R_SYM(info)].st_value;
         } else {
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol_( symbol );
            S = (Elf_Addr)S_tmp;
         }
         if (!S) {
            errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
            return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));

#ifdef arm_HOST_ARCH
          /*
           * 4.5.3 Symbol Values
           *
           * In addition to the normal rules for symbol values the following
           * rules shall also apply to symbols of type STT_FUNC:
           * - If the symbol addresses an ARM instruction, its value is the
           *   address of the instruction (in a relocatable object, the
           *   offset of the instruction from the start of the section
           *   containing it).
           * - If the symbol addresses a Thumb instruction, its value is the
           *   address of the instruction with bit zero set (in a relocatable
           *   object, the section offset with bit zero set).
           * - For the purposes of relocation the value used shall be the
           *   address of the instruction (st_value & ~1).
           *
           *  Note: This allows a linker to distinguish ARM and Thumb code
           *        symbols without having to refer to the map. An ARM symbol
           *        will always have an even value, while a Thumb symbol will
           *        always have an odd value. However, a linker should strip
           *        the discriminating bit from the value before using it for
           *        relocation.
           *
           * (source: ELF for the ARM Architecture
           *          ARM IHI 0044F, current through ABI release 2.10
           *          24th November 2015)
           */
          if(ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
              is_target_thm = S & 0x1;
              T = is_target_thm;
              S &= ~1;
          }
#endif
      }

      int reloc_type = ELF_R_TYPE(info);
      IF_DEBUG(linker,debugBelch( "Reloc: P = %p   S = %p   A = %p   type=%d\n",
                             (void*)P, (void*)S, (void*)A, reloc_type ));
      checkProddableBlock ( oc, pP, sizeof(Elf_Word) );

#ifdef i386_HOST_ARCH
      value = S + A;
#endif

      switch (reloc_type) {
#        ifdef i386_HOST_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif

#        ifdef arm_HOST_ARCH
         case R_ARM_ABS32:
         case R_ARM_TARGET1:  // Specified by Linux ARM ABI to be equivalent to ABS32
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            break;

         case R_ARM_REL32:
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            *(Elf32_Word *)P -= P;
            break;

         case R_ARM_CALL:
         case R_ARM_JUMP24:
         {
            // N.B. LLVM's LLD linker's relocation implement is a fantastic
            // resource
            StgWord32 *word = (StgWord32 *)P;
            StgInt32 imm = (*word & ((1<<24)-1)) << 2;

            const StgBool is_blx = (*word & 0xf0000000) == 0xf0000000;
            const StgWord32 hBit = is_blx ? ((*word >> 24) & 1) : 0;
            imm |= hBit << 1;

            // Sign extend to 32 bits
            // I would have thought this would be 24 bits but LLD uses 26 here.
            // Hmm.
            imm = sign_extend32(26, imm);

            StgWord32 result = ((S + imm) | T) - P;

            const StgBool overflow = !is_int(26, (StgInt32) result);

            // Handle overflow and Thumb interworking
            const StgBool needs_veneer = (is_target_thm && ELF_R_TYPE(info) == R_ARM_JUMP24) || overflow;
            if (needs_veneer) {
               // Generate veneer
               // The +8 below is to undo the PC-bias compensation done by the object producer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+8, 0, is_target_thm);
               // The -8 below is to compensate for PC bias
               result = (StgWord32) ((StgInt32) extra->jumpIsland - P - 8);
               result &= ~1; // Clear thumb indicator bit
               if (!is_int(26, (StgInt32) result)) {
                  errorBelch("Unable to fixup overflow'd R_ARM_CALL: jump island=%p, reloc=%p\n",
                             (void*) extra->jumpIsland, (void*) P);
                  return 0;
               }
            }

            // Update the branch target
            const StgWord32 imm24 = (result & 0x03fffffc) >> 2;
            *word = (*word & ~0x00ffffff)
                  | (imm24 & 0x00ffffff);

            // Change the relocated branch into a BLX if necessary
            const StgBool switch_mode = is_target_thm && (reloc_type == R_ARM_CALL);
            if (!needs_veneer && switch_mode) {
               const StgWord32 hBit = (result & 0x2) >> 1;
               // Change instruction to BLX
               *word = (*word & ~0xFF000000) | ((0xfa | hBit) << 24);
               IF_DEBUG(linker, debugBelch("Changed BL to BLX at %p\n", word));
            }
            break;
         }

         case R_ARM_MOVT_ABS:
         case R_ARM_MOVW_ABS_NC:
         {
            StgWord32 *word = (StgWord32 *)P;
            StgWord32 imm12 = *word & 0xfff;
            StgWord32 imm4 = (*word >> 16) & 0xf;
            StgInt32 offset = imm4 << 12 | imm12;
            StgWord32 result = (S + offset) | T;

            if (reloc_type == R_ARM_MOVT_ABS)
                result = (result & 0xffff0000) >> 16;

            StgWord32 result12 = result & 0xfff;
            StgWord32 result4 = (result >> 12) & 0xf;
            *word = (*word & ~0xf0fff) | (result4 << 16) | result12;
            break;
         }

         case R_ARM_THM_CALL:
         case R_ARM_THM_JUMP24:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);

            int overflow;
            int to_thm = (*lower >> 12) & 1;
            int sign = (*upper >> 10) & 1;
            int j1, j2, i1, i2;

            // Decode immediate value
            j1 = (*lower >> 13) & 1; i1 = ~(j1 ^ sign) & 1;
            j2 = (*lower >> 11) & 1; i2 = ~(j2 ^ sign) & 1;
            StgInt32 imm = (sign << 24)
                         | (i1 << 23)
                         | (i2 << 22)
                         | ((*upper & 0x03ff) << 12)
                         | ((*lower & 0x07ff) << 1);

            // Sign extend 25 to 32 bits
            if (imm & 0x01000000)
               imm -= 0x02000000;

            offset = ((imm + S) | T) - P;
            overflow = offset <= (StgWord32)0xff000000 || offset >= (StgWord32)0x01000000;

            if ((!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_JUMP24) || overflow) {
               // Generate veneer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+4, 1, is_target_thm);
               offset = (StgWord32) &extra->jumpIsland - P - 4;
               sign = offset >> 31;
               to_thm = 1;
            } else if (!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_CALL) {
               offset &= ~0x3;
               to_thm = 0;
            }

            // Reencode instruction
            i1 = ~(offset >> 23) & 1; j1 = sign ^ i1;
            i2 = ~(offset >> 22) & 1; j2 = sign ^ i2;
            *upper = ( (*upper & 0xf800)
                   | (sign << 10)
                   | ((offset >> 12) & 0x03ff) );
            *lower = ( (*lower & 0xd000)
                   | (j1 << 13)
                   | (to_thm << 12)
                   | (j2 << 11)
                   | ((offset >> 1) & 0x07ff) );
            break;
         }

         case R_ARM_THM_MOVT_ABS:
         case R_ARM_THM_MOVW_ABS_NC:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);
            StgInt32 offset = ((*upper & 0x000f) << 12)
                            | ((*upper & 0x0400) << 1)
                            | ((*lower & 0x7000) >> 4)
                            | (*lower & 0x00ff);

            offset = (offset ^ 0x8000) - 0x8000; // Sign extend
            offset += S;
            if (ELF_R_TYPE(info) == R_ARM_THM_MOVW_ABS_NC)
                   offset |= T;
            else if (ELF_R_TYPE(info) == R_ARM_THM_MOVT_ABS)
                   offset >>= 16;

            *upper = ( (*upper & 0xfbf0)
                   | ((offset & 0xf000) >> 12)
                   | ((offset & 0x0800) >> 1) );
            *lower = ( (*lower & 0x8f00)
                   | ((offset & 0x0700) << 4)
                   | (offset & 0x00ff) );
            break;
         }

         case R_ARM_THM_JUMP8:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x01fe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP8 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x01fe)
                  | (offset & 0x01fe);
            break;
         }

         case R_ARM_THM_JUMP11:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x0ffe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP11 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x0ffe)
                  | (offset & 0x0ffe);
            break;
         }

#        endif // arm_HOST_ARCH

         default:
            errorBelch("%s: unhandled ELF relocation(Rel) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum )
{
   int j;
   SymbolName* symbol = NULL;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
   int target_shndx = shdr[shnum].sh_info;
#if defined(SHN_XINDEX)
   Elf_Word* shndx_table = get_shndx_table((Elf_Ehdr*)ehdrC);
#endif
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
   /* This #ifdef only serves to avoid unused-var warnings. */
   Elf_Addr targ = (Elf_Addr) oc->sections[target_shndx].start;
#endif

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);

   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
           IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
           return 1;
   }

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
      Elf_Addr  A      = rtab[j].r_addend;
#endif
#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      Elf_Addr  value;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  S;
      void*     S_tmp;
#     if defined(sparc_HOST_ARCH)
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Word  w1, w2;
#     elif defined(powerpc_HOST_ARCH)
      Elf_Sword delta;
#     endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            /* See Note [Many ELF Sections] */
            Elf_Word secno = sym.st_shndx;
#if defined(SHN_XINDEX)
            if (secno == SHN_XINDEX) {
              secno = shndx_table[ELF_R_SYM(info)];
            }
#endif
            S = (Elf_Addr)oc->sections[secno].start
                + stab[ELF_R_SYM(info)].st_value;
#ifdef ELF_FUNCTION_DESC
            /* Make a function descriptor for this function */
            if (S && ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
               S = allocateFunctionDesc(S + A);
               A = 0;
            }
#endif
         } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol_( symbol );
            S = (Elf_Addr)S_tmp;

#ifdef ELF_FUNCTION_DESC
            /* If a function, already a function descriptor - we would
               have to copy it to add an offset. */
            if (S && (ELF_ST_TYPE(sym.st_info) == STT_FUNC) && (A != 0))
               errorBelch("%s: function %s with addend %p", oc->fileName, symbol, (void *)A);
#endif
         }
         if (!S) {
           errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
           return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));
      }

#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
      IF_DEBUG(linker,debugBelch("Reloc: P = %p   S = %p   A = %p\n",
                                        (void*)P, (void*)S, (void*)A ));
      checkProddableBlock(oc, (void*)P, sizeof(Elf_Word));
#endif

#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      value = S + A;
#endif

      switch (ELF_R_TYPE(info)) {
#        if defined(sparc_HOST_ARCH)
         case R_SPARC_WDISP30:
            w1 = *pP & 0xC0000000;
            w2 = (Elf_Word)((value - P) >> 2);
            ASSERT((w2 & 0xC0000000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_HI22:
            w1 = *pP & 0xFFC00000;
            w2 = (Elf_Word)(value >> 10);
            ASSERT((w2 & 0xFFC00000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_LO10:
            w1 = *pP & ~0x3FF;
            w2 = (Elf_Word)(value & 0x3FF);
            ASSERT((w2 & ~0x3FF) == 0);
            w1 |= w2;
            *pP = w1;
            break;

         /* According to the Sun documentation:
            R_SPARC_UA32
            This relocation type resembles R_SPARC_32, except it refers to an
            unaligned word. That is, the word to be relocated must be treated
            as four separate bytes with arbitrary alignment, not as a word
            aligned according to the architecture requirements.
         */
         case R_SPARC_UA32:
            w2  = (Elf_Word)value;

            // SPARC doesn't do misaligned writes of 32 bit words,
            //       so we have to do this one byte-at-a-time.
            char *pPc   = (char*)pP;
            pPc[0]      = (char) ((Elf_Word)(w2 & 0xff000000) >> 24);
            pPc[1]      = (char) ((Elf_Word)(w2 & 0x00ff0000) >> 16);
            pPc[2]      = (char) ((Elf_Word)(w2 & 0x0000ff00) >> 8);
            pPc[3]      = (char) ((Elf_Word)(w2 & 0x000000ff));
            break;

         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        elif defined(powerpc_HOST_ARCH)
         case R_PPC_ADDR16_LO:
            *(Elf32_Half*) P = value;
            break;

         case R_PPC_ADDR16_HI:
            *(Elf32_Half*) P = value >> 16;
            break;

         case R_PPC_ADDR16_HA:
            *(Elf32_Half*) P = (value + 0x8000) >> 16;
            break;

         case R_PPC_ADDR32:
            *(Elf32_Word *) P = value;
            break;

         case R_PPC_REL32:
            *(Elf32_Word *) P = value - P;
            break;

         case R_PPC_PLTREL24:
            value -= 0x8000; /* See Note [.LCTOC1 in PPC PIC code] */
            /* fallthrough */
         case R_PPC_REL24:
            delta = value - P;

            if( delta << 6 >> 6 != delta )
            {
               value = (Elf_Addr) (&makeSymbolExtra( oc, ELF_R_SYM(info), value )
                                        ->jumpIsland);
               delta = value - P;

               if( value == 0 || delta << 6 >> 6 != delta )
               {
                  barf( "Unable to make SymbolExtra for #%d",
                        ELF_R_SYM(info) );
                  return 0;
               }
            }

            *(Elf_Word *) P = (*(Elf_Word *) P & 0xfc000003)
                                          | (delta & 0x3fffffc);
            break;

         case R_PPC_REL16_LO:
            *(Elf32_Half*) P = value - P;
            break;

         case R_PPC_REL16_HI:
            *(Elf32_Half*) P = (value - P) >> 16;
            break;

         case R_PPC_REL16_HA:
            *(Elf32_Half*) P = (value + 0x8000 - P) >> 16;
            break;
#        endif

#if x86_64_HOST_ARCH
      case R_X86_64_64:
          *(Elf64_Xword *)P = value;
          break;

      case R_X86_64_PC32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PC32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  off = pltAddress + A - P;
              } else {
                  errorBelch("R_X86_64_PC32 relocation out of range: %s = %"
                             PRId64 "d\nRecompile %s with -fPIC.",
                             symbol, off, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }

      case R_X86_64_PC64:
      {
          StgInt64 off = value - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }

      case R_X86_64_32:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32 relocation, but ALWAYS_PIC.");
#else
          if (value >= 0x7fffffffL) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  value = pltAddress + A;
              } else {
                  errorBelch("R_X86_64_32 relocation out of range: %s = %"
                         PRId64 "d\nRecompile %s with -fPIC.",
                         symbol, value, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Word *)P = (Elf64_Word)value;
#endif
          break;

      case R_X86_64_32S:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32S relocation, but ALWAYS_PIC.");
#else
          if ((StgInt64)value > 0x7fffffffL || (StgInt64)value < -0x80000000L) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  value = pltAddress + A;
              } else {
                  errorBelch("R_X86_64_32S relocation out of range: %s = %"
                         PRId64 "d\nRecompile %s with -fPIC.",
                         symbol, value, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Sword *)P = (Elf64_Sword)value;
#endif
          break;
/* These two relocations were introduced in glibc 2.23 and binutils 2.26.
    But in order to use them the system which compiles the bindist for GHC needs
    to have glibc >= 2.23. So only use them if they're defined. */
#if defined(R_X86_64_REX_GOTPCRELX) && defined(R_X86_64_GOTPCRELX)
      case R_X86_64_REX_GOTPCRELX:
      case R_X86_64_GOTPCRELX:
#endif
      case R_X86_64_GOTPCREL:
      {
          StgInt64 gotAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)->addr;
          StgInt64 off = gotAddress + A - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }
#if defined(dragonfly_HOST_OS)
      case R_X86_64_GOTTPOFF:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_GOTTPOFF relocation, but ALWAYS_PIC.");
#else
        /* determine the offset of S to the current thread's tls
           area
           XXX: Move this to the beginning of function */
          struct tls_info ti;
          get_tls_area(0, &ti, sizeof(ti));
          /* make entry in GOT that contains said offset */
          StgInt64 gotEntry = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info),
                                         (S - (Elf64_Addr)(ti.base)))->addr;
          *(Elf64_Word *)P = gotEntry + A - P;
#endif
          break;
      }
#endif



      case R_X86_64_PLT32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PLT32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }
#endif

         default:
            errorBelch("%s: unhandled ELF relocation(RelA) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

int
ocResolve_ELF ( ObjectCode* oc )
{
   int       ok;
   Elf_Word  i;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   const Elf_Word shnum = elf_shnum(ehdr);

   /* Process the relocation sections. */
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type == SHT_REL) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr, i );
         if (!ok) return ok;
      }
      else
      if (shdr[i].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr, i );
         if (!ok) return ok;
      }
   }

#if defined(powerpc_HOST_ARCH) || defined(arm_HOST_ARCH)
   ocFlushInstructionCache( oc );
#endif

   return 1;
}

int ocRunInit_ELF( ObjectCode *oc )
{
   Elf_Word i;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[elf_shstrndx(ehdr)].sh_offset;
   int argc, envc;
   char **argv, **envv;

   getProgArgv(&argc, &argv);
   getProgEnvv(&envc, &envv);

   // XXX Apparently in some archs .init may be something
   // special!  See DL_DT_INIT_ADDRESS macro in glibc
   // as well as ELF_FUNCTION_PTR_IS_SPECIAL.  We've not handled
   // it here, please file a bug report if it affects you.
   for (i = 0; i < elf_shnum(ehdr); i++) {
      init_t *init_start, *init_end, *init;
      int is_bss = false;
      SectionKind kind = getSectionKind_ELF(&shdr[i], &is_bss);
      if (kind == SECTIONKIND_CODE_OR_RODATA
       && 0 == memcmp(".init", sh_strtab + shdr[i].sh_name, 5)) {
          init_t init_f = (init_t)(oc->sections[i].start);
          init_f(argc, argv, envv);
      }

      if (kind == SECTIONKIND_INIT_ARRAY) {
          char *init_startC = oc->sections[i].start;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         for (init = init_start; init < init_end; init++) {
            (*init)(argc, argv, envv);
         }
      }

      // XXX could be more strict and assert that it's
      // SECTIONKIND_RWDATA; but allowing RODATA seems harmless enough.
      if ((kind == SECTIONKIND_RWDATA || kind == SECTIONKIND_CODE_OR_RODATA)
       && 0 == memcmp(".ctors", sh_strtab + shdr[i].sh_name, 6)) {
          char *init_startC = oc->sections[i].start;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         // ctors run in reverse
         for (init = init_end - 1; init >= init_start; init--) {
            (*init)(argc, argv, envv);
         }
      }
   }

   freeProgEnvv(envc, envv);
   return 1;
}

/*
 * PowerPC & X86_64 ELF specifics
 */

#if NEED_SYMBOL_EXTRAS

int ocAllocateSymbolExtras_ELF( ObjectCode *oc )
{
  Elf_Ehdr *ehdr;
  Elf_Shdr* shdr;
  Elf_Word i, shnum;

  ehdr = (Elf_Ehdr *) oc->image;
  shdr = (Elf_Shdr *) ( ((char *)oc->image) + ehdr->e_shoff );

  shnum = elf_shnum(ehdr);

  for( i = 0; i < shnum; i++ )
    if( shdr[i].sh_type == SHT_SYMTAB )
      break;

  if( i == shnum )
  {
    // Not having a symbol table is not in principle a problem.
    // When an object file has no symbols then the 'strip' program
    // typically will remove the symbol table entirely.
    IF_DEBUG(linker, debugBelch( "The ELF file %s contains no symtab\n",
             oc->archiveMemberName ? oc->archiveMemberName : oc->fileName ));
    return 1;
  }

  if( shdr[i].sh_entsize != sizeof( Elf_Sym ) )
  {
    errorBelch( "The entry size (%d) of the symtab isn't %d\n",
      (int) shdr[i].sh_entsize, (int) sizeof( Elf_Sym ) );

    return 0;
  }

  return ocAllocateSymbolExtras( oc, shdr[i].sh_size / sizeof( Elf_Sym ), 0 );
}

#endif /* NEED_SYMBOL_EXTRAS */

#endif /* elf */
