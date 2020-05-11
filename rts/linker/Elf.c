#include "Rts.h"

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) \
|| defined(linux_android_HOST_OS) \
|| defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) \
|| defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) \
|| defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)

// It is essential that this is included before any <elf.h> is included. <elf.h>
// defines R_XXX relocations, which would interfere with the COMPAT_R_XXX
// relocations we generate.  E.g. COMPAT_ ## R_ARM_ARM32 would end up as
// const unsigned COMPAT_3 = 0x03; instead of
// const unsigned COMPAT_R_ARM_ARM32 = 0x03;
#include "elf_compat.h"

#include "RtsUtils.h"
#include "RtsSymbolInfo.h"
#include "linker/Elf.h"
#include "linker/CacheFlush.h"
#include "linker/M32Alloc.h"
#include "linker/SymbolExtras.h"
#include "sm/OSMem.h"
#include "GetEnv.h"
#include "linker/util.h"
#include "linker/elf_util.h"

#include <stdlib.h>
#include <string.h>
#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_FCNTL_H)
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
 * One workaround is to use shared Haskell libraries. This is the case
 * when dynamically-linked GHCi is used.
 *
 * Another workaround is to keep the static libraries but compile them
 * with -fPIC -fexternal-dynamic-refs, because that will generate PIC
 * references to data which can be relocated. This is the case when
 * +RTS -xp is passed.
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
#define X86_64_ELF_NONPIC_HACK (!RtsFlags.MiscFlags.linkerAlwaysPic)

#if defined(sparc_HOST_ARCH)
#  define ELF_TARGET_SPARC  /* Used inside <elf.h> */
#elif defined(i386_HOST_ARCH)
#  define ELF_TARGET_386    /* Used inside <elf.h> */
#elif defined(x86_64_HOST_ARCH)
#  define ELF_TARGET_X64_64
#  define ELF_TARGET_AMD64 /* Used inside <elf.h> on Solaris 11 */
#endif

#if !defined(openbsd_HOST_OS)
#  include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#  include <elf_abi.h>
#endif

#if defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)
#  define NEED_GOT
#  define NEED_PLT
#  include "elf_got.h"
#  include "elf_plt.h"
#  include "elf_reloc.h"
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
   Elf_Half shstrndx = ehdr->e_shstrndx;
#if defined(SHN_XINDEX)
   Elf_Shdr* shdr = (Elf_Shdr*) ((char*)ehdr + ehdr->e_shoff);
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
 * ocInit and ocDeinit
 */

void
ocInit_ELF(ObjectCode * oc)
{
    ocDeinit_ELF(oc);

    oc->info = (struct ObjectCodeFormatInfo*)stgCallocBytes(
            1, sizeof *oc->info,
            "ocInit_Elf(ObjectCodeFormatInfo)");
    // TODO: fill info
    oc->info->elfHeader = (Elf_Ehdr *)oc->image;
    oc->info->programHeader = (Elf_Phdr *) ((uint8_t*)oc->image
                                            + oc->info->elfHeader->e_phoff);
    oc->info->sectionHeader = (Elf_Shdr *) ((uint8_t*)oc->image
                                            + oc->info->elfHeader->e_shoff);
    oc->info->sectionHeaderStrtab = (char*)((uint8_t*)oc->image +
            oc->info->sectionHeader[oc->info->elfHeader->e_shstrndx].sh_offset);

    oc->n_sections = elf_shnum(oc->info->elfHeader);

    /* get the symbol table(s) */
    for(int i=0; i < oc->n_sections; i++) {
        if(SHT_REL  == oc->info->sectionHeader[i].sh_type) {
            ElfRelocationTable *relTab = (ElfRelocationTable *)stgCallocBytes(
                    1, sizeof(ElfRelocationTable),
                    "ocInit_Elf(ElfRelocationTable");
            relTab->index = i;

            relTab->relocations =
                (Elf_Rel*) ((uint8_t*)oc->info->elfHeader
                                    + oc->info->sectionHeader[i].sh_offset);
            relTab->n_relocations = oc->info->sectionHeader[i].sh_size
                                    / sizeof(Elf_Rel);
            relTab->targetSectionIndex = oc->info->sectionHeader[i].sh_info;

            relTab->sectionHeader      = &oc->info->sectionHeader[i];

            if(oc->info->relTable == NULL) {
                oc->info->relTable = relTab;
            } else {
                ElfRelocationTable * tail = oc->info->relTable;
                while(tail->next != NULL) tail = tail->next;
                tail->next = relTab;
            }

        } else if(SHT_RELA == oc->info->sectionHeader[i].sh_type) {
            ElfRelocationATable *relTab = (ElfRelocationATable *)stgCallocBytes(
                    1, sizeof(ElfRelocationATable),
                    "ocInit_Elf(ElfRelocationTable");
            relTab->index = i;

            relTab->relocations =
                (Elf_Rela*) ((uint8_t*)oc->info->elfHeader
                                     + oc->info->sectionHeader[i].sh_offset);
            relTab->n_relocations = oc->info->sectionHeader[i].sh_size
                                    / sizeof(Elf_Rela);
            relTab->targetSectionIndex = oc->info->sectionHeader[i].sh_info;

            relTab->sectionHeader      = &oc->info->sectionHeader[i];

            if(oc->info->relaTable == NULL) {
                oc->info->relaTable = relTab;
            } else {
                ElfRelocationATable * tail = oc->info->relaTable;
                while(tail->next != NULL) tail = tail->next;
                tail->next = relTab;
            }

        } else if(SHT_SYMTAB == oc->info->sectionHeader[i].sh_type) {

            ElfSymbolTable *symTab = (ElfSymbolTable *)stgCallocBytes(
                    1, sizeof(ElfSymbolTable),
                    "ocInit_Elf(ElfSymbolTable");

            symTab->index = i; /* store the original index, so we can later
                                * find or assert that we are dealing with the
                                * correct symbol table */

            Elf_Sym *stab = (Elf_Sym*)((uint8_t*)oc->info->elfHeader
                                       + oc->info->sectionHeader[i].sh_offset);
            symTab->n_symbols = oc->info->sectionHeader[i].sh_size
                                / sizeof(Elf_Sym);
            symTab->symbols = (ElfSymbol *)stgCallocBytes(
                    symTab->n_symbols, sizeof(ElfSymbol),
                    "ocInit_Elf(ElfSymbol)");

            /* get the strings table */
            size_t lnkIdx = oc->info->sectionHeader[i].sh_link;
            symTab->names = (char*)(uint8_t*)oc->info->elfHeader
                            + oc->info->sectionHeader[lnkIdx].sh_offset;

            /* build the ElfSymbols from the symbols */
            for(size_t j=0; j < symTab->n_symbols; j++) {

                symTab->symbols[j].name = stab[j].st_name == 0
                                          ? "(noname)"
                                          : symTab->names + stab[j].st_name;
                symTab->symbols[j].elf_sym = &stab[j];
                /* we don't have an address for this symbol yet; this will be
                 * populated during ocGetNames. hence addr = NULL.
                 */
                symTab->symbols[j].addr  = NULL;
                symTab->symbols[j].got_addr = NULL;
            }

            /* append the ElfSymbolTable */
            if(oc->info->symbolTables == NULL) {
                oc->info->symbolTables = symTab;
            } else {
                ElfSymbolTable * tail = oc->info->symbolTables;
                while(tail->next != NULL) tail = tail->next;
                tail->next = symTab;
            }
        }
    }
}

void
ocDeinit_ELF(ObjectCode * oc)
{
    /* free all ElfSymbolTables, and their associated
     * ElfSymbols
     */
    if(oc->info != NULL) {
#if defined(NEED_GOT)
        freeGot(oc);
#endif
        ElfSymbolTable * last = oc->info->symbolTables;

        while(last != NULL) {
            ElfSymbolTable * t = last;
            last = last->next;
            stgFree(t->symbols);
            stgFree(t);
        }

        {
            ElfRelocationTable *last = oc->info->relTable;
            while (last != NULL) {
                ElfRelocationTable *t = last;
                last = last->next;
                stgFree(t);
            }
        }

        {
            ElfRelocationATable *last = oc->info->relaTable;
            while (last != NULL) {
                ElfRelocationATable *t = last;
                last = last->next;
                stgFree(t);
            }
        }

        stgFree(oc->info);
        oc->info = NULL;
    }
}

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
       errorBelch("%s: unknown endianness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      errorBelch("%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   IF_DEBUG(linker, debugBelch( "Is a relocatable object (.o) file\n" ));

   IF_DEBUG(linker,debugBelch( "Architecture is " ));
   switch (ehdr->e_machine) {
#if defined(EM_ARM)
      case EM_ARM:   IF_DEBUG(linker,debugBelch( "arm" )); break;
#endif
      case EM_386:   IF_DEBUG(linker,debugBelch( "x86" )); break;
#if defined(EM_SPARC32PLUS)
      case EM_SPARC32PLUS:
#endif
      case EM_SPARC: IF_DEBUG(linker,debugBelch( "sparc" )); break;
#if defined(EM_IA_64)
      case EM_IA_64: IF_DEBUG(linker,debugBelch( "ia64" )); break;
#endif
      case EM_PPC:   IF_DEBUG(linker,debugBelch( "powerpc32" )); break;
#if defined(EM_PPC64)
      case EM_PPC64: IF_DEBUG(linker,debugBelch( "powerpc64" ));
          errorBelch("%s: RTS linker not implemented on PowerPC 64-bit",
                     oc->fileName);
          return 0;
#endif
#if defined(EM_S390)
      case EM_S390:  IF_DEBUG(linker,debugBelch( "s390" ));
          errorBelch("%s: RTS linker not implemented on s390",
                     oc->fileName);
          return 0;
#endif
#if defined(EM_X86_64)
      case EM_X86_64: IF_DEBUG(linker,debugBelch( "x86_64" )); break;
#elif defined(EM_AMD64)
      case EM_AMD64: IF_DEBUG(linker,debugBelch( "amd64" )); break;
#endif
#if defined(EM_AARCH64)
      case EM_AARCH64: IF_DEBUG(linker,debugBelch( "aarch64" )); break;
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
                         "This object file has probably been fully stripped. "
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
static SectionKind getSectionKind_ELF( Elf_Shdr *hdr, int *is_bss )
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
#if defined(SHT_INIT_ARRAY)
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

#if !defined(NEED_PLT)

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
#endif

int
ocGetNames_ELF ( ObjectCode* oc )
{
   Elf_Word i;
   int result, fd = -1;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;

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
      StgWord align = shdr[i].sh_addralign;

      if (is_bss && size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
#if defined(NEED_GOT) || RTS_LINKER_USE_MMAP
          if (USE_CONTIGUOUS_MMAP || RtsFlags.MiscFlags.linkerAlwaysPic) {
              /* The space for bss sections is already preallocated */
              ASSERT(oc->bssBegin != NULL);
              alloc = SECTION_NOMEM;
              start =
                oc->image + roundUpToAlign(oc->bssBegin - oc->image, align);
              oc->bssBegin = (char*)start + size;
              ASSERT(oc->bssBegin <= oc->bssEnd);
          } else {
              /* Use mmapForLinker to allocate .bss, otherwise the malloced
               * address might be out of range for sections that are mmaped.
               */
              alloc = SECTION_MMAP;
              start = mmapForLinker(size, MAP_ANONYMOUS, -1, 0);
              mapped_start = start;
              mapped_offset = 0;
              mapped_size = roundUpToPage(size);
          }
#else
          alloc = SECTION_MALLOC;
          start = stgCallocBytes(1, size, "ocGetNames_ELF(BSS)");
          mapped_start = start;
#endif
         /*
         debugBelch("BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
         */
          addSection(&sections[i], kind, alloc, start, size,
                     mapped_offset, mapped_start, mapped_size);

          oc->sections[i].info->nstubs = 0;
          oc->sections[i].info->stub_offset = NULL;
          oc->sections[i].info->stub_size = 0;
          oc->sections[i].info->stubs = NULL;
      } else if (kind != SECTIONKIND_OTHER && size > 0) {

#if defined(NEED_PLT)
          /* To support stubs next to sections, we will use the following
           * layout:
           *
           * .--------------.
           * | Section data |
           * |--------------|
           * | Stub space   |
           * '--------------'
           *
           * This ensures that the plt stubs are in range for the section data,
           * Unless the section data exceeds the size for relative jump, in
           * which case I wouldn't know how to solve this, without starting to
           * break up the section itself.
           */

          unsigned nstubs = numberOfStubsForSection(oc, i);
          unsigned stub_space = STUB_SIZE * nstubs;

          void * mem = mmapForLinker(size+stub_space, MAP_ANON, -1, 0);
          if( mem == NULL ) {
              barf("failed to mmap allocated memory to load section %d. "
                   "errno = %d", i, errno);
          }

          /* copy only the image part over; we don't want to copy data
           * into the stub part.
           */
          memcpy( mem, oc->image + offset, size );

          alloc = SECTION_MMAP;

          mapped_offset = 0;
          mapped_size = roundUpToPage(size+stub_space);
          start = mem;
          mapped_start = mem;
#else
          if (USE_CONTIGUOUS_MMAP || RtsFlags.MiscFlags.linkerAlwaysPic) {
              // already mapped.
              start = oc->image + offset;
              alloc = SECTION_NOMEM;
          }
          // use the m32 allocator if either the image is not mapped
          // (i.e. we cannot map the sections separately), or if the section
          // size is small.
          else if (!oc->imageMapped || size < getPageSize() / 3) {
              bool executable = kind == SECTIONKIND_CODE_OR_RODATA;
              m32_allocator *allocator = executable ? oc->rx_m32 : oc->rw_m32;
              start = m32_alloc(allocator, size, 8);
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
#endif
          addSection(&sections[i], kind, alloc, start, size,
                     mapped_offset, mapped_start, mapped_size);

#if defined(NEED_PLT)
          oc->sections[i].info->nstubs = 0;
          oc->sections[i].info->stub_offset = (uint8_t*)mem + size;
          oc->sections[i].info->stub_size = stub_space;
          oc->sections[i].info->stubs = NULL;
#else
          oc->sections[i].info->nstubs = 0;
          oc->sections[i].info->stub_offset = NULL;
          oc->sections[i].info->stub_size = 0;
          oc->sections[i].info->stubs = NULL;
#endif

          addProddableBlock(oc, start, size);
      } else {
          addSection(&oc->sections[i], kind, alloc, oc->image+offset, size,
                     0, 0, 0);
          oc->sections[i].info->nstubs = 0;
          oc->sections[i].info->stub_offset = NULL;
          oc->sections[i].info->stub_size = 0;
          oc->sections[i].info->stubs = NULL;
      }
      oc->sections[i].info->name          = oc->info->sectionHeaderStrtab
                                            + shdr[i].sh_name;
      oc->sections[i].info->sectionHeader = &shdr[i];




      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */

      oc->n_symbols = 0;
      for(ElfSymbolTable *symTab = oc->info->symbolTables;
          symTab != NULL; symTab = symTab->next) {
          oc->n_symbols += symTab->n_symbols;
      }

      oc->symbols = stgCallocBytes(oc->n_symbols, sizeof(Symbol_t),
                                   "ocGetNames_ELF(oc->symbols)");
      // Note calloc: if we fail partway through initializing symbols, we need
      // to undo the additions to the symbol table so far. We know which ones
      // have been added by whether the entry is NULL or not.

      unsigned curSymbol = 0;

      //TODO: we ignore local symbols anyway right? So we can use the
      //      shdr[i].sh_info to get the index of the first non-local symbol
      // ie we should use j = shdr[i].sh_info
       for(ElfSymbolTable *symTab = oc->info->symbolTables;
           symTab != NULL; symTab = symTab->next) {
           for (size_t j = 0; j < symTab->n_symbols; j++) {

               char isLocal = false; /* avoids uninit-var warning */
               HsBool isWeak = HS_BOOL_FALSE;
               SymbolName *nm = symTab->symbols[j].name;
               unsigned short shndx = symTab->symbols[j].elf_sym->st_shndx;

               ElfSymbol *symbol = &symTab->symbols[j];

               Elf_Word secno;


               /* See Note [Many ELF Sections] */
               /* Note that future checks for special SHN_* numbers should check
                * the shndx variable, not the section number in secno. Sections
                * with the real number in the SHN_LORESERVE..HIRESERVE range
                * will have shndx SHN_XINDEX and a secno with one of the
                * reserved values. */
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
                   symbol->addr = stgCallocBytes(1, symbol->elf_sym->st_size,
                                       "ocGetNames_ELF(COMMON)");
                   /*
                   debugBelch("COMMON symbol, size %d name %s\n",
                                   stab[j].st_size, nm);
                   */
                   /* Pointless to do addProddableBlock() for this area,
                      since the linker should never poke around in it. */
               } else if ((ELF_ST_BIND(symbol->elf_sym->st_info) == STB_GLOBAL
                           || ELF_ST_BIND(symbol->elf_sym->st_info) == STB_LOCAL
                           || ELF_ST_BIND(symbol->elf_sym->st_info) == STB_WEAK
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
                          /* and it's a not a section or string table or
                           * anything silly */
                          (ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_FUNC
                          || ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_OBJECT
                          || ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_NOTYPE
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
                   symbol->addr = (SymbolAddr*)(
                           (intptr_t) oc->sections[secno].start +
                           (intptr_t) symbol->elf_sym->st_value);

                   if (ELF_ST_BIND(symbol->elf_sym->st_info) == STB_LOCAL) {
                       isLocal = true;
                       isWeak = false;
                   } else { /* STB_GLOBAL or STB_WEAK */
                       IF_DEBUG(linker,
                                debugBelch("addOTabName(GLOB): %10p  %s %s\n",
                                           symbol->addr, oc->fileName, nm));
                       isLocal = false;
                       isWeak = ELF_ST_BIND(symbol->elf_sym->st_info)
                                == STB_WEAK;
                   }
               }

               /* And the decision is ... */

               if (symbol->addr != NULL) {
                   ASSERT(nm != NULL);
                   /* Acquire! */
                   if (!isLocal) {

                       if (isWeak == HS_BOOL_TRUE) {
                           setWeakSymbol(oc, nm);
                       }
                       if (!ghciInsertSymbolTable(oc->fileName, symhash,
                                                  nm, symbol->addr, isWeak, oc)
                           ) {
                           goto fail;
                       }
                       oc->symbols[curSymbol].name = nm;
                       oc->symbols[curSymbol].addr = symbol->addr;
                       curSymbol++;
                   }
               } else {
                   /* Skip. */
                   IF_DEBUG(linker,
                            debugBelch("skipping `%s'\n",
                                               nm)
                   );

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
   }

#if defined(NEED_GOT)
   if(makeGot( oc ))
       errorBelch("Failed to create GOT for %s",
                  oc->archiveMemberName
                  ? oc->archiveMemberName
                  : oc->fileName);
#endif
   result = 1;
   goto end;

fail:
   result = 0;
   goto end;

end:
   if (fd >= 0) close(fd);
   return result;
}

// the aarch64 linker uses relocacteObjectCodeAarch64,
// see elf_reloc_aarch64.{h,c}
#if !defined(aarch64_HOST_ARCH)

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   and arm-linux relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum )
{
   int j;

   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);

   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   ElfSymbolTable *stab = NULL;
   for(ElfSymbolTable * st = oc->info->symbolTables;
       st != NULL; st = st->next) {
       if((int)st->index == symtab_shndx) {
           stab = st;
           break;
       }
   }
   ASSERT(stab != NULL);

   targ  = (Elf_Word*)oc->sections[target_shndx].start;
   IF_DEBUG(linker,debugBelch(
                "relocations for section %d using symtab %d\n",
                target_shndx, symtab_shndx));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
       IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
       return 1;
   }

   /* The following nomenclature is used for the operation:
    * - S -- (when used on its own) is the address of the symbol.
    * - A -- is the addend for the relocation.
    * - P -- is the address of the place being relocated (derived from r_offset).
    * - Pa - is the adjusted address of the place being relocated, defined as (P & 0xFFFFFFFC).
    * - T -- is 1 if the target symbol S has type STT_FUNC and the symbol addresses a Thumb instruction; it is 0 otherwise.
    * - B(S) is the addressing origin of the output segment defining the symbol S. The origin is not required to be the
    *        base address of the segment. This value must always be word-aligned.
    * - GOT_ORG is the addressing origin of the Global Offset Table (the indirection table for imported data addresses).
    *        This value must always be word-aligned.  See §4.6.1.8, Proxy generating relocations.
    * - GOT(S) is the address of the GOT entry for the symbol S.
    *
    * See the ELF for "ARM Specification" for details:
    * https://developer.arm.com/architectures/system-architectures/software-standards/abi
    */

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
#if defined(i386_HOST_ARCH)
       Elf_Addr  value;
#endif
#if defined(arm_HOST_ARCH)
       int is_target_thm=0, T=0;
#endif

       ElfSymbol * symbol = NULL;

       IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p): ",
                                   j, (void*)offset, (void*)info ));
       if (!info) {
           IF_DEBUG(linker,debugBelch( " ZERO" ));
           S = 0;
       } else {
           symbol = &stab->symbols[ELF_R_SYM(info)];
           /* First see if it is a local symbol. */
           if (ELF_ST_BIND(symbol->elf_sym->st_info) == STB_LOCAL || strncmp(symbol->name, "_GLOBAL_OFFSET_TABLE_", 21) == 0) {
               S = (Elf_Addr)symbol->addr;
           } else {
               S_tmp = lookupDependentSymbol( symbol->name, oc );
               S = (Elf_Addr)S_tmp;
           }
           if (!S) {
               errorBelch("%s: unknown symbol `%s'",
                          oc->fileName, symbol->name);
               return 0;
           }
           IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol->name,
                                       (void*)S ));

#if defined(arm_HOST_ARCH)
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
           if(ELF_ST_TYPE(symbol->elf_sym->st_info) == STT_FUNC) {
               is_target_thm = S & 0x1;
               T = is_target_thm;
               S &= ~1;
           }
#endif
       }

       int reloc_type = ELF_R_TYPE(info);
       IF_DEBUG(linker,debugBelch("Reloc: P = %p   S = %p   A = %p   type=%d\n",
                                  (void*)P, (void*)S, (void*)A, reloc_type ));
       checkProddableBlock ( oc, pP, sizeof(Elf_Word) );

#if defined(i386_HOST_ARCH)
       value = S + A;
#endif

       switch (reloc_type) {
#        if defined(i386_HOST_ARCH)
       case COMPAT_R_386_NONE:                  break;
       case COMPAT_R_386_32:   *pP = value;     break;
       case COMPAT_R_386_PC32: *pP = value - P; break;
#        endif

#        if defined(arm_HOST_ARCH)
       case COMPAT_R_ARM_ABS32:     /* (S + A) | T */
           // Specified by Linux ARM ABI to be equivalent to ABS32
       case COMPAT_R_ARM_TARGET1:
           *(Elf32_Word *)P += S;
           *(Elf32_Word *)P |= T;
           break;

       case COMPAT_R_ARM_REL32:     /* ((S + A) | T) – P */
           *(Elf32_Word *)P += S;
           *(Elf32_Word *)P |= T;
           *(Elf32_Word *)P -= P;
           break;

       case COMPAT_R_ARM_BASE_PREL: /* B(S) + A – P */
       {
           int32_t A = *pP;
           // bfd used to encode sb (B(S)) as 0.
           *(uint32_t *)P += 0 + A - P;
           break;
       }

       case COMPAT_R_ARM_GOT_BREL: /* GOT(S) + A – GOT_ORG */
       {
           int32_t A = *pP;
           void* GOT_S = symbol->got_addr;
           *(uint32_t *)P = (uint32_t) GOT_S + A - (uint32_t) oc->info->got_start;
           break;
       }

       case COMPAT_R_ARM_CALL:
       case COMPAT_R_ARM_JUMP24:
       {
           // N.B. LLVM's LLD linker's relocation implementation is a fantastic
           // resource
           StgWord32 *word = (StgWord32 *)P;
           StgInt32 imm = (*word & ((1<<24)-1)) << 2;

           const StgBool is_blx = (*word & 0xf0000000) == 0xf0000000;
           const StgWord32 hBit = is_blx ? ((*word >> 24) & 1) : 0;
           imm |= hBit << 1;

           // Sign extend to 32 bits
           // I would have thought this would be 24 bits but LLD uses 26 here.
           // Hmm.
           int32_t A = signExtend32(26, imm);

           S = S + A; A = 0;

           StgWord32 result = ((S + A) | T) - P;

           const StgBool overflow = !isInt(26, (StgInt32) result);
           // Handle overflow and Thumb interworking
           const StgBool needs_veneer =
               (is_target_thm && ELF_R_TYPE(info) == COMPAT_R_ARM_JUMP24)
               || overflow;

           if(needs_veneer) { /* overflow or thum interworking */
               // Note [PC bias]
               // From the ELF for the ARM Architecture documentation:
               // > 4.6.1.1 Addends and PC-bias compensation
               // > A binary file may use REL or RELA relocations or a mixture
               // > of the two (but multiple relocations for the same address
               // > must use only one type).
               // > If the relocation is pc-relative then compensation for the
               // > PC bias (the PC value is 8 bytes ahead of the executing
               // > instruction in ARM state and 4 bytes in Thumb state) must
               // > be encoded in the relocation by the object producer.
               int32_t bias = 8;

               S += bias;
               /* try to locate an existing stub for this target */
               if(findStub(&oc->sections[target_shndx], (void**)&S, 0)) {
                   /* didn't find any. Need to create one */
                   if(makeStub(&oc->sections[target_shndx], (void**)&S, 0)) {
                       errorBelch("Unable to create veneer for ARM_CALL\n");
                       return 0;
                   }
               }
               S -= bias;

               result = ((S + A) | T) - P;
               result &= ~1; // Clear thumb indicator bit

               ASSERT(isInt(26, result)); /* X in range */
           }

           // Update the branch target
           const StgWord32 imm24 = (result & 0x03fffffc) >> 2;
           *word = (*word & ~0x00ffffff)
                 | (imm24 & 0x00ffffff);

           // Change the relocated branch into a BLX if necessary
           const StgBool switch_mode =
               is_target_thm && (reloc_type == COMPAT_R_ARM_CALL);
           if (!needs_veneer && switch_mode) {
               const StgWord32 hBit = (result & 0x2) >> 1;
               // Change instruction to BLX
               *word = (*word & ~0xFF000000) | ((0xfa | hBit) << 24);
               IF_DEBUG(linker, debugBelch("Changed BL to BLX at %p\n", word));
           }
           break;
       }

       case COMPAT_R_ARM_MOVT_ABS:
       case COMPAT_R_ARM_MOVW_ABS_NC:
       {
           StgWord32 *word = (StgWord32 *)P;
           StgWord32 imm12 = *word & 0xfff;
           StgWord32 imm4 = (*word >> 16) & 0xf;
           StgInt32 offset = imm4 << 12 | imm12;
           StgWord32 result = (S + offset) | T;

           if (reloc_type == COMPAT_R_ARM_MOVT_ABS)
               result = (result & 0xffff0000) >> 16;

           StgWord32 result12 = result & 0xfff;
           StgWord32 result4 = (result >> 12) & 0xf;
           *word = (*word & ~0xf0fff) | (result4 << 16) | result12;
           break;
       }

       case COMPAT_R_ARM_THM_CALL:
       case COMPAT_R_ARM_THM_JUMP24:
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

           StgInt32 A = (sign << 24)
                        | (i1 << 23)
                        | (i2 << 22)
                        | ((*upper & 0x03ff) << 12)
                        | ((*lower & 0x07ff) << 1);

            // Sign extend 25 to 32 bits
           if (A & 0x01000000)
               A -= 0x02000000;

           S = S + A; A = 0;

           offset = ((S + A) | T) - P;
           overflow = offset <= (StgWord32)0xff000000
                   || offset >= (StgWord32)0x01000000;

           if ((!is_target_thm && ELF_R_TYPE(info) == COMPAT_R_ARM_THM_JUMP24)
               || overflow) {
               // Generate veneer

               // see [PC bias] above.
               int32_t bias = 4;
               S += bias;
               // set the Thumb indicator to S, the final address should
               // carry the correct thumb indicator.
               S |= T;
               /* try to locate an existing stub for this target */
               if(findStub(&oc->sections[target_shndx], (void**)&S, 1)) {
                   /* didn't find any. Need to create one */
                   if(makeStub(&oc->sections[target_shndx], (void**)&S, 1)) {
                       errorBelch("Unable to create veneer for ARM_THM_CALL\n");
                       return 0;
                   }
               }
               S -= bias;

               offset = ((S + A) | T) - P;

               sign = offset >> 31;
               to_thm = 1;
           } else if (!is_target_thm
                      && ELF_R_TYPE(info) == COMPAT_R_ARM_THM_CALL) {
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

       case COMPAT_R_ARM_THM_MOVT_ABS:
       case COMPAT_R_ARM_THM_MOVW_ABS_NC:
       {
           StgWord16 *upper = (StgWord16 *)P;
           StgWord16 *lower = (StgWord16 *)(P + 2);
           StgInt32 offset = ((*upper & 0x000f) << 12)
                           | ((*upper & 0x0400) << 1)
                           | ((*lower & 0x7000) >> 4)
                           | (*lower & 0x00ff);

           offset = (offset ^ 0x8000) - 0x8000; // Sign extend
           offset += S;
           if (ELF_R_TYPE(info) == COMPAT_R_ARM_THM_MOVW_ABS_NC)
               offset |= T;
           else if (ELF_R_TYPE(info) == COMPAT_R_ARM_THM_MOVT_ABS)
               offset >>= 16;

           *upper = ( (*upper & 0xfbf0)
                  | ((offset & 0xf000) >> 12)
                  | ((offset & 0x0800) >> 1) );
           *lower = ( (*lower & 0x8f00)
                  | ((offset & 0x0700) << 4)
                  | (offset & 0x00ff) );
           break;
       }

       case COMPAT_R_ARM_THM_JUMP8:
       {
           StgWord16 *word = (StgWord16 *)P;
           StgWord offset = *word & 0x01fe;
           offset += S - P;
           if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP8 relocation "
                          "not supported\n",
                          oc->fileName);
               return 0;
           }

           *word = (*word & ~0x01fe)
                 | (offset & 0x01fe);
           break;
       }

       case COMPAT_R_ARM_THM_JUMP11:
       {
           StgWord16 *word = (StgWord16 *)P;
           StgWord offset = *word & 0x0ffe;
           offset += S - P;
           if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP11 relocation "
                          "not supported\n",
                          oc->fileName);
               return 0;
           }

           *word = (*word & ~0x0ffe)
                 | (offset & 0x0ffe);
           break;
       }
       case COMPAT_R_ARM_GOT_PREL: {
              int32_t A = *pP;
              void* GOT_S = symbol->got_addr;
              ASSERT(GOT_S);
              *(uint32_t *)P = (uint32_t) GOT_S + A - P;
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
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
   /* This #if def only serves to avoid unused-var warnings. */
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
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
      /* This #if def only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
      Elf_Addr  A      = rtab[j].r_addend;
#endif
#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
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
         } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S_tmp = lookupDependentSymbol( symbol, oc );
            S = (Elf_Addr)S_tmp;
         }
         if (!S) {
           errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
           return 0;
         }
         IF_DEBUG(linker,debugBelch("`%s' resolves to %p\n", symbol, (void*)S));
      }

#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
      IF_DEBUG(linker,debugBelch("Reloc: P = %p   S = %p   A = %p\n",
                                        (void*)P, (void*)S, (void*)A ));
      checkProddableBlock(oc, (void*)P, sizeof(Elf_Word));
#endif

#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
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
            FALLTHROUGH;
         case R_PPC_REL24:
            delta = value - P;

            if( delta << 6 >> 6 != delta )
            {
               value = (Elf_Addr)(&makeSymbolExtra( oc, ELF_R_SYM(info), value )
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

#if defined(x86_64_HOST_ARCH)
      case COMPAT_R_X86_64_NONE:
          break;

      case COMPAT_R_X86_64_64:
      {
          Elf64_Xword payload = value;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_PC32:
      {
          StgInt64 off = value - P;
          if (off != (Elf64_Sword)off && X86_64_ELF_NONPIC_HACK) {
              StgInt64 pltAddress =
                  (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                            -> jumpIsland;
              off = pltAddress + A - P;
          }
          if (off != (Elf64_Sword)off) {
              errorBelch(
                  "R_X86_64_PC32 relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, off, oc->fileName);
              return 0;
          }
          Elf64_Sword payload = off;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_PC64:
      {
          Elf64_Sxword payload = value - P;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_32:
      {
          if (value != (Elf64_Word)value && X86_64_ELF_NONPIC_HACK) {
              StgInt64 pltAddress =
                  (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                            -> jumpIsland;
              value = pltAddress + A;
          }
          if (value != (Elf64_Word)value) {
              errorBelch(
                  "R_X86_64_32 relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, value, oc->fileName);
              return 0;
          }
          Elf64_Word payload = value;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }

      case COMPAT_R_X86_64_32S:
      {
          if ((StgInt64)value != (Elf64_Sword)value && X86_64_ELF_NONPIC_HACK) {
              StgInt64 pltAddress =
                  (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                            -> jumpIsland;
              value = pltAddress + A;
          }
          if ((StgInt64)value != (Elf64_Sword)value) {
              errorBelch(
                  "R_X86_64_32S relocation out of range: %s = %" PRIx64
                  "\nRecompile %s with -fPIC -fexternal-dynamic-refs.",
                  symbol, value, oc->fileName);
              return 0;
          }
          Elf64_Sword payload = value;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }
      case COMPAT_R_X86_64_REX_GOTPCRELX:
      case COMPAT_R_X86_64_GOTPCRELX:
      case COMPAT_R_X86_64_GOTPCREL:
      {
          StgInt64 gotAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)->addr;
          StgInt64 off = gotAddress + A - P;
          if (off != (Elf64_Sword)off) {
              barf(
                  "COMPAT_R_X86_64_GOTPCREL relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
          }
          Elf64_Sword payload = off;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }
#if defined(dragonfly_HOST_OS)
      case COMPAT_R_X86_64_GOTTPOFF:
      {
        /* determine the offset of S to the current thread's tls
           area
           XXX: Move this to the beginning of function */
          struct tls_info ti;
          get_tls_area(0, &ti, sizeof(ti));
          /* make entry in GOT that contains said offset */
          StgInt64 gotEntry = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info),
                                         (S - (Elf64_Addr)(ti.base)))->addr;
          StgInt64 off = gotEntry + A - P;
          if (off != (Elf64_Sword)off) {
              barf(
                  "COMPAT_R_X86_64_GOTTPOFF relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
          }
          Elf64_SWord payload = off;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }
#endif

      case COMPAT_R_X86_64_PLT32:
      {
          StgInt64 off = value - P;
          if (off != (Elf64_Sword)off) {
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
          }
          if (off != (Elf64_Sword)off) {
              barf(
                  "R_X86_64_PLT32 relocation out of range: "
                  "%s = %" PRIx64 " in %s.",
                  symbol, off, oc->fileName);
          }
          Elf64_Sword payload = off;
          memcpy((void*)P, &payload, sizeof(payload));
          break;
      }
#endif

         default:
            barf("%s: unhandled ELF relocation(RelA) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}
#endif /* !aarch64_HOST_ARCH */


static bool
ocMprotect_Elf( ObjectCode *oc )
{
    for(int i=0; i < oc->n_sections; i++) {
        Section *section = &oc->sections[i];
        if(section->size == 0) continue;
        switch (section->kind) {
        case SECTIONKIND_CODE_OR_RODATA:
            if (section->alloc != SECTION_M32) {
                // N.B. m32 handles protection of its allocations during
                // flushing.
                mmapForLinkerMarkExecutable(section->mapped_start, section->mapped_size);
            }
            break;
        default:
            break;
        }
    }

    return true;
}

int
ocResolve_ELF ( ObjectCode* oc )
{
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   const Elf_Word shnum = elf_shnum(ehdr);

#if defined(SHN_XINDEX)
    Elf_Word* shndxTable = get_shndx_table(ehdr);
#endif

    /* resolve section symbols
     * these are special symbols that point to sections, and have no name.
     * Usually there should be one symbol for each text and data section.
     *
     * We need to resolve (assign addresses) to them, to be able to use them
     * during relocation.
     */
    for(ElfSymbolTable *symTab = oc->info->symbolTables;
        symTab != NULL; symTab = symTab->next) {
        for (size_t i = 0; i < symTab->n_symbols; i++) {
            ElfSymbol *symbol = &symTab->symbols[i];
            if(STT_SECTION == ELF_ST_TYPE(symbol->elf_sym->st_info)) {
                /* NOTE: We assume that oc->sections corresponds to the
                 *       sections in the object file.  This is currently true,
                 *       and will stay true, unless we start to compress
                 *       oc->sections by not having an entry for sections we
                 *       are not interested in.
                 */


                /* See Note [Many ELF Sections] */
                /* Note that future checks for special SHN_* numbers should
                 * check the shndx variable, not the section number in secno.
                 * Sections with the real number in the SHN_LORESERVE..HIRESERVE
                 * range will have shndx SHN_XINDEX and a secno with one of the
                 * reserved values.
                 */
                Elf_Word secno = symbol->elf_sym->st_shndx;
#if defined(SHN_XINDEX)
                if (secno == SHN_XINDEX) {
                    ASSERT(shndxTable);
                    secno = shndxTable[i];
                }
#endif
                ASSERT(symbol->elf_sym->st_name == 0);
                ASSERT(symbol->elf_sym->st_value == 0);
                symbol->addr = oc->sections[ secno ].start;
            }
        }
    }

#if defined(NEED_GOT)
    if(fillGot( oc ))
        return 0;
    /* silence warnings */
    (void) shnum;
    (void) shdr;
#endif /* NEED_GOT */

#if defined(aarch64_HOST_ARCH)
    /* use new relocation design */
    if(relocateObjectCode( oc ))
        return 0;
#else
    /* Process the relocation sections. */
    for (Elf_Word i = 0; i < shnum; i++) {
        if (shdr[i].sh_type == SHT_REL) {
          bool ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr, i );
          if (!ok)
              return ok;
        }
        else
        if (shdr[i].sh_type == SHT_RELA) {
          bool ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr, i );
          if (!ok)
              return ok;
        }
    }
#endif

#if defined(powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

    return ocMprotect_Elf(oc);
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

#if defined(NEED_SYMBOL_EXTRAS)

int ocAllocateExtras_ELF( ObjectCode *oc )
{
  Elf_Ehdr *ehdr = (Elf_Ehdr *) oc->image;
  Elf_Shdr* shdr = (Elf_Shdr *) ( ((char *)oc->image) + ehdr->e_shoff );
  Elf_Shdr* symtab = NULL;
  Elf_Word shnum = elf_shnum(ehdr);
  int bssSize = 0;

  for (Elf_Word i = 0; i < shnum; ++i) {
    if(shdr[i].sh_type == SHT_SYMTAB) {
      symtab = &shdr[i];
    } else {
      int isBss = 0;
      getSectionKind_ELF(&shdr[i], &isBss);
      if (isBss && shdr[i].sh_size > 0) {
        bssSize += roundUpToAlign(shdr[i].sh_size, shdr[i].sh_addralign);
      }
    }
  }

  if (symtab == NULL)
  {
    // Not having a symbol table is not in principle a problem.
    // When an object file has no symbols then the 'strip' program
    // typically will remove the symbol table entirely.
    IF_DEBUG(linker, debugBelch( "The ELF file %s contains no symtab\n",
             oc->archiveMemberName ? oc->archiveMemberName : oc->fileName ));
    return 1;
  }

  if( symtab->sh_entsize != sizeof( Elf_Sym ) )
  {
    errorBelch( "The entry size (%d) of the symtab isn't %d\n",
      (int) symtab->sh_entsize, (int) sizeof( Elf_Sym ) );

    return 0;
  }

  return ocAllocateExtras(oc, symtab->sh_size / sizeof( Elf_Sym ), 0, bssSize);
}

#endif /* NEED_SYMBOL_EXTRAS */

#endif /* elf */
