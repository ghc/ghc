/* --------------------------------------------------------------------------
 * Symbol Extras.
 * This is about allocating a small chunk of memory for every symbol in the
 * object file. We make sure that the SymboLExtras are always "in range" of
 * limited-range PC-relative instructions on various platforms by allocating
 * them right next to the object code itself.
 *
 * This implementation is shared by the MachO and ELF implementations. Windows'
 * PEi386 has its own implementation of symbol extras.
 */

#include "LinkerInternals.h"
#include "linker/MMap.h"

#if defined(NEED_SYMBOL_EXTRAS)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)

#include "RtsUtils.h"
#include "sm/OSMem.h"
#include "linker/SymbolExtras.h"
#include "linker/M32Alloc.h"

#if defined(OBJFORMAT_ELF)
#  include "linker/Elf.h"
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachO.h"
#endif

#include <string.h>
#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif /* RTS_LINKER_USE_MMAP */

/*
  ocAllocateExtras

  Allocate additional space at the end of the object file image to make room
  for jump islands (powerpc, x86_64, arm), GOT entries (x86_64) and
  bss sections.

  PowerPC relative branch instructions have a 24 bit displacement field.
  As PPC code is always 4-byte-aligned, this yields a +-32MB range.
  If a particular imported symbol is outside this range, we have to redirect
  the jump to a short piece of new code that just loads the 32bit absolute
  address and jumps there.
  On x86_64, PC-relative jumps and PC-relative accesses to the GOT are limited
  to 32 bits (+-2GB).

  This function just allocates space for one SymbolExtra for every
  undefined symbol in the object file. The code for the jump islands is
  filled in by makeSymbolExtra below.
*/

int ocAllocateExtras(ObjectCode* oc, int count, int first, int bssSize)
{
  void* oldImage = oc->image;
  const size_t extras_size = sizeof(SymbolExtra) * count;

  if (count > 0 || bssSize > 0) {
    if (!RTS_LINKER_USE_MMAP) {
      /* N.B. We currently can't mark symbol extras as non-executable in this
       * case. */

      // round up to the nearest 4
      int aligned = (oc->fileSize + 3) & ~3;
      int misalignment = oc->misalignment;

      oc->image -= misalignment;
      oc->image = stgReallocBytes( oc->image,
                               misalignment +
                               aligned + extras_size,
                               "ocAllocateExtras" );
      oc->image += misalignment;

      oc->symbol_extras = (SymbolExtra *) (oc->image + aligned);
    } else if (USE_CONTIGUOUS_MMAP || RtsFlags.MiscFlags.linkerAlwaysPic) {
      /* Keep image, bssExtras and symbol_extras contiguous */
      /* N.B. We currently can't mark symbol extras as non-executable in this
       * case. */
      size_t n = roundUpToPage(oc->fileSize);
      // round bssSize up to the nearest page size since we need to ensure that
      // symbol_extras is aligned to a page boundary so it can be mprotect'd.
      bssSize = roundUpToPage(bssSize);
      size_t allocated_size = n + bssSize + extras_size;
      void *new = mmapAnonForLinker(allocated_size);
      if (new) {
          memcpy(new, oc->image, oc->fileSize);
          if (oc->imageMapped) {
              munmapForLinker(oc->image, n, "ocAllocateExtras");
          }
          oc->image = new;
          oc->imageMapped = true;
          oc->fileSize = allocated_size;
          oc->symbol_extras = (SymbolExtra *) (oc->image + n + bssSize);
          oc->bssBegin = oc->image + n;
          oc->bssEnd = oc->image + n + bssSize;
      }
      else {
          oc->symbol_extras = NULL;
          return 0;
      }
    } else {
        /* m32_allocator_flush ensures that these are marked as executable when
         * we finish building them. */
        oc->symbol_extras = m32_alloc(oc->rx_m32, extras_size, 8);
        if (oc->symbol_extras == NULL) return 0;
    }
  }

  if (oc->symbol_extras != NULL) {
      memset( oc->symbol_extras, 0, extras_size );
  }

  // ObjectCodeFormatInfo contains computed addresses based on offset to
  // image, if the address of image changes, we need to invalidate
  // the ObjectCodeFormatInfo and recompute it.
  if (oc->image != oldImage) {
#if defined(OBJFORMAT_MACHO)
    ocInit_MachO( oc );
#endif
#if defined(OBJFORMAT_ELF)
    ocInit_ELF( oc );
#endif
  }

  oc->first_symbol_extra = first;
  oc->n_symbol_extras = count;

  return 1;
}

/**
 * Mark the symbol extras as non-writable.
 */
void ocProtectExtras(ObjectCode* oc)
{
  if (oc->n_symbol_extras == 0) return;

  if (!RTS_LINKER_USE_MMAP) {
    /*
     * In this case the symbol extras were allocated via malloc. All we can do
     * in this case is hope that the platform doesn't mark such allocations as
     * non-executable.
     */
  } else if (USE_CONTIGUOUS_MMAP || RtsFlags.MiscFlags.linkerAlwaysPic) {
    mprotectForLinker(oc->symbol_extras, sizeof(SymbolExtra) * oc->n_symbol_extras, MEM_READ_EXECUTE);
  } else {
    /*
     * The symbol extras were allocated via m32. They will be protected when
     * the m32 allocator is finalized
     */
  }
}


#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(riscv64_HOST_ARCH)
SymbolExtra* makeSymbolExtra( ObjectCode const* oc,
                              unsigned long symbolNumber,
                              unsigned long target )
{
    SymbolExtra *extra;

    ASSERT( symbolNumber >= oc->first_symbol_extra
            && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);

    extra = &oc->symbol_extras[symbolNumber - oc->first_symbol_extra];

#if defined(powerpc_HOST_ARCH)
    // lis r12, hi16(target)
    extra->jumpIsland.lis_r12     = 0x3d80;
    extra->jumpIsland.hi_addr     = target >> 16;

    // ori r12, r12, lo16(target)
    extra->jumpIsland.ori_r12_r12 = 0x618c;
    extra->jumpIsland.lo_addr     = target & 0xffff;

    // mtctr r12
    extra->jumpIsland.mtctr_r12   = 0x7d8903a6;

    // bctr
    extra->jumpIsland.bctr        = 0x4e800420;
#endif /* powerpc_HOST_ARCH */
#if defined(x86_64_HOST_ARCH)
    // jmp *-14(%rip)
    // 0xFF 25 is opcode + ModRM of near absolute indirect jump
    // Two bytes trailing padding, needed for TLSGD GOT entries
    // See Note [TLSGD relocation] in elf_tlsgd.c
    static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF, 0x00, 0x00 };
    extra->addr = target;
    memcpy(extra->jumpIsland, jmp, 8);
#endif /* x86_64_HOST_ARCH */
#if defined(riscv64_HOST_ARCH)
    // Fake GOT entry (used like GOT, but located in symbol extras)
    extra->addr = target;
#endif
    return extra;
}
#endif /* powerpc_HOST_ARCH || x86_64_HOST_ARCH || riscv64_HOST_ARCH */
#endif /* !x86_64_HOST_ARCH) || !mingw32_HOST_OS */
#endif // NEED_SYMBOL_EXTRAS
