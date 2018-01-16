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

#if defined(NEED_SYMBOL_EXTRAS)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)

#include "RtsUtils.h"
#include "sm/OSMem.h"
#include "linker/SymbolExtras.h"
#include "linker/M32Alloc.h"

#include <string.h>
#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif /* RTS_LINKER_USE_MMAP */

/*
  ocAllocateSymbolExtras

  Allocate additional space at the end of the object file image to make room
  for jump islands (powerpc, x86_64, arm) and GOT entries (x86_64).

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

int ocAllocateSymbolExtras( ObjectCode* oc, int count, int first )
{
  size_t n;

  if (RTS_LINKER_USE_MMAP && USE_CONTIGUOUS_MMAP) {
      n = roundUpToPage(oc->fileSize);

      /* Keep image and symbol_extras contiguous */
      void *new = mmapForLinker(n + (sizeof(SymbolExtra) * count),
                                MAP_ANONYMOUS, -1, 0);
      if (new) {
          memcpy(new, oc->image, oc->fileSize);
          if (oc->imageMapped) {
              munmap(oc->image, n);
          }
          oc->image = new;
          oc->imageMapped = true;
          oc->fileSize = n + (sizeof(SymbolExtra) * count);
          oc->symbol_extras = (SymbolExtra *) (oc->image + n);
      }
      else {
          oc->symbol_extras = NULL;
          return 0;
      }
  }
  else if( count > 0 ) {
    if (RTS_LINKER_USE_MMAP) {
        n = roundUpToPage(oc->fileSize);

        oc->symbol_extras = m32_alloc(sizeof(SymbolExtra) * count, 8);
        if (oc->symbol_extras == NULL) return 0;
    }
    else {
        // round up to the nearest 4
        int aligned = (oc->fileSize + 3) & ~3;
        int misalignment = oc->misalignment;

        oc->image -= misalignment;
        oc->image = stgReallocBytes( oc->image,
                                 misalignment +
                                 aligned + sizeof (SymbolExtra) * count,
                                 "ocAllocateSymbolExtras" );
        oc->image += misalignment;

        oc->symbol_extras = (SymbolExtra *) (oc->image + aligned);
    }
  }

  if (oc->symbol_extras != NULL) {
      memset( oc->symbol_extras, 0, sizeof (SymbolExtra) * count );
  }

  oc->first_symbol_extra = first;
  oc->n_symbol_extras = count;

  return 1;
}


#if !defined(arm_HOST_ARCH)
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
    static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF };
    extra->addr = target;
    memcpy(extra->jumpIsland, jmp, 6);
#endif /* x86_64_HOST_ARCH */

    return extra;
}
#endif /* !arm_HOST_ARCH */
#endif /* !x86_64_HOST_ARCH) || !mingw32_HOST_OS */
#endif // NEED_SYMBOL_EXTRAS
