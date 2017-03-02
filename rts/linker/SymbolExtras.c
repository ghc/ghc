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

#if NEED_SYMBOL_EXTRAS
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


#ifndef arm_HOST_ARCH
SymbolExtra* makeSymbolExtra( ObjectCode const* oc,
                              unsigned long symbolNumber,
                              unsigned long target )
{
    SymbolExtra *extra;

    ASSERT( symbolNumber >= oc->first_symbol_extra
            && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);

    extra = &oc->symbol_extras[symbolNumber - oc->first_symbol_extra];

#ifdef powerpc_HOST_ARCH
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
#ifdef x86_64_HOST_ARCH
    // jmp *-14(%rip)
    static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF };
    extra->addr = target;
    memcpy(extra->jumpIsland, jmp, 6);
#endif /* x86_64_HOST_ARCH */

    return extra;
}
#endif

#ifdef arm_HOST_ARCH
/*
  Note [The ARM/Thumb Story]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

  Support for the ARM architecture is complicated by the fact that ARM has not
  one but several instruction encodings. The two relevant ones here are the original
  ARM encoding and Thumb, a more dense variant of ARM supporting only a subset
  of the instruction set.

  How the CPU decodes a particular instruction is determined by a mode bit. This
  mode bit is set on jump instructions, the value being determined by the low
  bit of the target address: An odd address means the target is a procedure
  encoded in the Thumb encoding whereas an even address means it's a traditional
  ARM procedure (the actual address jumped to is even regardless of the encoding bit).

  Interoperation between Thumb- and ARM-encoded object code (known as "interworking")
  is tricky. If the linker needs to link a call by an ARM object into Thumb code
  (or vice-versa) it will produce a jump island using makeArmSymbolExtra. This,
  however, is incompatible with GHC's tables-next-to-code since pointers
  fixed-up in this way will point to a bit of generated code, not a info
  table/Haskell closure like TNTC expects. For this reason, it is critical that
  GHC emit exclusively ARM or Thumb objects for all Haskell code.

  We still do, however, need to worry about calls to foreign code, hence the
  need for makeArmSymbolExtra.
*/

/* Produce a jump island for ARM/Thumb interworking */
SymbolExtra* makeArmSymbolExtra( ObjectCode const* oc,
                                 unsigned long symbolNumber,
                                 unsigned long target,
                                 bool fromThumb,
                                 bool toThumb )
{
  ASSERT( symbolNumber >= oc->first_symbol_extra
        && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);

  SymbolExtra *extra = &oc->symbol_extras[symbolNumber - oc->first_symbol_extra];

  // Make sure instruction mode bit is set properly
  if (toThumb)
    target |= 1;
  else
    target &= ~1;

  if (!fromThumb) {
    // In ARM encoding:
    //   movw r12, #0
    //   movt r12, #0
    //   bx r12
    uint32_t code[] = { 0xe300c000, 0xe340c000, 0xe12fff1c };

    // Patch lower half-word into movw
    code[0] |= ((target>>12) & 0xf) << 16;
    code[0] |= target & 0xfff;
    // Patch upper half-word into movt
    target >>= 16;
    code[1] |= ((target>>12) & 0xf) << 16;
    code[1] |= target & 0xfff;

    memcpy(extra->jumpIsland, code, 12);

  } else {
    // In Thumb encoding:
    //   movw r12, #0
    //   movt r12, #0
    //   bx r12
    uint16_t code[] = { 0xf240,  0x0c00,
                        0xf2c0,  0x0c00,
                        0x4760 };

    // Patch lower half-word into movw
    code[0] |= (target>>12) & 0xf;
    code[0] |= ((target>>11) & 0x1) << 10;
    code[1] |= ((target>>8) & 0x7) << 12;
    code[1] |= target & 0xff;
    // Patch upper half-word into movt
    target >>= 16;
    code[2] |= (target>>12) & 0xf;
    code[2] |= ((target>>11) & 0x1) << 10;
    code[3] |= ((target>>8) & 0x7) << 12;
    code[3] |= target & 0xff;

    memcpy(extra->jumpIsland, code, 10);
  }

  return extra;
}
#endif // arm_HOST_ARCH

#endif
#endif // NEED_SYMBOL_EXTRAS
