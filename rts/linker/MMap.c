#include "Rts.h"

#include "sm/OSMem.h"
#include "linker/MMap.h"
#include "Trace.h"
#include "ReportMemoryMap.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

/* Link objects into the lower 2Gb on x86_64 and AArch64.  GHC assumes the
 * small memory model on this architecture (see gcc docs,
 * -mcmodel=small).
 *
 * MAP_32BIT not available on OpenBSD/amd64
 */
#if defined(MAP_32BIT) && (defined(x86_64_HOST_ARCH) || (defined(aarch64_TARGET_ARCH) || defined(aarch64_HOST_ARCH)))
#define MAP_LOW_MEM
#define TRY_MAP_32BIT MAP_32BIT
#else
#define TRY_MAP_32BIT 0
#endif

/* MAP_ANONYMOUS is MAP_ANON on some systems,
   e.g. OS X (before Sierra), OpenBSD etc */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* In order to simplify control flow a bit, some references to mmap-related
   definitions are blocked off by a C-level if statement rather than a CPP-level
   #if statement. Since those are dead branches when !RTS_LINKER_USE_MMAP, we
   just stub out the relevant symbols here
*/
#if !RTS_LINKER_USE_MMAP
#define munmap(x,y) /* nothing */
#define MAP_ANONYMOUS 0
#endif

void *mmap_32bit_base = LINKER_LOAD_BASE;

static const char *memoryAccessDescription(MemoryAccess mode)
{
  switch (mode) {
  case MEM_NO_ACCESS:    return "no-access";
  case MEM_READ_ONLY:    return "read-only";
  case MEM_READ_WRITE:   return "read-write";
  case MEM_READ_EXECUTE: return "read-execute";
  case MEM_READ_WRITE_EXECUTE:
                         return "read-write-execute";
  default: barf("invalid MemoryAccess");
  }
}

#if defined(mingw32_HOST_OS)

static DWORD
memoryAccessToProt(MemoryAccess access)
{
  switch (access) {
  case MEM_NO_ACCESS:    return PAGE_NOACCESS;
  case MEM_READ_ONLY:    return PAGE_READONLY;
  case MEM_READ_WRITE:   return PAGE_READWRITE;
  case MEM_READ_EXECUTE: return PAGE_EXECUTE_READ;
  case MEM_READ_WRITE_EXECUTE:
                         return PAGE_EXECUTE_READWRITE;
  default: barf("invalid MemoryAccess");
  }
}

//
// Returns NULL on failure.
//
void *
mmapAnonForLinker (size_t bytes)
{
  return VirtualAlloc(NULL, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

void
munmapForLinker (void *addr, size_t bytes, const char *caller)
{
  if (VirtualFree(addr, 0, MEM_RELEASE) == 0) {
    sysErrorBelch("munmapForLinker: %s: Failed to unmap %zd bytes at %p",
                  caller, bytes, addr);
  }
}

/**
 * Change the allowed access modes of a region of memory previously allocated
 * with mmapAnonForLinker.
 */
void
mprotectForLinker(void *start, size_t len, MemoryAccess mode)
{
  DWORD old;
  if (len == 0) {
    return;
  }
  DWORD prot = memoryAccessToProt(mode);

  if (VirtualProtect(start, len, prot, &old) == 0) {
    sysErrorBelch("mprotectForLinker: failed to protect %zd bytes at %p as %s",
                  len, start, memoryAccessDescription(mode));
    ASSERT(false);
  }
}

#elif RTS_LINKER_USE_MMAP

static int
memoryAccessToProt(MemoryAccess access)
{
    switch (access) {
    case MEM_NO_ACCESS:    return 0;
    case MEM_READ_ONLY:    return PROT_READ;
    case MEM_READ_WRITE:   return PROT_READ | PROT_WRITE;
    case MEM_READ_EXECUTE: return PROT_READ | PROT_EXEC;
    case MEM_READ_WRITE_EXECUTE:
                           return PROT_READ | PROT_WRITE | PROT_EXEC;
    default: barf("invalid MemoryAccess");
    }
}

//
// Returns NULL on failure.
//
void *
mmapForLinker (size_t bytes, MemoryAccess access, uint32_t flags, int fd, int offset)
{
   void *map_addr = NULL;
   void *result;
   size_t size;
   uint32_t tryMap32Bit = RtsFlags.MiscFlags.linkerAlwaysPic
     ? 0
     : TRY_MAP_32BIT;
   static uint32_t fixed = 0;
   int prot = memoryAccessToProt(access);

   IF_DEBUG(linker_verbose, debugBelch("mmapForLinker: start\n"));
   size = roundUpToPage(bytes);

#if defined(MAP_LOW_MEM)
mmap_again:
#endif

   if (mmap_32bit_base != NULL) {
       map_addr = mmap_32bit_base;
   }

   IF_DEBUG(linker_verbose,
            debugBelch("mmapForLinker: \tprotection %#0x\n", prot));
   IF_DEBUG(linker_verbose,
            debugBelch("mmapForLinker: \tflags      %#0x\n",
                       MAP_PRIVATE | tryMap32Bit | fixed | flags));
   IF_DEBUG(linker_verbose,
            debugBelch("mmapForLinker: \tsize       %#0zx\n", bytes));
   IF_DEBUG(linker_verbose,
            debugBelch("mmapForLinker: \tmap_addr   %p\n", map_addr));

   result = mmap(map_addr, size, prot,
                 MAP_PRIVATE|tryMap32Bit|fixed|flags, fd, offset);

   if (result == MAP_FAILED) {
       reportMemoryMap();
       sysErrorBelch("mmap %" FMT_Word " bytes at %p",(W_)size,map_addr);
       errorBelch("Try specifying an address with +RTS -xm<addr> -RTS");
       return NULL;
   }

#if defined(MAP_LOW_MEM)
   if (RtsFlags.MiscFlags.linkerAlwaysPic) {
       /* make no attempt at mapping low memory if we are assuming PIC */
   } else if (mmap_32bit_base != NULL) {
       if (result != map_addr) {
           if ((W_)result > 0x80000000) {
               // oops, we were given memory over 2Gb
               munmap(result,size);
#if defined(freebsd_HOST_OS)  || \
    defined(kfreebsdgnu_HOST_OS) || \
    defined(dragonfly_HOST_OS)
               // Some platforms require MAP_FIXED.  This is normally
               // a bad idea, because MAP_FIXED will overwrite
               // existing mappings.
               fixed = MAP_FIXED;
               goto mmap_again;
#else
               reportMemoryMap();
               errorBelch("mmapForLinker: failed to mmap() memory below 2Gb; "
                          "asked for %lu bytes at %p. "
                          "Try specifying an address with +RTS -xm<addr> -RTS",
                          size, map_addr);
               return NULL;
#endif
           } else {
               // hmm, we were given memory somewhere else, but it's
               // still under 2Gb so we can use it.
           }
       }
   } else {
       if ((W_)result > 0x80000000) {
           // oops, we were given memory over 2Gb
           // ... try allocating memory somewhere else?;
           debugTrace(DEBUG_linker,
                      "MAP_32BIT didn't work; gave us %lu bytes at 0x%p",
                      bytes, result);
           munmap(result, size);

           // Set a base address and try again... (guess: 1Gb)
           mmap_32bit_base = (void*)0x40000000;
           goto mmap_again;
       }
   }
#elif (defined(aarch64_TARGET_ARCH) || defined(aarch64_HOST_ARCH))
    // for aarch64 we need to make sure we stay within 4GB of the
    // mmap_32bit_base, and we also do not want to update it.
    if (result != map_addr) {
        // upper limit 4GB - size of the object file - 1mb wiggle room.
        if(llabs((uintptr_t)result - (uintptr_t)&stg_upd_frame_info) > (2<<32) - size - (2<<20)) {
            // not within range :(
            debugTrace(DEBUG_linker,
                        "MAP_32BIT didn't work; gave us %lu bytes at 0x%p",
                        bytes, result);
            munmap(result, size);
            // TODO: some abort/mmap_32bit_base recomputation based on
            //       if mmap_32bit_base is changed, or still at stg_upd_frame_info
            goto mmap_again;
        }
    }
#endif

    if (mmap_32bit_base != NULL) {
       // Next time, ask for memory right after our new mapping to maximize the
       // chance that we get low memory.
        mmap_32bit_base = (void*) ((uintptr_t)result + size);
    }

    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: mapped %" FMT_Word
                        " bytes starting at %p\n", (W_)size, result));
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: done\n"));

    return result;
}

/*
 * Map read/write pages in low memory. Returns NULL on failure.
 */
void *
mmapAnonForLinker (size_t bytes)
{
  return mmapForLinker (bytes, MEM_READ_WRITE, MAP_ANONYMOUS, -1, 0);
}

void munmapForLinker (void *addr, size_t bytes, const char *caller)
{
  int r = munmap(addr, bytes);
  if (r == -1) {
    // Should we abort here?
    sysErrorBelch("munmap: %s", caller);
  }
}

/* Note [Memory protection in the linker]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * For many years the linker would simply map all of its memory
 * with PROT_READ|PROT_WRITE|PROT_EXEC. However operating systems have been
 * becoming increasingly reluctant to accept this practice (e.g. #17353,
 * #12657) and for good reason: writable code is ripe for exploitation.
 *
 * Consequently mmapForLinker now maps its memory with PROT_READ|PROT_WRITE.
 * After the linker has finished filling/relocating the mapping it must then
 * call mprotectForLinker on the sections of the mapping which
 * contain executable code.
 *
 * Note that the m32 allocator handles protection of its allocations. For this
 * reason the caller to m32_alloc() must tell the allocator whether the
 * allocation needs to be executable. The caller must then ensure that they
 * call m32_allocator_flush() after they are finished filling the region, which
 * will cause the allocator to change the protection bits to
 * PROT_READ|PROT_EXEC.
 *
 */

/*
 * Mark an portion of a mapping previously reserved by mmapForLinker
 * as executable (but not writable).
 */
void mprotectForLinker(void *start, size_t len, MemoryAccess mode)
{
    if (len == 0) {
      return;
    }
    IF_DEBUG(linker_verbose,
             debugBelch("mprotectForLinker: protecting %" FMT_Word
                        " bytes starting at %p as %s\n",
                        (W_)len, start, memoryAccessDescription(mode)));

    int prot = memoryAccessToProt(mode);

    if (mprotect(start, len, prot) == -1) {
        sysErrorBelch("mprotectForLinker: failed to protect %zd bytes at %p as %s",
                      len, start, memoryAccessDescription(mode));
    }
}
#endif
