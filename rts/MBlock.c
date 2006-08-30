/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-1999
 *
 * MegaBlock Allocator Interface.  This file contains all the dirty
 * architecture-dependent hackery required to get a chunk of aligned
 * memory from the operating system.
 *
 * ---------------------------------------------------------------------------*/

/* This is non-posix compliant. */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "MBlock.h"
#include "BlockAlloc.h"
#include "Trace.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef mingw32_HOST_OS
# ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
# endif
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if darwin_HOST_OS
#include <mach/vm_map.h>
#endif

#include <errno.h>

lnat mblocks_allocated = 0;

/* -----------------------------------------------------------------------------
   The MBlock Map: provides our implementation of HEAP_ALLOCED()
   -------------------------------------------------------------------------- */

#if SIZEOF_VOID_P == 4
StgWord8 mblock_map[MBLOCK_MAP_SIZE]; // initially all zeros
#elif SIZEOF_VOID_P == 8
static MBlockMap dummy_mblock_map;
MBlockMap *mblock_cache = &dummy_mblock_map;
int mblock_map_count = 0;
MBlockMap **mblock_maps = NULL;

static MBlockMap *
findMBlockMap(void *p)
{
    int i;
    StgWord32 hi = (StgWord32) (((StgWord)p) >> 32);
    for( i = 0; i < mblock_map_count; i++ )
    {
        if(mblock_maps[i]->addrHigh32 == hi)
        {
	    return mblock_maps[i];
	}
    }
    return NULL;
}

StgBool
slowIsHeapAlloced(void *p)
{
    MBlockMap *map = findMBlockMap(p);
    if(map)
    {
    	mblock_cache = map;
	return map->mblocks[MBLOCK_MAP_ENTRY(p)];
    }
    else
    	return 0;
}
#endif

static void
markHeapAlloced(void *p)
{
#if SIZEOF_VOID_P == 4
    mblock_map[MBLOCK_MAP_ENTRY(p)] = 1;
#elif SIZEOF_VOID_P == 8
    MBlockMap *map = findMBlockMap(p);
    if(map == NULL)
    {
    	mblock_map_count++;
    	mblock_maps = realloc(mblock_maps,
			      sizeof(MBlockMap*) * mblock_map_count);
	map = mblock_maps[mblock_map_count-1] = calloc(1,sizeof(MBlockMap));
	map->addrHigh32 = (StgWord32) (((StgWord)p) >> 32);
    }
    map->mblocks[MBLOCK_MAP_ENTRY(p)] = 1;
    mblock_cache = map;
#endif
}

/* -----------------------------------------------------------------------------
   Allocate new mblock(s)
   -------------------------------------------------------------------------- */

void *
getMBlock(void)
{
  return getMBlocks(1);
}

/* -----------------------------------------------------------------------------
   The mmap() method

   On Unix-like systems, we use mmap() to allocate our memory.  We
   want memory in chunks of MBLOCK_SIZE, and aligned on an MBLOCK_SIZE
   boundary.  The mmap() interface doesn't give us this level of
   control, so we have to use some heuristics.

   In the general case, if we want a block of n megablocks, then we
   allocate n+1 and trim off the slop from either side (using
   munmap()) to get an aligned chunk of size n.  However, the next
   time we'll try to allocate directly after the previously allocated
   chunk, on the grounds that this is aligned and likely to be free.
   If it turns out that we were wrong, we have to munmap() and try
   again using the general method.

   Note on posix_memalign(): this interface is available on recent
   systems and appears to provide exactly what we want.  However, it
   turns out not to be as good as our mmap() implementation, because
   it wastes extra space (using double the address space, in a test on
   x86_64/Linux).  The problem seems to be that posix_memalign()
   returns memory that can be free()'d, so the library must store
   extra information along with the allocated block, thus messing up
   the alignment.  Hence, we don't use posix_memalign() for now.

   -------------------------------------------------------------------------- */

#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS)

// A wrapper around mmap(), to abstract away from OS differences in
// the mmap() interface.

static void *
my_mmap (void *addr, lnat size)
{
    void *ret;

#if defined(solaris2_HOST_OS) || defined(irix_HOST_OS)
    { 
	int fd = open("/dev/zero",O_RDONLY);
	ret = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
	close(fd);
    }
#elif hpux_HOST_OS
    ret = mmap(addr, size, PROT_READ | PROT_WRITE, 
	       MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
#elif darwin_HOST_OS
    // Without MAP_FIXED, Apple's mmap ignores addr.
    // With MAP_FIXED, it overwrites already mapped regions, whic
    // mmap(0, ... MAP_FIXED ...) is worst of all: It unmaps the program text
    // and replaces it with zeroes, causing instant death.
    // This behaviour seems to be conformant with IEEE Std 1003.1-2001.
    // Let's just use the underlying Mach Microkernel calls directly,
    // they're much nicer.
    
    kern_return_t err;
    ret = addr;
    if(addr)	// try to allocate at adress
	err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, FALSE);
    if(!addr || err)	// try to allocate anywhere
	err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, TRUE);
	
    if(err) {
	// don't know what the error codes mean exactly, assume it's
	// not our problem though.
	errorBelch("memory allocation failed (requested %lu bytes)", size);
	stg_exit(EXIT_FAILURE);
    } else {
	vm_protect(mach_task_self(),ret,size,FALSE,VM_PROT_READ|VM_PROT_WRITE);
    }
#else
    ret = mmap(addr, size, PROT_READ | PROT_WRITE | PROT_EXEC, 
	       MAP_ANON | MAP_PRIVATE, -1, 0);
#endif

    if (ret == (void *)-1) {
	if (errno == ENOMEM || 
	    (errno == EINVAL && sizeof(void*)==4 && size >= 0xc0000000)) {
	    // If we request more than 3Gig, then we get EINVAL
	    // instead of ENOMEM (at least on Linux).
	    errorBelch("out of memory (requested %lu bytes)", size);
	    stg_exit(EXIT_FAILURE);
	} else {
	    barf("getMBlock: mmap: %s", strerror(errno));
	}
    }

    return ret;
}

// Implements the general case: allocate a chunk of memory of 'size'
// mblocks.

static void *
gen_map_mblocks (lnat size)
{
    int slop;
    void *ret;

    // Try to map a larger block, and take the aligned portion from
    // it (unmap the rest).
    size += MBLOCK_SIZE;
    ret = my_mmap(0, size);
    
    // unmap the slop bits around the chunk we allocated
    slop = (W_)ret & MBLOCK_MASK;
    
    if (munmap(ret, MBLOCK_SIZE - slop) == -1) {
      barf("gen_map_mblocks: munmap failed");
    }
    if (slop > 0 && munmap(ret+size-slop, slop) == -1) {
      barf("gen_map_mblocks: munmap failed");
    }

    // ToDo: if we happened to get an aligned block, then don't
    // unmap the excess, just use it. For this to work, you
    // need to keep in mind the following:
    //     * Calling my_mmap() with an 'addr' arg pointing to
    //       already my_mmap()ed space is OK and won't fail.
    //     * If my_mmap() can't satisfy the request at the
    //       given 'next_request' address in getMBlocks(), that
    //       you unmap the extra mblock mmap()ed here (or simply
    //       satisfy yourself that the slop introduced isn't worth
    //       salvaging.)
    // 

    // next time, try after the block we just got.
    ret += MBLOCK_SIZE - slop;
    return ret;
}


// The external interface: allocate 'n' mblocks, and return the
// address.

void *
getMBlocks(nat n)
{
  static caddr_t next_request = (caddr_t)HEAP_BASE;
  caddr_t ret;
  lnat size = MBLOCK_SIZE * n;
  nat i;
 
  if (next_request == 0) {
      // use gen_map_mblocks the first time.
      ret = gen_map_mblocks(size);
  } else {
      ret = my_mmap(next_request, size);

      if (((W_)ret & MBLOCK_MASK) != 0) {
	  // misaligned block!
#if 0 // defined(DEBUG)
	  errorBelch("warning: getMBlock: misaligned block %p returned when allocating %d megablock(s) at %p", ret, n, next_request);
#endif

	  // unmap this block...
	  if (munmap(ret, size) == -1) {
	      barf("getMBlock: munmap failed");
	  }
	  // and do it the hard way
	  ret = gen_map_mblocks(size);
      }
  }

  // Next time, we'll try to allocate right after the block we just got.
  // ToDo: check that we haven't already grabbed the memory at next_request
  next_request = ret + size;

  debugTrace(DEBUG_gc, "allocated %d megablock(s) at %p",n,ret);

  // fill in the table
  for (i = 0; i < n; i++) {
      markHeapAlloced( ret + i * MBLOCK_SIZE );
  }

  mblocks_allocated += n;

  return ret;
}

void
freeAllMBlocks(void)
{
  /* XXX Do something here */
}

#else /* defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS) */

/* alloc_rec keeps the info we need to have matching VirtualAlloc and
   VirtualFree calls.
*/
typedef struct alloc_rec_ {
    char* base;     /* non-aligned base address, directly from VirtualAlloc */
    int size;       /* Size in bytes */
    struct alloc_rec_* next;
} alloc_rec;

typedef struct block_rec_ {
    char* base;         /* base address, non-MBLOCK-aligned */
    int size;           /* size in bytes */
    struct block_rec_* next;
} block_rec;

static alloc_rec* allocs = 0;
static block_rec* free_blocks = 0;

static
alloc_rec*
allocNew(nat n) {
    alloc_rec* rec;
    rec = (alloc_rec*)stgMallocBytes(sizeof(alloc_rec),"getMBlocks: allocNew");
    rec->size = (n+1)*MBLOCK_SIZE;
    rec->base = 
        VirtualAlloc(NULL, rec->size, MEM_RESERVE, PAGE_READWRITE);
    if(rec->base==0) {
        stgFree((void*)rec);
        rec=0;
        errorBelch(
            "getMBlocks: VirtualAlloc MEM_RESERVE %d blocks failed with: %ld\n"
            , n, GetLastError());
    } else {
        if(allocs==0) {
            allocs=rec;
            rec->next=0;
        } else {
            alloc_rec* it;
            it=allocs;
            for(; it->next!=0 && it->next->base<rec->base; it=it->next) ;
            rec->next=it->next;
            it->next=rec;
        }
        debugTrace(DEBUG_gc, "allocated %d megablock(s) at 0x%x",n,(nat)rec->base);
    }
    return rec;
}

static
void
insertFree(char* alloc_base, int alloc_size) {
    block_rec temp;
    block_rec* it;
    block_rec* prev;

    temp.base=0; temp.size=0; temp.next=free_blocks;
    it = free_blocks;
    prev = &temp;
    for( ; it!=0 && it->base<alloc_base; prev=it, it=it->next) {}

    if(it!=0 && alloc_base+alloc_size == it->base) {
        if(prev->base + prev->size == alloc_base) {        /* Merge it, alloc, prev */
            prev->size += alloc_size + it->size;
            prev->next = it->next;
            stgFree(it);
        } else {                                            /* Merge it, alloc */
            it->base = alloc_base;
            it->size += alloc_size;
        }
    } else if(prev->base + prev->size == alloc_base) {     /* Merge alloc, prev */
        prev->size += alloc_size;
    } else {                                                /* Merge none */
        block_rec* rec;
        rec = (block_rec*)stgMallocBytes(sizeof(block_rec),"getMBlocks: insertFree");
        rec->base=alloc_base;
        rec->size=alloc_size;
        rec->next = it;
        prev->next=rec;
    }
    free_blocks=temp.next;
}

static
void*
findFreeBlocks(nat n) {
    void* ret=0;
    block_rec* it;
    block_rec temp;
    block_rec* prev;

    int required_size;
    it=free_blocks;
    required_size = n*MBLOCK_SIZE;
    temp.next=free_blocks; temp.base=0; temp.size=0;
    prev=&temp;
    /* TODO: Don't just take first block, find smallest sufficient block */
    for( ; it!=0 && it->size<required_size; prev=it, it=it->next ) {}
    if(it!=0) {
        if( (((unsigned long)it->base) & MBLOCK_MASK) == 0) { /* MBlock aligned */
            ret = (void*)it->base;
            if(it->size==required_size) {
                prev->next=0;
                stgFree(it);
            } else {
                it->base += required_size;
                it->size -=required_size;
            }
        } else {
            char* need_base;
            block_rec* next;
            int new_size;
            need_base = (char*)(((unsigned long)it->base) & ((unsigned long)~MBLOCK_MASK)) + MBLOCK_SIZE;
            next = (block_rec*)stgMallocBytes(
                    sizeof(block_rec)
                    , "getMBlocks: findFreeBlocks: splitting");
            new_size = need_base - it->base;
            next->base = need_base +required_size;
            next->size = it->size - (new_size+required_size);
            it->size = new_size;
            next->next = it->next;
            it->next = next;
            ret=(void*)need_base;
        }
    }
    free_blocks=temp.next;
    return ret;
}

/* VirtualAlloc MEM_COMMIT can't cross boundaries of VirtualAlloc MEM_RESERVE,
   so we might need to do many VirtualAlloc MEM_COMMITs.  We simply walk the
   (ordered) allocated blocks. */
static void
commitBlocks(char* base, int size) {
    alloc_rec* it;
    it=allocs;
    for( ; it!=0 && (it->base+it->size)<base; it=it->next ) {}
    for( ; it!=0 && size>0; it=it->next ) {
        int size_delta;
        void* temp;
        size_delta = it->size - (base-it->base);
        if(size_delta>size) size_delta=size;
        temp = VirtualAlloc(base, size_delta, MEM_COMMIT, PAGE_READWRITE);
        if(temp==0) {
            errorBelch("getMBlocks: VirtualAlloc MEM_COMMIT failed: %ld\n", GetLastError());
	    stg_exit(EXIT_FAILURE);
	}
        size-=size_delta;
        base+=size_delta;
    }
}

void *
getMBlocks(nat n) {
    void* ret;
    ret = findFreeBlocks(n);
    if(ret==0) {
        alloc_rec* alloc;
        alloc = allocNew(n);
        /* We already belch in allocNew if it fails */
	if (alloc == 0) {
	    stg_exit(EXIT_FAILURE);
	} else {
            insertFree(alloc->base, alloc->size);
            ret = findFreeBlocks(n);
	}
    }

    if(ret!=0) {
        /* (In)sanity tests */
        if (((W_)ret & MBLOCK_MASK) != 0) {
            barf("getMBlocks: misaligned block returned");
        }

        commitBlocks(ret, MBLOCK_SIZE*n);

        /* Global bookkeeping */
        mblocks_allocated += n;
        int i;
        for(i=0; i<(int)n; ++i) {
            markHeapAlloced( ret + i * MBLOCK_SIZE );
        }
    }

    return ret;
}

void
freeAllMBlocks(void)
{
    {
        block_rec* next;
        block_rec* it;
        next=0;
        it = free_blocks;
        for(; it!=0; ) {
            next = it->next;
            stgFree(it);
            it=next;
        }
    }
    {
        alloc_rec* next;
        alloc_rec* it;
        next=0;
        it=allocs;
        for(; it!=0; ) {
            if(!VirtualFree((void*)it->base, 0, MEM_RELEASE)) {
                errorBelch("freeAllMBlocks: VirtualFree MEM_RELEASE failed with %ld", GetLastError());
		stg_exit(EXIT_FAILURE);
            }
            next = it->next;
            stgFree(it);
            it=next;
        }
    }
}

#endif
