/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "sm/OSMem.h"
#include "sm/HeapAlloc.h"
#include "RtsUtils.h"

#include <windows.h>

typedef struct alloc_rec_ {
    char* base;    // non-aligned base address, directly from VirtualAlloc
    W_ size;       // Size in bytes
    struct alloc_rec_* next;
} alloc_rec;

typedef struct block_rec_ {
    char* base;        // base address, non-MBLOCK-aligned
    W_ size;           // size in bytes
    struct block_rec_* next;
} block_rec;

/* allocs are kept in ascending order, and are the memory regions as
   returned by the OS as we need to have matching VirtualAlloc and
   VirtualFree calls.

   If USE_LARGE_ADDRESS_SPACE is defined, this list will contain only
   one element.
*/
static alloc_rec* allocs = NULL;

/* free_blocks are kept in ascending order, and adjacent blocks are merged */
static block_rec* free_blocks = NULL;

/* Mingw-w64 does not currently have this in their header. So we have to import it.*/
typedef LPVOID(WINAPI *VirtualAllocExNumaProc)(HANDLE, LPVOID, SIZE_T, DWORD, DWORD, DWORD);

/* Cache NUMA API call. */
VirtualAllocExNumaProc VirtualAllocExNuma;

void
osMemInit(void)
{
    allocs = NULL;
    free_blocks = NULL;

    /* Resolve and cache VirtualAllocExNuma. */
    if (osNumaAvailable() && RtsFlags.GcFlags.numa)
    {
        VirtualAllocExNuma = (VirtualAllocExNumaProc)GetProcAddress(GetModuleHandleW(L"kernel32"), "VirtualAllocExNuma");
        if (!VirtualAllocExNuma)
        {
            sysErrorBelch(
                "osBindMBlocksToNode: VirtualAllocExNuma does not exist. How did you get this far?");
        }
    }
}

static
alloc_rec*
allocNew(uint32_t n) {
    alloc_rec* rec;
    rec = (alloc_rec*)stgMallocBytes(sizeof(alloc_rec),"getMBlocks: allocNew");
    rec->size = ((W_)n+1)*MBLOCK_SIZE;
    rec->base =
        VirtualAlloc(NULL, rec->size, MEM_RESERVE, PAGE_READWRITE);
    if(rec->base==0) {
        stgFree((void*)rec);
        rec=0;
        if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {

            errorBelch("Out of memory\n");
            stg_exit(EXIT_HEAPOVERFLOW);
        } else {
            sysErrorBelch(
                "getMBlocks: VirtualAlloc MEM_RESERVE %d blocks failed", n);
        }
    } else {
        alloc_rec temp;
        temp.base=0; temp.size=0; temp.next=allocs;

        alloc_rec* it;
        it=&temp;
        for(; it->next!=0 && it->next->base<rec->base; it=it->next) ;
        rec->next=it->next;
        it->next=rec;

        allocs=temp.next;
    }
    return rec;
}

static
void
insertFree(char* alloc_base, W_ alloc_size) {
    block_rec temp;
    block_rec* it;
    block_rec* prev;

    temp.base=0; temp.size=0; temp.next=free_blocks;
    it = free_blocks;
    prev = &temp;
    for( ; it!=0 && it->base<alloc_base; prev=it, it=it->next) {}

    if(it!=0 && alloc_base+alloc_size == it->base) {
        if(prev->base + prev->size == alloc_base) { /* Merge it, alloc, prev */
            prev->size += alloc_size + it->size;
            prev->next = it->next;
            stgFree(it);
        } else {                                    /* Merge it, alloc */
            it->base = alloc_base;
            it->size += alloc_size;
        }
    } else if(prev->base + prev->size == alloc_base) { /* Merge alloc, prev */
        prev->size += alloc_size;
    } else {                                           /* Merge none */
        block_rec* rec;
        rec = (block_rec*)stgMallocBytes(sizeof(block_rec),
                                         "getMBlocks: insertFree");
        rec->base=alloc_base;
        rec->size=alloc_size;
        rec->next = it;
        prev->next=rec;
    }
    free_blocks=temp.next;
}

static
void*
findFreeBlocks(uint32_t n) {
    void* ret=0;
    block_rec* it;
    block_rec temp;
    block_rec* prev;

    W_ required_size;
    it=free_blocks;
    required_size = n*MBLOCK_SIZE;
    temp.next=free_blocks; temp.base=0; temp.size=0;
    prev=&temp;
    /* TODO: Don't just take first block, find smallest sufficient block */
    for( ; it!=0 && it->size<required_size; prev=it, it=it->next ) {}
    if(it!=0) {
        if( (((W_)it->base) & MBLOCK_MASK) == 0) { /* MBlock aligned */
            ret = (void*)it->base;
            if(it->size==required_size) {
                prev->next=it->next;
                stgFree(it);
            } else {
                it->base += required_size;
                it->size -=required_size;
            }
        } else {
            char* need_base;
            block_rec* next;
            int new_size;
            need_base =
                (char*)(((W_)it->base) & ((W_)~MBLOCK_MASK)) + MBLOCK_SIZE;
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
commitBlocks(char* base, W_ size) {
    alloc_rec* it;
    it=allocs;
    for( ; it!=0 && (it->base+it->size)<=base; it=it->next ) {}
    for( ; it!=0 && size>0; it=it->next ) {
        W_ size_delta;
        void* temp;
        size_delta = it->size - (base-it->base);
        if(size_delta>size) size_delta=size;
        temp = VirtualAlloc(base, size_delta, MEM_COMMIT, PAGE_READWRITE);
        if(temp==0) {
            sysErrorBelch("getMBlocks: VirtualAlloc MEM_COMMIT failed");
            stg_exit(EXIT_HEAPOVERFLOW);
        }
        size-=size_delta;
        base+=size_delta;
    }
}

void *
osGetMBlocks(uint32_t n) {
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

        commitBlocks(ret, (W_)MBLOCK_SIZE*n);
    }

    return ret;
}

static void decommitBlocks(char *addr, W_ nBytes)
{
    alloc_rec *p;

    p = allocs;
    while ((p != NULL) && (addr >= (p->base + p->size))) {
        p = p->next;
    }
    while (nBytes > 0) {
        if ((p == NULL) || (p->base > addr)) {
            errorBelch("Memory to be freed isn't allocated\n");
            stg_exit(EXIT_FAILURE);
        }
        if (p->base + p->size >= addr + nBytes) {
            if (!VirtualFree(addr, nBytes, MEM_DECOMMIT)) {
                sysErrorBelch("osFreeMBlocks: VirtualFree MEM_DECOMMIT failed");
                stg_exit(EXIT_FAILURE);
            }
            nBytes = 0;
        }
        else {
            W_ bytesToFree = p->base + p->size - addr;
            if (!VirtualFree(addr, bytesToFree, MEM_DECOMMIT)) {
                sysErrorBelch("osFreeMBlocks: VirtualFree MEM_DECOMMIT failed");
                stg_exit(EXIT_FAILURE);
            }
            addr += bytesToFree;
            nBytes -= bytesToFree;
            p = p->next;
        }
    }
}

void osFreeMBlocks(void *addr, uint32_t n)
{
    W_ nBytes = (W_)n * MBLOCK_SIZE;

    insertFree(addr, nBytes);
    decommitBlocks(addr, nBytes);
}

void osReleaseFreeMemory(void)
{
    alloc_rec *prev_a, *a;
    alloc_rec head_a;
    block_rec *prev_fb, *fb;
    block_rec head_fb;
    char *a_end, *fb_end;

    /* go through allocs and free_blocks in lockstep, looking for allocs
       that are completely free, and uncommit them */

    head_a.base = 0;
    head_a.size = 0;
    head_a.next = allocs;
    head_fb.base = 0;
    head_fb.size = 0;
    head_fb.next = free_blocks;
    prev_a = &head_a;
    a = allocs;
    prev_fb = &head_fb;
    fb = free_blocks;

    while (a != NULL) {
        a_end = a->base + a->size;
        /* If a is freeable then there is a single freeblock in fb that
           covers it. The end of this free block must be >= the end of
           a, so skip anything in fb that ends before a. */
        while (fb != NULL && fb->base + fb->size < a_end) {
            prev_fb = fb;
            fb = fb->next;
        }

        if (fb == NULL) {
            /* If we have nothing left in fb, then neither a nor
               anything later in the list is freeable, so we are done. */
            break;
        }
        else {
            fb_end = fb->base + fb->size;
            /* We have a candidate fb. But does it really cover a? */
            if (fb->base <= a->base) {
                /* Yes, the alloc is within the free block. Now we need
                   to know if it sticks out at either end. */
                if (fb_end == a_end) {
                    if (fb->base == a->base) {
                        /* fb and a are identical, so just free fb */
                        prev_fb->next = fb->next;
                        stgFree(fb);
                        fb = prev_fb->next;
                    }
                    else {
                        /* fb begins earlier, so truncate it to not include a */
                        fb->size = a->base - fb->base;
                    }
                }
                else {
                    /* fb ends later, so we'll make fb just be the part
                       after a. First though, if it also starts earlier,
                       we make a new free block record for the before bit. */
                    if (fb->base != a->base) {
                        block_rec *new_fb;

                        new_fb =
                            (block_rec *)stgMallocBytes(sizeof(block_rec),
                                                        "osReleaseFreeMemory");
                        new_fb->base = fb->base;
                        new_fb->size = a->base - fb->base;
                        new_fb->next = fb;
                        prev_fb->next = new_fb;
                    }
                    fb->size = fb_end - a_end;
                    fb->base = a_end;
                }
                /* Now we can free the alloc */
                prev_a->next = a->next;
                if(!VirtualFree((void *)a->base, 0, MEM_RELEASE)) {
                    sysErrorBelch("freeAllMBlocks: VirtualFree MEM_RELEASE "
                                  "failed");
                    stg_exit(EXIT_FAILURE);
                }
                stgFree(a);
                a = prev_a->next;
            }
            else {
                /* Otherwise this alloc is not freeable, so go on to the
                   next one */
                prev_a = a;
                a = a->next;
            }
        }
    }

    allocs = head_a.next;
    free_blocks = head_fb.next;
}

void
osFreeAllMBlocks(void)
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
                sysErrorBelch("freeAllMBlocks: VirtualFree MEM_RELEASE failed");
                stg_exit(EXIT_FAILURE);
            }
            next = it->next;
            stgFree(it);
            it=next;
        }
    }
}

size_t getPageSize (void)
{
    static size_t pagesize = 0;

    if (pagesize == 0) {
        SYSTEM_INFO sSysInfo;
        GetSystemInfo(&sSysInfo);
        pagesize = sSysInfo.dwPageSize;
    }

    return pagesize;
}

/* Returns 0 if physical memory size cannot be identified */
StgWord64 getPhysicalMemorySize (void)
{
    static StgWord64 physMemSize = 0;
    if (!physMemSize) {
        MEMORYSTATUSEX status;
        status.dwLength = sizeof(status);
        if (!GlobalMemoryStatusEx(&status)) {
#if defined(DEBUG)
            errorBelch("warning: getPhysicalMemorySize: cannot get physical "
                       "memory size");
#endif
            return 0;
        }
        physMemSize = status.ullTotalPhys;
    }
    return physMemSize;
}

void setExecutable (void *p, W_ len, bool exec)
{
    DWORD dwOldProtect = 0;
    if (VirtualProtect (p, len,
                        exec ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE,
                        &dwOldProtect) == 0)
    {
        sysErrorBelch("setExecutable: failed to protect 0x%p; old protection: "
                      "%lu\n", p, (unsigned long)dwOldProtect);
        stg_exit(EXIT_FAILURE);
    }
}

#ifdef USE_LARGE_ADDRESS_SPACE

static void* heap_base = NULL;

void *osReserveHeapMemory (void *startAddress, W_ *len)
{
    void *start;

    heap_base = VirtualAlloc(startAddress, *len + MBLOCK_SIZE,
                              MEM_RESERVE, PAGE_READWRITE);
    if (heap_base == NULL) {
        if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {
            errorBelch("out of memory");
        } else {
            sysErrorBelch(
                "osReserveHeapMemory: VirtualAlloc MEM_RESERVE %llu bytes \
                at address %p bytes failed",
                len + MBLOCK_SIZE, startAddress);
        }
        stg_exit(EXIT_FAILURE);
    }

    // VirtualFree MEM_RELEASE must always match a
    // previous MEM_RESERVE call, in address and size
    // so we necessarily leak some address space here,
    // before and after the aligned area
    // It is not a huge problem because we never commit
    // that memory
    start = MBLOCK_ROUND_UP(heap_base);

    return start;
}

void osCommitMemory (void *at, W_ size)
{
    void *temp;
    temp = VirtualAlloc(at, size, MEM_COMMIT, PAGE_READWRITE);
    if (temp == NULL) {
        sysErrorBelch("osCommitMemory: VirtualAlloc MEM_COMMIT failed");
        stg_exit(EXIT_FAILURE);
    }
}

void osDecommitMemory (void *at, W_ size)
{
    if (!VirtualFree(at, size, MEM_DECOMMIT)) {
        sysErrorBelch("osDecommitMemory: VirtualFree MEM_DECOMMIT failed");
        stg_exit(EXIT_FAILURE);
    }
}

void osReleaseHeapMemory (void)
{
    VirtualFree(heap_base, 0, MEM_RELEASE);
}

#endif

bool osNumaAvailable(void)
{
    return osNumaNodes() > 1;
}

uint32_t osNumaNodes(void)
{
    /* Cache the amount of NUMA values. */
    static ULONG numNumaNodes = 0;

    /* Cache the amount of NUMA nodes. */
    if (!numNumaNodes && !GetNumaHighestNodeNumber(&numNumaNodes))
    {
        numNumaNodes = 1;
    }

    return numNumaNodes;
}

uint64_t osNumaMask(void)
{
    uint64_t numaMask;
    if (!GetNumaNodeProcessorMask(0, &numaMask))
    {
        return 1;
    }
    return numaMask;
}

void osBindMBlocksToNode(
    void *addr,
    StgWord size,
    uint32_t node)
{
    if (osNumaAvailable())
    {
        void* temp;
        if (RtsFlags.GcFlags.numa) {
            /* Note [base memory]
               I would like to use addr here to specify the base
               memory of allocation. The problem is that the address
               we are requesting is too high. I can't figure out if it's
               because of my NUMA-emulation or a bug in the code.

               On windows also -xb is broken, it does nothing so that can't
               be used to tweak it (see #12577). So for now, just let the OS decide.
            */
            temp = VirtualAllocExNuma(
                          GetCurrentProcess(),
                          NULL, // addr? See base memory
                          size,
                          MEM_RESERVE | MEM_COMMIT,
                          PAGE_READWRITE,
                          node
                        );

            if (!temp) {
                if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {
                    errorBelch("out of memory");
                }
                else {
                    sysErrorBelch(
                        "osBindMBlocksToNode: VirtualAllocExNuma MEM_RESERVE %" FMT_Word " bytes "
                        "at address %p bytes failed",
                                        size, addr);
                }
                stg_exit(EXIT_FAILURE);
            }
        }
    }
}
