/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "OSMem.h"
#include "RtsUtils.h"
#include "RtsMessages.h"

#if HAVE_WINDOWS_H
#include <windows.h>
#endif

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

static alloc_rec* allocs = NULL;
static block_rec* free_blocks = NULL;

void
osMemInit(void)
{
    allocs = NULL;
    free_blocks = NULL;
}

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
        if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {

            errorBelch("out of memory");
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
    for( ; it!=0 && (it->base+it->size)<=base; it=it->next ) {}
    for( ; it!=0 && size>0; it=it->next ) {
        int size_delta;
        void* temp;
        size_delta = it->size - (base-it->base);
        if(size_delta>size) size_delta=size;
        temp = VirtualAlloc(base, size_delta, MEM_COMMIT, PAGE_READWRITE);
        if(temp==0) {
            sysErrorBelch("getMBlocks: VirtualAlloc MEM_COMMIT failed");
	    stg_exit(EXIT_FAILURE);
	}
        size-=size_delta;
        base+=size_delta;
    }
}

void *
osGetMBlocks(nat n) {
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
    }

    return ret;
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

lnat getPageSize (void)
{
    static lnat pagesize = 0;
    if (pagesize) {
	return pagesize;
    } else {
	SYSTEM_INFO sSysInfo;
	GetSystemInfo(&sSysInfo);
	pagesize = sSysInfo.dwPageSize;
	return pagesize;
    }
}

void setExecutable (void *p, lnat len, rtsBool exec)
{
    DWORD dwOldProtect = 0;
    if (VirtualProtect (p, len, 
			exec ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE, 
			&dwOldProtect) == 0)
    {
	sysErrorBelch("makeExecutable: failed to protect 0x%p; old protection: %lu\n",
                      p, (unsigned long)dwOldProtect);
        stg_exit(EXIT_FAILURE);
    }
}
