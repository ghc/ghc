#include "Rts.h"
#include "RtsAPI.h"
#include "list_threads_and_misc_roots_c.h"

static int tsoCount = 0;
static StgTSO** tsos;

static int miscRootsCount = 0;
static StgClosure** miscRoots;

void collectTSOsCallback(void *user, StgTSO* tso){
    tsoCount++;
    tsos = realloc(tsos, sizeof(StgTSO*) * tsoCount);
    tsos[tsoCount - 1] = tso;
}

void collectMiscRootsCallback(void *user, StgClosure* closure){
    miscRootsCount++;
    miscRoots = realloc(miscRoots, sizeof(StgClosure*) * miscRootsCount);
    miscRoots[miscRootsCount - 1] = closure;
}

void check_tso_and_misc_roots(void) {
    Capability * cap = rts_pause();
    rts_listThreads(&collectTSOsCallback, NULL);
    rts_listMiscRoots(&collectMiscRootsCallback, NULL);

    for (int i = 0; i < tsoCount; i++)
    {
        StgTSO *tso = UNTAG_CLOSURE(tsos[i]);
        if (get_itbl(tso)->type != TSO)
        {
            printf("tso returned a non-TSO type %zu at index %i\n",
                tso->header.info->type,
                i);
            exit(1);
        }
    }

    for (int i = 0; i < miscRootsCount; i++)
    {
        StgClosure *root = UNTAG_CLOSURE(miscRoots[i]);
        printf("get_itbl(root) = %p\n", get_itbl(root)); fflush(stdout);
        if (get_itbl(root)->type == TSO)
        {
            printf("rts_listThreads returned a TSO type at index %i (TSO=%zu)\n", i, TSO); fflush(stdout);
            exit(1);
        }
    }

    rts_resume(cap);
}
