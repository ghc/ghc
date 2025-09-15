
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

void checkGcRoots(void)
{
    PauseToken * token = rts_pause();

    // Check TSO collection.
    rts_listThreads(&collectTSOsCallback, NULL);
    for (int i = 0; i < tsoCount; i++)
    {
        StgClosure *tso = UNTAG_CLOSURE((StgClosure*) tsos[i]);
        if (get_itbl(tso)->type != TSO)
        {
            fprintf(stderr, "tso returned a non-TSO type %u at index %i\n",
                tso->header.info->type,
                i);
            exit(1);
        }
    }

    // Check misc GC roots collection.
    rts_listMiscRoots(&collectMiscRootsCallback, NULL);
    for (int i = 0; i < miscRootsCount; i++)
    {
        StgClosure *root = UNTAG_CLOSURE(miscRoots[i]);
        if (get_itbl(root)->type == TSO)
        {
            fprintf(stderr, "rts_listThreads unexpectedly returned an TSO type at index %i (TSO=%d)\n", i, TSO);
            exit(1);
        }
    }


    rts_resume(token);
}
