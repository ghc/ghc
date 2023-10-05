#include "Rts.h"
#include "RtsUtils.h"
#include "LinkerInternals.h"
#include "GetEnv.h"
#include "InitFini.h"

/*
 * Note [Initializers and finalizers (PEi386/ELF)]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Most ABIs allow an object to define initializers and finalizers to be run
 * at load/unload time, respectively. These are represented in two ways:
 *
 *  - a `.init`/`.fini` section which contains a function of type init_t which
 *    is to be executed during initialization/finalization.
 *
 *  - `.ctors`/`.dtors` sections; these contain an array of pointers to
 *    `init_t`/`fini_t` functions, all of which should be executed at
 *    initialization/finalization time. The `.ctors` entries are run in reverse
 *    order. The list may end in a 0 or -1 sentinel value.
 *
 *  - `.init_array`/`.fini_array` sections; these contain an array
 *    of pointers to `init_t`/`fini_t` functions.
 *
 * Objects may contain multiple `.ctors`/`.dtors` and
 * `.init_array`/`.fini_array` sections, each optionally suffixed with an
 * 16-bit integer priority (e.g. `.init_array.1234`). Confusingly, `.ctors`
 * priorities and `.init_array` priorities have different orderings: `.ctors`
 * sections are run from high to low priority whereas `.init_array` sections
 * are run from low-to-high.
 *
 * Sections without a priority (e.g.  `.ctors`) are assumed to run last (that
 * is, are given a priority of 0xffff).
 *
 * In general, we run finalizers in the reverse order of the associated
 * initializers. That is to say, e.g., .init_array entries are run from first
 * to last entry and therefore .fini_array entries are run from last-to-first.
 *
 * To determine the ordering among the various section types, we follow glibc's
 * model:
 *
 *  - first run .ctors (last entry to first entry)
 *  - then run .init_arrays (first-to-last)
 *
 * and on unload we run in opposite order:
 *
 *  - first run fini_arrays (first-to-last)
 *  - then run .dtors (last-to-first)
 *
 * For more about how the code generator emits initializers and finalizers see
 * Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini.
 */

// Priority follows the init_array definition: initializers are run
// lowest-to-highest, finalizers run highest-to-lowest.
void addInitFini(struct InitFiniList **head, Section *section, enum InitFiniKind kind, uint32_t priority)
{
    struct InitFiniList *slist = stgMallocBytes(sizeof(struct InitFiniList), "addInitFini");
    slist->section = section;
    slist->kind = kind;
    slist->priority = priority;
    slist->next = *head;
    *head = slist;
}

enum SortOrder { INCREASING, DECREASING };

// Sort a InitFiniList by priority.
static void sortInitFiniList(struct InitFiniList **slist, enum SortOrder order)
{
    // Bubble sort
    bool done = false;
    while (!done) {
        struct InitFiniList **last = slist;
        done = true;
        while (*last != NULL && (*last)->next != NULL) {
            struct InitFiniList *s0 = *last;
            struct InitFiniList *s1 = s0->next;
            bool flip;
            switch (order) {
                case INCREASING: flip = s0->priority > s1->priority; break;
                case DECREASING: flip = s0->priority < s1->priority; break;
            }
            if (flip) {
                s0->next = s1->next;
                s1->next = s0;
                *last = s1;
                done = false;
            } else {
                last = &s0->next;
            }
        }
    }
}

void freeInitFiniList(struct InitFiniList *slist)
{
    while (slist != NULL) {
        struct InitFiniList *next = slist->next;
        stgFree(slist);
        slist = next;
    }
}

static bool runInitFini(struct InitFiniList **head)
{
    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (struct InitFiniList *slist = *head;
           slist != NULL;
           slist = slist->next)
    {
        Section *section = slist->section;
        switch (slist->kind) {
        case INITFINI_INIT: {
            init_t *init = (init_t*)section->start;
            (*init)(argc, argv, envv);
            break;
        }
        case INITFINI_FINI: {
            fini_t *fini = (fini_t*)section->start;
            (*fini)();
            break;
        }
        case INITFINI_CTORS: {
            uint8_t *init_startC = section->start;
            init_t *init_start   = (init_t*)init_startC;
            init_t *init_end     = (init_t*)(init_startC + section->size);

            // ctors are run *backwards*!
            for (init_t *init = init_end - 1; init >= init_start; init--) {
                if ((intptr_t) *init == 0x0 || (intptr_t)*init == -1) {
                    continue;
                }
                (*init)(argc, argv, envv);
            }
            break;
        }
        case INITFINI_DTORS: {
            char *fini_startC = section->start;
            fini_t *fini_start = (fini_t*)fini_startC;
            fini_t *fini_end = (fini_t*)(fini_startC + section->size);
            for (fini_t *fini = fini_start; fini < fini_end; fini++) {
                if ((intptr_t) *fini == 0x0 || (intptr_t) *fini == -1) {
                    continue;
                }
                (*fini)();
            }
            break;
        }
        case INITFINI_INIT_ARRAY: {
            char *init_startC = section->start;
            init_t *init_start = (init_t*)init_startC;
            init_t *init_end = (init_t*)(init_startC + section->size);
            for (init_t *init = init_start; init < init_end; init++) {
                CHECK(0x0 != *init);
                (*init)(argc, argv, envv);
            }
            break;
        }
        case INITFINI_FINI_ARRAY: {
            char *fini_startC = section->start;
            fini_t *fini_start = (fini_t*)fini_startC;
            fini_t *fini_end = (fini_t*)(fini_startC + section->size);
            // .fini_array finalizers are run backwards
            for (fini_t *fini = fini_end - 1; fini >= fini_start; fini--) {
                CHECK(0x0 != *fini);
                (*fini)();
            }
            break;
        }
        default: barf("unknown InitFiniKind");
        }
    }
    freeInitFiniList(*head);
    *head = NULL;

    freeProgEnvv(envc, envv);
    return true;
}

// Run the constructors/initializers of an ObjectCode.
// Returns 1 on success.
// See Note [Initializers and finalizers (PEi386/ELF)].
bool runInit(struct InitFiniList **head)
{
    sortInitFiniList(head, INCREASING);
    return runInitFini(head);
}

// Run the finalizers of an ObjectCode.
// Returns 1 on success.
// See Note [Initializers and finalizers (PEi386/ELF)].
bool runFini(struct InitFiniList **head)
{
    sortInitFiniList(head, DECREASING);
    return runInitFini(head);
}
