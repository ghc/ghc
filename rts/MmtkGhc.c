#include "Rts.h"
#include "mmtk.h"
#include "Capability.h"
#include "Task.h"
#include "mmtk/ghc/mmtk_upcalls.h"

#if defined(MMTK_GHC)
MMTk_Mutator *upcall_get_mutator(void *tls)
{
    ASSERT(upcall_is_task(tls));
    Task *task = (Task *) tls;
    return task->mmutator;
}
#endif