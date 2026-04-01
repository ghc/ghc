#include "Rts.h"

int has_local_stop_next_breakpoint(void)
{
    CapabilityPublic *cap = (CapabilityPublic *) rts_unsafeGetMyCapability();
    StgTSO *tso = cap->r.rCurrentTSO;
    return (tso->flags & TSO_STOP_NEXT_BREAKPOINT) != 0;
}

int has_local_stop_after_return(void)
{
    CapabilityPublic *cap = (CapabilityPublic *) rts_unsafeGetMyCapability();
    StgTSO *tso = cap->r.rCurrentTSO;
    return (tso->flags & TSO_STOP_AFTER_RETURN) != 0;
}
