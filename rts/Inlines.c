// All functions declared with EXTERN_INLINE in the header files get
// compiled for real here. Some of them are called by Cmm (e.g.
// recordClosureMutated) and therefore the real thing needs to reside
// in Inlines.o for Cmm ccall to work.
#define KEEP_INLINES
#include "rts/PosixSource.h"
#include "Rts.h"
#include "Schedule.h"
#include "Capability.h"
#include "WSDeque.h"
#include "SMPClosureOps.h"
