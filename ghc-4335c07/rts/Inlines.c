// all functions declared with EXTERN_INLINE in the header files get
// compiled for real here, just in case the definition was not inlined
// at some call site:
#define KEEP_INLINES
#include "PosixSource.h"
#include "Rts.h"
#include "Schedule.h"
#include "Capability.h"
#include "WSDeque.h"
#include "SMPClosureOps.h"
