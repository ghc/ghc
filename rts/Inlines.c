// all functions declared with EXTERN_INLINE in the header files get
// compiled for real here, just in case the definition was not inlined
// at some call site:
#define KEEP_INLINES
#include "PosixSource.h"
#include "Rts.h"
#include "Schedule.h"
#include "Capability.h"
#include "WSDeque.h"

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
