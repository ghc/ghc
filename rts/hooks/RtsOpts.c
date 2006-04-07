/* -----------------------------------------------------------------------------
 *
 * Default RTS options.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#include <stdlib.h>

// Default RTS options can be given by providing an alternate
// definition for this variable, pointing to a string of RTS options.
char *ghc_rts_opts = NULL;
