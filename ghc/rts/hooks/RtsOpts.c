/* -----------------------------------------------------------------------------
 * $Id: RtsOpts.c,v 1.1 2001/10/01 11:36:29 simonmar Exp $
 *
 * Default RTS options.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

// Default RTS options can be given by providing an alternate
// definition for this variable, pointing to a string of RTS options.
char *ghc_rts_opts = NULL;
