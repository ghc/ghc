/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.5 2002/09/25 15:24:07 simonmar Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsBase.h"

/* Raw errno */
/* Covers up the fact that on Windows this is a function */

int *ghcErrno(void) {
  return &errno;
}
