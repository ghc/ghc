/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.2 2001/07/31 11:51:09 simonmar Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsCore.h"

/* Raw errno */
/* Covers up the fact that on Windows this is a function */

int *ghcErrno(void) {
  return &errno;
}
