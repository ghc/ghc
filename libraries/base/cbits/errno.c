/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.1 2001/06/28 14:15:04 simonmar Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsCore.h"

/* Raw errno */

int *ghcErrno(void) {
  return &errno;
}
