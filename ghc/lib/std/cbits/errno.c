/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.5 2001/05/18 16:54:06 simonmar Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsStd.h"

/* Raw errno */

int *ghcErrno(void) {
  return &errno;
}
