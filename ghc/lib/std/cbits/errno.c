/* 
 * (c) The University of Glasgow, 2000-2001
 *
 * $Id: errno.c,v 1.6 2001/07/13 11:11:34 rrt Exp $
 *
 * GHC Error Number Conversion
 */

#include "HsStd.h"

/* Raw errno */
/* Covers up the fact that on Windows this is a function */

int *ghcErrno(void) {
  return &errno;
}
