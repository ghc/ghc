/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: progargs.c,v 1.2 1999/03/02 20:14:01 sof Exp $
 *
 * System.getArgs Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

DLLIMPORT extern char** prog_argv;
DLLIMPORT extern int prog_argc;

StgAddr
get_prog_argv(void)
{ 
  return prog_argv;
}

StgInt
get_prog_argc()
{
  return prog_argc;
}

