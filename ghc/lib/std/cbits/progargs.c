/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: progargs.c,v 1.1 1998/12/09 17:09:50 sof Exp $
 *
 * System.getArgs Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

extern char** prog_argv;
extern int prog_argc;

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

