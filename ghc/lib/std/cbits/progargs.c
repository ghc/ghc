/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: progargs.c,v 1.3 2000/03/14 01:52:25 sof Exp $
 *
 * System.getArgs Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

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

