/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: progargs.c,v 1.4 2001/01/11 17:25:58 simonmar Exp $
 *
 * System.getArgs Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

HsAddr
get_prog_argv(void)
{ 
  return prog_argv;
}

HsInt
get_prog_argc()
{
  return prog_argc;
}

