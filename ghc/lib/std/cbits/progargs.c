/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: progargs.c,v 1.5 2001/05/18 16:54:06 simonmar Exp $
 *
 * System.getArgs Runtime Support
 */

#include "Rts.h"

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

