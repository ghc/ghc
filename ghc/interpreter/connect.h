/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Connections between components of the Hugs system
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: connect.h,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:03 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

#define RESET   1               /* reset subsystem                         */
#define MARK    2               /* mark parts of graph in use by subsystem */
#define INSTALL 3               /* install subsystem (executed once only)  */
#define EXIT    4               /* Take action immediately before exit()   */
#define BREAK   5               /* Take action after program break         */

extern  Void   everybody        Args((Int));
extern  Void   machdep          Args((Int));
extern  Void   storage          Args((Int));
extern  Void   linkControl      Args((Int));
extern  Void   translateControl Args((Int));
extern  Void   staticAnalysis   Args((Int));
extern  Void   interface        Args((Int));
extern  Void   deriveControl    Args((Int));
extern  Void   input            Args((Int));
extern  Void   typeChecker      Args((Int));
extern  Void   desugarControl   Args((Int));
extern  Void   codegen          Args((Int));
extern  Void   compiler         Args((Int));
extern  Void   substitution     Args((Int));
extern  Void   stgTranslate     Args((Int));
extern  Void   codegen          Args((Int));

/*-------------------------------------------------------------------------*/
