/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Send message to each component of system:
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: connect.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:02 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"

Void everybody(what)            /* send command `what' to each component of*/
Int what; {                     /* system to respond as appropriate ...    */
    machdep(what);              /* The order of calling each component is  */
    storage(what);              /* important for the INSTALL command       */
    substitution(what);
    input(what);
    linkControl(what);
    staticAnalysis(what);
    deriveControl(what);
    typeChecker(what);
    desugarControl(what);
    translateControl(what);
    compiler(what);
    codegen(what);
}

/*-------------------------------------------------------------------------*/
