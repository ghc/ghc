/* -----------------------------------------------------------------------------
 * $Id: MachDeps.h,v 1.2 1998/12/02 13:21:12 simonm Exp $
 *
 * (c) The GRASP/AQUA Project, Glasgow University, 1998
 * 
 * Definitions that characterise machine specific properties of basic
 * Stg types provided as unboxed types (mirrors the typedefs in
 * StgTypes.)
 *
 * NB: THIS FILE IS INCLUDED IN HASKELL SOURCE!
 * ---------------------------------------------------------------------------*/

#ifndef MACHDEPS_H
#define MACHDEPS_H

#include "config.h"

#define CHAR_SIZE_IN_BYTES	1
#define ADDR_SIZE_IN_BYTES	SIZEOF_VOID_P
#define INT_SIZE_IN_BYTES	SIZEOF_LONG
#define WORD_SIZE_IN_BYTES	SIZEOF_LONG

#if SIZEOF_DOUBLE == SIZEOF_VOID_P
#define FLOAT_SIZE_IN_BYTES 	SIZEOF_DOUBLE
#define DOUBLE_SIZE_IN_BYTES	SIZEOF_DOUBLE
#else
#define FLOAT_SIZE_IN_BYTES 	SIZEOF_FLOAT
#define DOUBLE_SIZE_IN_BYTES	SIZEOF_DOUBLE
#endif

#endif
