/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002.
 *
 * Prototypes for functions in Interpreter.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE Capability *interpretBCO (Capability* cap);
void interp_startup ( void );
void interp_shutdown ( void );

void rts_enableStopNextBreakpointAll  ( void );
void rts_disableStopNextBreakpointAll ( void );
void rts_enableStopNextBreakpoint     ( StgPtr );
void rts_disableStopNextBreakpoint    ( StgPtr );
