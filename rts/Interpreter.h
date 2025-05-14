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

// Debugger single step
void rts_enableStopNextBreakpointAll  ( void );
void rts_disableStopNextBreakpointAll ( void );
void rts_enableStopNextBreakpoint     ( StgPtr );
void rts_disableStopNextBreakpoint    ( StgPtr );

// Debugger step out
void rts_enableStopAfterReturn        ( StgPtr );
void rts_disableStopAfterReturn       ( StgPtr );
