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
void rts_enableStopNextBreakpoint     ( StgTSO* );
void rts_disableStopNextBreakpoint    ( StgTSO* );

// Debugger step out
void rts_enableStopAfterReturn        ( StgTSO* );
void rts_disableStopAfterReturn       ( StgTSO* );
