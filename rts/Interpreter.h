/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002.
 *
 * Prototypes for functions in Interpreter.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE Capability *interpretBCO (Capability* cap);
RTS_PUBLIC void interp_startup ( void );
RTS_PUBLIC void interp_shutdown ( void );

// Debugger single step
RTS_PUBLIC void rts_enableStopNextBreakpointAll  ( void );
RTS_PUBLIC void rts_disableStopNextBreakpointAll ( void );
RTS_PUBLIC void rts_enableStopNextBreakpoint     ( StgTSO* );
RTS_PUBLIC void rts_disableStopNextBreakpoint    ( StgTSO* );

// Debugger step out
RTS_PUBLIC void rts_enableStopAfterReturn        ( StgTSO* );
RTS_PUBLIC void rts_disableStopAfterReturn       ( StgTSO* );

// Internal
extern RTS_PUBLIC HsStablePtr rts_breakpoint_io_action;
extern RTS_PUBLIC int rts_stop_next_breakpoint;
extern RTS_PUBLIC int rts_stop_on_exception;