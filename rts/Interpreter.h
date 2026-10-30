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

// The following debugger functions are exported from here, but not from the
// RTS public api (RtsApi.h). They are used by the `ghci` library to implement
// debugger commands on the interpreter like step-next, step-out, and
// break-on-exception.

// Debugger single step
RTS_PUBLIC void rts_enableStopNextBreakpointAll  ( void );
RTS_PUBLIC void rts_disableStopNextBreakpointAll ( void );
RTS_PUBLIC void rts_enableStopNextBreakpoint     ( StgTSO* );
RTS_PUBLIC void rts_disableStopNextBreakpoint    ( StgTSO* );

// Debugger step out
RTS_PUBLIC void rts_enableStopAfterReturn        ( StgTSO* );
RTS_PUBLIC void rts_disableStopAfterReturn       ( StgTSO* );

// Debugger stop on exception
RTS_PUBLIC void rts_enableStopOnException        ( StgTSO* );
RTS_PUBLIC void rts_disableStopOnException       ( StgTSO* );
RTS_PUBLIC void rts_enableStopOnExceptionAll     ( void );
RTS_PUBLIC void rts_disableStopOnExceptionAll    ( void );
