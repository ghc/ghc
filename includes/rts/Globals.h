/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_GLOBALS_H
#define RTS_GLOBALS_H

StgStablePtr getOrSetGHCConcSignalSignalHandlerStore(StgStablePtr value);
StgStablePtr getOrSetGHCConcWindowsPendingDelaysStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcWindowsIOManagerThreadStore(StgStablePtr ptr);
StgStablePtr getOrSetGHCConcWindowsProddingStore(StgStablePtr ptr);
StgStablePtr getOrSetSystemEventThreadEventManagerStore(StgStablePtr ptr);
StgStablePtr getOrSetSystemEventThreadIOManagerThreadStore(StgStablePtr ptr);
StgStablePtr getOrSetSystemTimerThreadEventManagerStore(StgStablePtr ptr);
StgStablePtr getOrSetSystemTimerThreadIOManagerThreadStore(StgStablePtr ptr);
StgStablePtr getOrSetLibHSghcFastStringTable(StgStablePtr ptr);

#endif /* RTS_GLOBALS_H */
