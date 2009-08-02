/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_GLOBALS_H
#define RTS_GLOBALS_H

StgStablePtr getOrSetTypeableStore(StgStablePtr value);
StgStablePtr getOrSetSignalHandlerStore(StgStablePtr value);

#endif /* RTS_GLOBALS_H */
