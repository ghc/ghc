/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * En/disable RTS options
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSOPTS_H
#define RTSOPTS_H

typedef enum {
    RtsOptsNone,         // +RTS causes an error
    RtsOptsSafeOnly,     // safe RTS options allowed; others cause an error
    RtsOptsAll           // all RTS options allowed
  } RtsOptsEnabledEnum;

extern const RtsOptsEnabledEnum rtsOptsEnabled;

#endif /* RTSOPTS_H */
