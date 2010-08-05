/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * En/disable RTS options
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSOPTS_H
#define RTSOPTS_H

typedef enum {rtsOptsNone, rtsOptsSafeOnly, rtsOptsAll} rtsOptsEnabledEnum;

extern const rtsOptsEnabledEnum rtsOptsEnabled;

#endif /* RTSOPTS_H */
