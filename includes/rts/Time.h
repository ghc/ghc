/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Time values in the RTS
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * --------------------------------------------------------------------------*/

#pragma once

// For most time values in the RTS we use a fixed resolution of nanoseconds,
// normalising the time we get from platform-dependent APIs to this
// resolution.
#define TIME_RESOLUTION 1000000000
typedef int64_t Time;

#define TIME_MAX HS_INT64_MAX

#if TIME_RESOLUTION == 1000000000
// I'm being lazy, but it's awkward to define fully general versions of these
#define TimeToMS(t)      ((t) / 1000000)
#define TimeToUS(t)      ((t) / 1000)
#define TimeToNS(t)      (t)
#define MSToTime(t)      ((Time)(t) * 1000000)
#define USToTime(t)      ((Time)(t) * 1000)
#define NSToTime(t)      ((Time)(t))
#else
#error Fix TimeToNS(), TimeToUS() etc.
#endif

#define SecondsToTime(t) ((Time)(t) * TIME_RESOLUTION)
#define TimeToSeconds(t) ((t) / TIME_RESOLUTION)

// Use instead of SecondsToTime() when we have a floating-point
// seconds value, to avoid truncating it.
INLINE_HEADER Time fsecondsToTime (double t)
{
    return (Time)(t * TIME_RESOLUTION);
}

Time getProcessElapsedTime (void);
