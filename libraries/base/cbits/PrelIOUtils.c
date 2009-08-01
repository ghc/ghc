/* 
 * (c) The University of Glasgow 2002
 *
 * static versions of the inline functions in HsCore.h
 */

#define INLINE

#ifdef __GLASGOW_HASKELL__
# include "Rts.h"
#endif

#include "HsBase.h"

#ifdef __GLASGOW_HASKELL__

void errorBelch2(const char*s, char *t)
{
    errorBelch(s,t);
}

void debugBelch2(const char*s, char *t)
{
    debugBelch(s,t);
}

// Use a C wrapper for this because we avoid hsc2hs in base
#if HAVE_LANGINFO_H
#include <langinfo.h>
char *localeEncoding (void)
{
    return nl_langinfo(CODESET);
}
#endif

#endif /* __GLASGOW_HASKELL__ */
