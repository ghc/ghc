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
# include "RtsMessages.h"

void errorBelch2(const char*s, char *t)
{
    errorBelch(s,t);
}

void debugBelch2(const char*s, char *t)
{
    debugBelch(s,t);
}
#endif /* __GLASGOW_HASKELL__ */
