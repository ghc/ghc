/* 
 * (c) The University of Glasgow 2002
 *
 * static versions of the inline functions in HsCore.h
 */

#define INLINE
#include "HsBase.h"

void errorBelch2(const char*s, char *t)
{
    return errorBelch(s,t);
}

void debugBelch2(const char*s, char *t)
{
    return debugBelch(s,t);
}
