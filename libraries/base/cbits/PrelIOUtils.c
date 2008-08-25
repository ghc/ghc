/* 
 * (c) The University of Glasgow 2002
 *
 * static versions of the inline functions in HsCore.h
 */

#define INLINE
#include "HsBase.h"
#include "Stg.h"
#include "RtsMessages.h"

void errorBelch2(const char*s, char *t)
{
    errorBelch(s,t);
}

void debugBelch2(const char*s, char *t)
{
    debugBelch(s,t);
}
