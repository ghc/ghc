#include "HsFFI.h"
#include <unistd.h>
#include <pthread.h>
#include "hs_try_putmvar002_stub.h"

void externalPutMVar(HsStablePtr mvar, HsInt cap)
{
    hs_try_putmvar(cap,mvar);
}

void externalPutMVarFE(HsStablePtr mvar, HsInt cap)
{
    callbackPutMVar(mvar);
}

void externalManyPutMVars(HsStablePtr mvar, HsInt n, HsInt cap)
{
    for (int i = 0; i < n; i++) {
        hs_try_putmvar(cap,mvar);
    }
}

void externalManyPutMVarsFE(HsStablePtr mvar, HsInt n, HsInt cap)
{
    for (int i = 0; i < n; i++) {
        callbackPutMVar(mvar);
    }
}
