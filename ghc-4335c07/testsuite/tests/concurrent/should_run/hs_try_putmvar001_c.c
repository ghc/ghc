#include "HsFFI.h"
#include "Rts.h"
#include "RtsAPI.h"
#include <unistd.h>
#include <pthread.h>

struct callback {
    HsStablePtr mvar;
    int cap;
    int *presult;
};

void* callback(struct callback *p)
{
    usleep(200);
    *p->presult = 42;
    hs_try_putmvar(p->cap,p->mvar);
    free(p);
    hs_thread_done();
    return NULL;
}

void scheduleCallback(HsStablePtr mvar, HsInt cap, int *presult)
{
    pthread_t t;
    struct callback *p = malloc(sizeof(struct callback));
    p->mvar = mvar;
    p->cap = cap;
    p->presult = presult;
    pthread_create(&t, NULL, callback, p);
}
