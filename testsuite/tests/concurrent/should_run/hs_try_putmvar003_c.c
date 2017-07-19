#include "HsFFI.h"
#include "Rts.h"
#include "RtsAPI.h"
#include <unistd.h>
#include <pthread.h>
#include "hs_try_putmvar003_stub.h"

struct callback_queue {
    pthread_mutex_t lock;
    pthread_cond_t cond;
    int use_foreign_export;
    // How many requests will be submitted to this queue?
    // (e.g. n_threads * n_requests_per_thread)
    int n_requests;
    struct callback *pending;
};

struct callback {
    HsStablePtr mvar;
    int cap;
    int *presult;
    struct callback *next;
};

void* callback(struct callback_queue *q)
{
    struct callback *cb;

    pthread_mutex_lock(&q->lock);
    for (int i=0; i < q->n_requests; i++) {
        if (q->pending == NULL) {
            pthread_cond_wait(&q->cond,&q->lock);
        }
        if (q->pending != NULL) {
            cb = q->pending;
            q->pending = cb->next;
            *cb->presult = 42;
            if (q->use_foreign_export) {
                callbackPutMVar(cb->mvar);
            } else {
                hs_try_putmvar(cb->cap,cb->mvar);
            }
            free(cb);
        }
    }
    pthread_mutex_unlock(&q->lock);

    hs_thread_done();
    return NULL;
}

typedef void* threadfunc(void *);

struct callback_queue* mkCallbackQueue(int use_foreign_export, int n_requests)
{
    struct callback_queue *q = malloc(sizeof(struct callback_queue));
    pthread_t t;
    pthread_mutex_init(&q->lock, NULL);
    pthread_cond_init(&q->cond, NULL);
    q->pending = NULL;
    q->use_foreign_export = use_foreign_export;
    q->n_requests = n_requests;
    pthread_create(&t, NULL, (threadfunc*)callback, q);
    return q;
}

void destroyCallbackQueue(struct callback_queue *q)
{
    pthread_mutex_destroy(&q->lock);
    pthread_cond_destroy(&q->cond);
    free(q);
}

void scheduleCallback(struct callback_queue *q,
                      HsStablePtr mvar,
                      HsInt cap, int *presult)
{
    struct callback *p = malloc(sizeof(struct callback));
    p->mvar = mvar;
    p->cap = cap;
    p->presult = presult;
    pthread_mutex_lock(&q->lock);
    p->next = q->pending;
    q->pending = p;
    pthread_cond_signal(&q->cond);
    pthread_mutex_unlock(&q->lock);
}
