#include <stdlib.h>
#include "T8124_stub.h"
#include "HsFFI.h"
#include <pthread.h>

void *thread(void *param)
{
    f(3);
    hs_thread_done();
    pthread_exit(NULL);
}

int main (int argc, char *argv[])
{
    hs_init(&argc,&argv);

    // check that we can call hs_thread_done() without having made any
    // Haskell calls:
    hs_thread_done();

    // check that we can call hs_thread_done() and then make another Haskell
    // call:
    int i;
    for (i=0; i < 1000; i++) {
        f(3);
        hs_thread_done();
    }

    // check that we can call hs_thread_done() twice:
    hs_thread_done();
    hs_thread_done();

    // check that hs_thread_done() from child threads works:
    pthread_t pid;
    for (i=0; i < 1000; i++) {
        pthread_create(&pid, NULL, thread, NULL);
        pthread_join(pid, NULL);
    }

    hs_exit();
    exit(0);
}
