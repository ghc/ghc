#include <signal.h>
#include <stdio.h>
#include <errno.h>

// Prints the state of the signal handlers to stdout
int main()
{
    int open = 0, i;
    sigset_t blockedsigs;

    printf("ChildInfo { masked = [");

    sigprocmask(SIG_BLOCK, NULL, &blockedsigs);
    for(i = 0; i < NSIG; ++i)
    {
        int ret = sigismember(&blockedsigs, i);
        if(ret >= 0)
        {
            if(!open)
                open=1;
            else
                printf(",");
            printf("(%d,%s)", i, ret == 1 ? "True" : "False");
        }
    }
    printf("], handlers = [");

    open = 0;
    for(i = 0; i < NSIG; ++i)
    {
        struct sigaction old;
        if(sigaction(i, NULL, &old) >= 0)
        {
            if(!open)
                open=1;
            else
                printf(",");

            printf("(%d,%s)", i,
                    old.sa_handler == SIG_IGN ? "Ignored" :
                    (old.sa_handler == SIG_DFL ? "Default" : "Handled"));
        }
    }
    printf("]}");

    return 0;
}
