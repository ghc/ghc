#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

// Invokes a process, making sure that the state of the signal
// handlers has all been set back to the unix default.
int main(int argc, char **argv)
{
    int i;
    sigset_t blockedsigs;
    struct sigaction action;

    // unblock all signals
    sigemptyset(&blockedsigs);
    sigprocmask(SIG_BLOCK, NULL, NULL);

    // reset all signals to SIG_DFL
    memset(&action, 0, sizeof(action));
    action.sa_handler = SIG_DFL;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    for(i = 0; i < NSIG; ++i)
        sigaction(i, &action, NULL);

    execv(argv[1], argv+1);
    fprintf(stderr, "failed to execv %s\n", argv[1]);
    return 0;
}
