# line 7 "io/posix.lc"

#define NULL_REG_MAP
#include "stgdefs.h"
#include "stgio.h"
#include "libposix.h"
#include "signals.h"

int 
cvtSignal(signum)
int signum;
{
    switch(signum) {
    default:
	return signum;
    case SIGABRT:
	return GHC_SIGABRT;
    case SIGALRM:
	return GHC_SIGALRM;
    case SIGFPE:
	return GHC_SIGFPE;
    case SIGHUP:
	return GHC_SIGHUP;
    case SIGILL:
	return GHC_SIGILL;
    case SIGINT:
	return GHC_SIGINT;
    case SIGKILL:
	return GHC_SIGKILL;
    case SIGPIPE:
	return GHC_SIGPIPE;
    case SIGQUIT:
	return GHC_SIGQUIT;
    case SIGSEGV:
	return GHC_SIGSEGV;
    case SIGTERM:
	return GHC_SIGTERM;
    case SIGUSR1:
	return GHC_SIGUSR1;
    case SIGUSR2:
	return GHC_SIGUSR2;
    case SIGCHLD:
	return GHC_SIGCHLD;
    case SIGCONT:
	return GHC_SIGCONT;
    case SIGSTOP:
	return GHC_SIGSTOP;
    case SIGTSTP:
	return GHC_SIGTSTP;
    case SIGTTIN:
	return GHC_SIGTTIN;
    case SIGTTOU:
	return GHC_SIGTTOU;
    }
}
