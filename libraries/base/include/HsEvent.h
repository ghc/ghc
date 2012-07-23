#ifndef __HS_EVENT_H__
#define __HS_EVENT_H__

#include "EventConfig.h"

#include <signal.h>
#include <pthread.h>

#if !defined(INLINE)
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE inline
# endif
#endif

INLINE int __hsevent_num_signals(void)
{
#if defined(_NSIG)
    return _NSIG;
#else
    return 128; /* best guess */
#endif
}

INLINE void __hsevent_thread_self(pthread_t *tid)
{
    *tid = pthread_self();
}

INLINE int __hsevent_kill_thread(pthread_t *tid, int sig)
{
    return pthread_kill(*tid, sig);
}

#endif /* __HS_EVENT_H__ */
/*
 * Local Variables: 
 * c-file-style: "stroustrup" 
 * End: 
 */
