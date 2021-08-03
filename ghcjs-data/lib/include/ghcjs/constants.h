#ifndef __GHCJS_CONSTANTS_H_
#define __GHCJS_CONSTANTS_H_

// values defined in Gen2.ClosureInfo
#define CLOSURE_TYPE_FUN (1)
#define CLOSURE_TYPE_CON (2)
#define CLOSURE_TYPE_THUNK (0)
#define CLOSURE_TYPE_PAP (3)
#define CLOSURE_TYPE_BLACKHOLE (5)
#define CLOSURE_TYPE_STACKFRAME (-1)

// thread status
#define THREAD_RUNNING (0)
#define THREAD_BLOCKED (1)
#define THREAD_FINISHED (16)
#define THREAD_DIED (17)

#endif
