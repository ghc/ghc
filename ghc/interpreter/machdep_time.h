
#ifndef MACHDEP_TIME_INCLUDED
#define MACHDEP_TIME_INCLUDED

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#if RISCOS
typedef struct { unsigned hi, lo; } Time;
#define timeChanged(now,thn)    (now.hi!=thn.hi || now.lo!=thn.lo)
#define timeSet(var,tm)         var.hi = tm.hi; var.lo = tm.lo
#error  firstTimeIsLater, whicheverIsLater needs implementing
#else
typedef time_t Time;
#define timeChanged(now,thn)      (now!=thn)
#define timeSet(var,tm)           var = tm
#define firstTimeIsLater(t1,t2)   ((t1)>(t2))
#define whicheverIsLater(t1,t2)   (((t1)>(t2)) ? (t1) : (t2))
#endif

#endif
