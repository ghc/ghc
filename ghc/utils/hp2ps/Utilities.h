#ifndef UTILITIES_H
#define UTILITIES_H

extern char* Basename    PROTO((char *));
extern void  DropSuffix  PROTO((char *, char *));
extern FILE* OpenFile    PROTO((char *, char *));
extern void  CommaPrint  PROTO((FILE *, int));
extern char *copystring  PROTO((char *));
extern char *copystring2 PROTO((char *, char *));
extern void *xmalloc	 PROTO((int));
extern void *xrealloc	 PROTO((void *, int));

#endif /* UTILITIES_H */
