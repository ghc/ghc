#ifndef MAIN_H
#define MAIN_H

#ifdef __STDC__
#define PROTO(x)	x
#else
#define PROTO(x)	()
#endif

/* our own ASSERT macro (for C) */
#ifndef DEBUG
#define ASSERT(predicate) /*nothing*/

#else
void _ghcAssert PROTO((char *, unsigned int));

#define ASSERT(predicate)			\
	if (predicate)				\
	    /*null*/;				\
	else					\
	    _ghcAssert(__FILE__, __LINE__)
#endif

/* partain: some ubiquitous types: floatish & intish.
   Dubious to use float/int, but that is what it used to be...
   (WDP 95/03)   
*/
typedef double	floatish;
typedef double  doublish; /* higher precision, if anything; little used */
typedef long	intish;
typedef int	boolish;

extern intish nsamples;
extern intish nmarks;
extern intish nidents;

extern floatish maxcombinedheight;
extern floatish areabelow;
extern floatish epsfwidth;

extern floatish xrange;
extern floatish yrange;

extern floatish auxxrange;
extern floatish auxyrange;

extern boolish eflag;
extern boolish gflag;
extern boolish yflag;
extern boolish bflag;
extern boolish sflag;
extern int     mflag;
extern boolish tflag;
extern boolish cflag;

extern char *programname;

extern char *hpfile;
extern char *psfile;
extern char *auxfile;

extern FILE *hpfp;
extern FILE *psfp;
extern FILE *auxfp;

#endif /* MAIN_H */
