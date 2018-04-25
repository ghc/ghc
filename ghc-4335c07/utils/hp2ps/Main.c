#include "Main.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Defines.h"
#include "AuxFile.h"
#include "AreaBelow.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "PsFile.h"
#include "Reorder.h"
#include "Scale.h"
#include "TopTwenty.h"
#include "TraceElement.h"
#include "Deviation.h"
#include "Error.h"
#include "Utilities.h"

static boolish pflag = 0;	/* read auxiliary file			*/
boolish eflag = 0;	/* scaled EPSF 				*/
static boolish dflag = 0;	/* sort by standard deviation		*/
static int     iflag = 0;	/* sort by identifier (3-way flag)      */
boolish gflag = 0;	/* output suitable for previewer	*/
boolish yflag = 0; 	/* ignore marks				*/
boolish bflag = 0; 	/* use a big title box			*/
boolish sflag = 0;	/* use a small title box		*/
static int     mflag = 0;	/* max no. of bands displayed (default 20) */
static boolish tflag = 0;	/* ignored threshold specified          */
boolish cflag = 0;      /* colour output                        */

static boolish filter;		/* true when running as a filter	*/
boolish multipageflag = 0;  /* true when the output should be 2 pages - key and profile */ 

static floatish WidthInPoints PROTO((char *));		  /* forward */
static FILE *Fp PROTO((char *, char **, char *, char *)); /* forward */

char *hpfile;
static char *psfile;
char *auxfile;

char *programname;

static char *pathName;
static char *baseName; /* "basename" is a std C library name (sigh) */

FILE* hpfp;
FILE* psfp;
static FILE* auxfp;

floatish xrange = 0.0;
floatish yrange = 0.0;

floatish auxxrange = 0.0;
floatish auxyrange = 0.0;

floatish epsfwidth;
floatish areabelow;

intish nsamples;
intish nmarks;
intish nidents;

floatish THRESHOLD_PERCENT = DEFAULT_THRESHOLD;
int TWENTY = DEFAULT_TWENTY;

int main(int argc, char *argv[])
{

    programname = copystring(Basename(argv[0]));

    argc--, argv++;
    while (argc && argv[0][0] == '-') {
        while (*++*argv)
            switch(**argv) {
	    case 'p':
                pflag++;
                break;
	    case 'e':
		eflag++;
                epsfwidth = WidthInPoints(*argv + 1);
                goto nextarg;
	    case 'd':
		dflag++;
                goto nextarg;
	    case 'i':
		switch( *(*argv + 1) ) {
		  case '-':
		    iflag = -1;
		    break;
		  case '+':
		  default:
		    iflag = 1;
		}
                goto nextarg;
	    case 'g':
		gflag++;
		goto nextarg;
	    case 'y':
		yflag++;
		goto nextarg;
	    case 'b':
		bflag++;
		goto nextarg;
	    case 's':
		sflag++;
		goto nextarg;
	    case 'm':
		mflag++;
		TWENTY = atoi(*argv + 1);
		// only 20 keys fit on a page
		if (TWENTY > DEFAULT_TWENTY) 
		   multipageflag++;
		goto nextarg;
	    case 'M':
	        multipageflag++;
                goto nextarg;
	    case 't':
		tflag++;
		THRESHOLD_PERCENT = (floatish) atof(*argv + 1);
		if (THRESHOLD_PERCENT < 0 || THRESHOLD_PERCENT > 5)
		    Usage(*argv-1);
		goto nextarg;
	    case 'c':
		cflag++;
		goto nextarg;
	    case '?':
	    default:
		Usage(*argv-1);
            }
nextarg: ;
        argc--, argv++;
    }

    hpfile = "stdin";
    psfile = "stdout";

    hpfp = stdin;
    psfp = stdout;

    filter = argc < 1;



    if (!filter) {
	pathName = copystring(argv[0]);
	DropSuffix(pathName, ".hp");
#if defined(_WIN32)
	DropSuffix(pathName, ".exe");
#endif
	baseName = copystring(Basename(pathName));
        
        hpfp  = Fp(pathName, &hpfile, ".hp", "r"); 
	psfp  = Fp(baseName, &psfile, ".ps", "w"); 

	if (pflag) auxfp = Fp(baseName, &auxfile, ".aux", "r");
    }

    GetHpFile(hpfp);

    if (!filter && pflag) GetAuxFile(auxfp);


    TraceElement();          /* Orders on total, Removes trace elements (tflag) */

    if (dflag) Deviation();  /* ReOrders on deviation */

    if (iflag) Identorder(iflag); /* ReOrders on identifier */

    if (pflag) Reorder();    /* ReOrders on aux file */

    /* Selects top bands (mflag) - can be more than 20 now */
    if (TWENTY != 0) TopTwenty(); 

    Dimensions();

    areabelow = AreaBelow();

    Scale();

    PutPsFile();

    if (!filter) {
        auxfp = Fp(baseName, &auxfile, ".aux", "w");
	PutAuxFile(auxfp);
    } 

    return(0);
}



typedef enum {POINTS, INCHES, MILLIMETRES} pim;

static pim Units PROTO((char *));   /* forward */

static floatish
WidthInPoints(char *wstr)
{
    floatish result;

    result = (floatish) atof(wstr);

    switch (Units(wstr)) {
	case INCHES:  		
	    result *= 72.0;
	    break;
        case MILLIMETRES:	
	    result *= 2.834646;
	    break;
        case POINTS:
	default: ;
    }

    if (result <= 144)   /* Minimum of 2in wide ! */
	Usage(wstr);

    return result;
}

	
static pim
Units(char *wstr)
{
int i;

    i = strlen(wstr) - 2;

    if (wstr[i] == 'p' && wstr[i+1] == 't') {
	return POINTS;
    } else if (wstr[i] == 'i' && wstr[i+1] == 'n') {
	return INCHES;	
    } else if (wstr[i] == 'm' && wstr[i+1] == 'm') {
	return MILLIMETRES;
    } else {
        return POINTS;
    }
}

static FILE *
Fp(char *rootname, char **filename, char *suffix, char *mode)
{
    *filename = copystring2(rootname, suffix);

    return(OpenFile(*filename, mode));
}

#if defined(DEBUG)
void
_stgAssert (filename, linenum)
  char		*filename;
  unsigned int  linenum;
{
    fflush(stdout);
    fprintf(stderr, "ASSERTION FAILED: file %s, line %u\n", filename, linenum);
    fflush(stderr);
    abort();
}
#endif
