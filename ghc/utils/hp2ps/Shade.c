#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Main.h"
#include "Defines.h"
#include "Error.h"
#include "Utilities.h"

/* own stuff */
#include "Shade.h"

static struct shade {
	char* ident;
	floatish shade;
} *shademap;

static int shademapmax = 0;
static int shademapindex = 0;

/*
 *	Set the shade to be used for "ident" to "shade".
 */

void
ShadeFor(ident, shade)
  char* ident; 
  floatish shade;
{
    if (! shademap) {
	shademapmax = (nidents > TWENTY ? nidents : TWENTY) * 2;
	         /* Assume nidents read is indication of the No of
		    idents in the .aux file (*2 for good luck) */
	         /* NB *2 is needed as .aux and .hp elements may differ */
	shademap = xmalloc(shademapmax * sizeof(struct shade));
    }

    if (shademapindex < shademapmax) {
	shademap[ shademapindex ].ident = copystring(ident);
	shademap[ shademapindex ].shade = shade;
	shademapindex++;
    } else {
	Disaster("shade map overflow");
    }
}

/*
 *	Get the shade to be used for "ident" if there is one. 
 *	Otherwise, think of a new one.
 */

static floatish ThinkOfAShade PROTO((void));	/* forward */

floatish
ShadeOf(ident)
  char* ident;
{
    int i;
    floatish shade;

    for (i = 0; i < shademapindex; i++) {
	if (strcmp(shademap[i].ident, ident) == 0) {	/* got it */
	    return(shademap[i].shade);
	}
    }

    shade = ThinkOfAShade();

    ShadeFor(ident, shade);

    return shade; 
}



#define N_SHADES 10 

static floatish shades[ N_SHADES ] = {
    0.00000, 0.20000, 0.60000, 0.30000, 0.90000, 
    0.40000, 1.00000, 0.70000, 0.50000,  0.80000
};

static floatish
ThinkOfAShade()
{
    static int thisshade = 0;

    floatish x;

    x = shades[ thisshade ]; 
    thisshade = (thisshade + 1) % N_SHADES;
    return x; 
}
