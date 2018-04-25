#include "Main.h"
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "Defines.h"
#include "Dimensions.h"
#include "HpFile.h"
#include "Shade.h"
#include "PsFile.h"
#include "Utilities.h"

/* own stuff */
#include "Key.h"

static void KeyEntry PROTO((floatish, char *, floatish));

void Key(void)
{
    intish i;
    floatish c;
    floatish dc;

    for (i = 0; i < nidents; i++)    /* count identifiers */ 
	;

    c  = multipageflag ? 0 : graphy0;
    dc = graphheight / (floatish) ((i <= 20) ? (i + 1) : 20);

    for (i = 0; i < nidents; i++) {
	c += dc;
	KeyEntry(c, identtable[i]->name, ShadeOf(identtable[i]->name));
        // if we have spit out 20 entries and we're going to output more
        // advance the page
	if (i % DEFAULT_TWENTY == (DEFAULT_TWENTY - 1) && i != nidents - 1) {
	  c = 0;
	  NextPage();
	}
    }
}

static void
escape(char *result, const char *name)
{
    while (*name != '\0')
    {
        // escape parentheses too, because "name" could contain un-matched ones
        if (*name == '\\' || *name == '(' || *name == ')')
        {
            *result++ = '\\';
        }
        *result++ = *name++;
    }
    *result = '\0';
}

static void
KeyEntry(floatish centreline, char *name, floatish colour)
{
    floatish namebase;
    floatish keyboxbase;
    floatish kstart;

    namebase = centreline - (floatish) (NORMAL_FONT / 2);
    keyboxbase = centreline - ((floatish) KEY_BOX_WIDTH / 2.0);

    kstart = graphx0 + (multipageflag ? 0 : graphwidth);

    fprintf(psfp, "%f %f moveto\n", kstart + borderspace, keyboxbase);
    fprintf(psfp, "0 %d rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "%d 0 rlineto\n", KEY_BOX_WIDTH);
    fprintf(psfp, "0 %d rlineto\n", -KEY_BOX_WIDTH);
    fprintf(psfp, "closepath\n");

    fprintf(psfp, "gsave\n"); 
    SetPSColour(colour);
    fprintf(psfp, "fill\n");
    fprintf(psfp, "grestore\n");
    fprintf(psfp, "stroke\n");

    fprintf(psfp, "HE%d setfont\n", NORMAL_FONT);
    fprintf(psfp, "%f %f moveto\n", kstart + (floatish) KEY_BOX_WIDTH + 2 * borderspace, namebase);

    // escape backslashes in 'name'
    char *escaped_name = (char*) xmalloc(strlen(name) * 2 + 1);
    escape(escaped_name, name);
    fprintf(psfp, "(%s) show\n", escaped_name);
    free(escaped_name);
}
