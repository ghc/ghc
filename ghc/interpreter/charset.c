/* --------------------------------------------------------------------------
 * Character set handling:
 *
 * Hugs follows Haskell 1.3 in assuming that input uses the ISO-8859-1
 * character set.  The following code provides methods for classifying
 * input characters according to the lexical structure specified by the
 * report.  Hugs should still accept older programs because ASCII is
 * essentially just a subset of the ISO character set.
 *
 * Notes: If you want to port Hugs to a machine that uses something
 * substantially different from the ISO character set, then you will need
 * to insert additional code to map between character sets.
 *
 * Relies, implicitly but for this comment, on assumption that NUM_CHARS=256.
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "charset.h"

unsigned char   ctable[NUM_CHARS];

Void initCharTab() {                    /* Initialize char decode table    */
#define setRange(x,f,t) {Int i=f;   while (i<=t) ctable[i++] |=x;}
#define setChars(x,s)   {char *p=s; while (*p)   ctable[(Int)*p++]|=x;}
#define setCopy(x,c)    {Int i;                         \
                         for (i=0; i<NUM_CHARS; ++i)    \
                             if (isIn(i,c))             \
                                 ctable[i]|=x;          \
                        }

    setRange(DIGIT,     '0','9');       /* ASCII decimal digits            */

    setRange(SMALL,     'a','z');       /* ASCII lower case letters        */
    setRange(SMALL,     223,246);       /* ISO lower case letters          */
    setRange(SMALL,     248,255);       /* (omits division symbol, 247)    */

    setRange(LARGE,     'A','Z');       /* ASCII upper case letters        */
    setRange(LARGE,     192,214);       /* ISO upper case letters          */
    setRange(LARGE,     216,222);       /* (omits multiplication, 215)     */

    setRange(SYMBOL,    161,191);       /* Symbol characters + ':'         */
    setRange(SYMBOL,    215,215);
    setRange(SYMBOL,    247,247);
    setChars(SYMBOL,    ":!#$%&*+./<=>?@\\^|-~");

    setChars(IDAFTER,   "'_");          /* Characters in identifier        */
    setCopy (IDAFTER,   (DIGIT|SMALL|LARGE));

    setRange(SPACE,     ' ',' ');       /* ASCII space character           */
    setRange(SPACE,     160,160);       /* ISO non breaking space          */
    setRange(SPACE,     9,13);          /* special whitespace: \t\n\v\f\r  */

    setChars(PRINT,     "(),;[]_`{}");  /* Special characters              */
    setChars(PRINT,     " '\"");        /* Space and quotes                */
    setCopy (PRINT,     (DIGIT|SMALL|LARGE|SYMBOL));

#undef setRange
#undef setChars
#undef setCopy
}

