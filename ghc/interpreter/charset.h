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

extern  unsigned char   ctable[NUM_CHARS];

#define isIn(c,x)       (ctable[(Int)(c)]&(x))
#define isISO(c)        (0<=(c) && (c)<NUM_CHARS)

#define DIGIT           0x01
#define SMALL           0x02
#define LARGE           0x04
#define SYMBOL          0x08
#define IDAFTER         0x10
#define SPACE           0x20
#define PRINT           0x40

extern Void local initCharTab Args(( Void ));

