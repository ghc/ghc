/*
    Compile this with GCC on a new platform, to learn various
    things about argument-passing, return-returning, stack
    layout, etc.  WDP 95/05
*/

extern long int foo (long int, double, void *, char *, double, long int, char, long int);
extern double   bar (char, float, long int, long int, char *, char **);

long int
foo (long int a, double b, void *c, char *d, double e, long int f, char g, long int h)
{
    __asm__ volatile ("--- BEGIN ---");
    bar(*d, (float) b, a, f, d, (char **) d);
    __asm__ volatile ("--- END ---");
}

double
bar (char a, float b, long int c, long int d, char *e, char **f)
{
    __asm__ volatile ("--- BEGIN ---");
   foo(c, (double) b, (void *) 0, e, 0.0, d, a, d);
    __asm__ volatile ("--- END ---");
}

double 
baz(w)
    int w;
{
    int x[1000];
    int y;

    for(y = 0; y < 1000; y++)
	w += x[y];

    return ((double) w);
}
