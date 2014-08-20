#ifndef WCSUBST_INCL

#define WCSUBST_INCL

#include <stdlib.h>

int u_iswupper(int wc);
int u_iswdigit(int wc);
int u_iswalpha(int wc);
int u_iswcntrl(int wc);
int u_iswspace(int wc);
int u_iswprint(int wc);
int u_iswlower(int wc);

int u_iswalnum(int wc);

int u_towlower(int wc);
int u_towupper(int wc);
int u_towtitle(int wc);

int u_gencat(int wc);

#endif

