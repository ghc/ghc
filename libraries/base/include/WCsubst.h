#ifndef WCSUBST_INCL

#define WCSUBST_INCL

#include <stdlib.h>
#include "HsFFI.h"

HsInt u_iswupper(HsInt wc);
HsInt u_iswdigit(HsInt wc);
HsInt u_iswalpha(HsInt wc);
HsInt u_iswcntrl(HsInt wc);
HsInt u_iswspace(HsInt wc);
HsInt u_iswprint(HsInt wc);
HsInt u_iswlower(HsInt wc);

HsInt u_iswalnum(HsInt wc);

HsInt u_towlower(HsInt wc);
HsInt u_towupper(HsInt wc);
HsInt u_towtitle(HsInt wc);

HsInt u_gencat(HsInt wc);

#endif

