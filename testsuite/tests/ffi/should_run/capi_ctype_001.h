
#ifndef __capi_ctype_001_H__
#define __capi_ctype_001_H__

typedef struct {
    int i;
    int j;
    int k;
} Foo;

int f(Foo *p);

#define g(p) p->j

#endif

