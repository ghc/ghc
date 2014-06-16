#include <stdio.h>

#include "HsFFI.h"

#include "fptr01.h"

void f( HsInt *i )
{
    printf( "f%d\n", (int)*i );
    fflush( stdout );
}

void g( HsInt *i )
{
    printf( "g%d\n", (int)*i );
    fflush( stdout );
}

void h( HsInt *i )
{
    printf( "h%d\n", (int)*i );
    fflush( stdout );
}

void f_env( HsInt *env, HsInt *i )
{
    printf( "f_env %d %d\n", *env, (int)*i );
    fflush( stdout );
}
