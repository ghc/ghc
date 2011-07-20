#include <stdio.h>

void __attribute__((stdcall)) test(int arg)
{
   printf("The argument passed was %i\n", arg );
}
