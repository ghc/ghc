#include <stdio.h>
#include "ffi002_stub.h"

#include "RtsAPI.h"

extern void __stginit_Foo ( void );

int main(int argc, char *argv[])
{
  int i;

  startupHaskell(argc, argv, __stginit_Foo);

  for (i = 0; i < 5; i++) {
    printf("%d\n", foo(2500));
  }

  shutdownHaskell();

  return 0;
}
