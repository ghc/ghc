#include <stdio.h>
#include "foo_stub.h"

#include "RtsAPI.h"

extern void __init_Foo ( void );

int main(void)
{
  int i;

  char* bogusFlags[1] = { "\0" };

  startupHaskell(0, bogusFlags, __init_Foo);

  for (i = 0; i < 5; i++) {
    printf("%d\n", foo(2500));
  }

  shutdownHaskell();

  return 0;
}
