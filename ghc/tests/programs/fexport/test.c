#include "For_stub.h"

int
main(int argc, char *argv[])
{
  int i;
  char msg[] = "Hello, world\n";

  startupHaskell(argc,argv);

  for (i=0; i < sizeof(msg) - 1; i++) {
     putChar(msg[i]);
  }
  shutdownHaskell();
}
