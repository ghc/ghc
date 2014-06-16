#include <stdio.h>
#include "T5594_stub.h"

#include "HsFFI.h"

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
  hello();
  hs_exit();
  return 0;
}
