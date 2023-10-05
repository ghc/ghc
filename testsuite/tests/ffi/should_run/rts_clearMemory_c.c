#include <Rts.h>
#include "rts_clearMemory_stub.h"

int main(int argc, char *argv[]) {
  hs_init_with_rtsopts(&argc, &argv);

  for (int i = 0; i < 8; ++i) {
    foo(1000000);
    hs_perform_gc();
    rts_clearMemory();
  }
}
