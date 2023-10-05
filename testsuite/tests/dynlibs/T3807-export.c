
#include <HsFFI.h>

void
test_init (void)
{
  static char *argv[] = { "T3807test.so", 0 };
  static char **argv_ = argv;
  static int argc = 1;

  hs_init (&argc, &argv_);
}

void
test_exit (void)
{
  hs_exit ();
}
