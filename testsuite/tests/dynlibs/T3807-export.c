
#include <HsFFI.h>

extern void __stginit_T3807Export(void);

void
test_init (void)
{
  static char *argv[] = { "T3807test.so", 0 };
  static char **argv_ = argv;
  static int argc = 1;

  hs_init (&argc, &argv_);
  hs_add_root (__stginit_T3807Export);
}

void
test_exit (void)
{
  hs_exit ();
}
