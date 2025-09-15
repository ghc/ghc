/* Note that the filename must begin with a lowercase letter, because GHC thinks it as a module name otherwise. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
    .globl _foo
_foo:
#else
    .globl foo
foo:
#endif
    .quad 24839
