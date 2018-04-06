#include "HsBase.h"
#if defined(HAVE_UNSETENV)
int __hsbase_unsetenv(const char *name) {
#if defined(UNSETENV_RETURNS_VOID)
    unsetenv(name);
    return 0;
#else
    return unsetenv(name);
#endif
}
#endif
