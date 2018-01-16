/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2011
 *
 * Access to the process environment variables
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "GetEnv.h"

#if defined(darwin_HOST_OS)

/* While the "extern char** environ" var does exist on OSX, it is not
 * available to shared libs. See ghc ticket #2458 and
 * http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
 */
#include <crt_externs.h>

static char** get_environ(void) { return *(_NSGetEnviron()); }

#else

/* On proper unix systems the environ is just a global var.
 */
extern char** environ;
static char** get_environ(void) { return environ; }

#endif


void getProgEnvv(int *out_envc, char **out_envv[]) {
    int envc;
    char **environ = get_environ();

    for (envc = 0; environ[envc] != NULL; envc++) {};

    *out_envc = envc;
    *out_envv = environ;
}

void freeProgEnvv(int envc STG_UNUSED, char *envv[] STG_UNUSED) {
    /* nothing */
}
