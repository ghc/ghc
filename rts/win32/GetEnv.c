/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2011
 *
 * Access to the process environment variables
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "GetEnv.h"

#include <windows.h>

/* Windows does it differently, though arguably the most sanely.
 * GetEnvironmentStrings() returns a pointer to a block of
 * environment vars with a double null terminator:
 *   Var1=Value1\0
 *   Var2=Value2\0
 *   ...
 *   VarN=ValueN\0\0
 * But because everyone else (ie POSIX) uses a vector of strings, we convert
 * to that format. Fortunately this is just a matter of making an array of
 * offsets into the environment block.
 *
 * Note that we have to call FreeEnvironmentStrings() at the end.
 *
 */
void getProgEnvv(int *out_envc, char **out_envv[]) {
    int envc, i;
    char *env;
    char *envp;
    char **envv;

    /* For now, use the 'A'nsi not 'W'ide variant.
       Note: corresponding Free below must use the same 'A'/'W' variant. */
    env = GetEnvironmentStringsA();

    envc = 0;
    for (envp = env; *envp != 0; envp += strlen(envp) + 1) {
        envc++;
    }

    envv = stgMallocBytes(sizeof(char*) * (envc+1), "getProgEnvv");

    i = 0;
    for (envp = env; *envp != 0; envp += strlen(envp) + 1) {
        envv[i] = envp;
        i++;
    }
    /* stash whole env in last+1 entry */
    envv[envc] = env;

    *out_envc = envc;
    *out_envv = envv;
}

void freeProgEnvv(int envc, char *envv[]) {
    /* we stashed the win32 env block in the last+1 entry */
    FreeEnvironmentStringsA(envv[envc]);
    stgFree(envv);
}
