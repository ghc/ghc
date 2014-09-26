/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2011
 *
 * OS-independent interface to the process environment variables
 *
 * ---------------------------------------------------------------------------*/

#ifndef GETENV_H
#define GETENV_H

#include "BeginPrivate.h"

/* Get the process environment vector (same style interface as argc/argv)
 */
void getProgEnvv  (int *out_envc, char **out_envv[]);
void freeProgEnvv (int envc, char *envv[]);

/* calls to getProgEnvv must have a corresponding freeProgEnvv */

#include "EndPrivate.h"

#endif /* GETENV_H */
