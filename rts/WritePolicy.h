/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Write-barrier globalisation policy
 *
 * ---------------------------------------------------------------------------*/

#ifndef WRITE_POLICY_H
#define WRITE_POLICY_H

#define UPDATE_GLOBALISE   globalise_
#define MUT_VAR_GLOBALISE  globaliseFull_
#define MUT_ARR_GLOBALISE  globaliseFull_
#define MVAR_GLOBALISE     globaliseFull_
#define TVAR_GLOBALISE     globaliseFull_
#define SPARK_GLOBALISE    globaliseFull_
#define MSG_GLOB_GLOBALISE globaliseFull_

#endif /* WRITE_POLICY_H */
