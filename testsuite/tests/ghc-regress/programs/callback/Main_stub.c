#define IN_STG_CODE 0
#include "Stg.h"
#include "HsStd.h"
#include "HsLang.h"
#include "RtsAPI.h"
extern HaskellObj Main_zdfmkCounter_closure;
HsInt Main_d11Y(HsStablePtr a0, HsAddr a_)
{
SchedulerStatus rc;
HaskellObj ret;
rc=rts_evalIO(rts_apply((StgClosure*)&Main_zdfmkCounter_closure,rts_mkStablePtr(a0)),&ret);
rts_checkSchedStatus("Main_d11Y",rc);
return(rts_getInt(ret));
}