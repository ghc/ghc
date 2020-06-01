#include "Rts.h"
#include "RtsAPI.h"

StgTSO* create_tso(){
    HaskellObj trueClosure = rts_mkBool(&MainCapability, 1);

    StgTSO * tso = createGenThread(&MainCapability, 500U, trueClosure);

    return tso;
}
