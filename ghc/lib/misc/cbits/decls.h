#ifndef GHC_CBITS_DECLS_H
#define GHC_CBITS_DECLS_H

StgByteArray	getCPUTime(StgByteArray);
StgInt		getClockTime(StgByteArray, StgByteArray);
StgAddr		showTime(I_, StgByteArray, StgByteArray);
StgAddr		toClockSec(I_, I_, I_, I_, I_, I_, I_, StgByteArray);
StgAddr		toLocalTime(I_, StgByteArray, StgByteArray);
StgAddr		toUTCTime  (I_, StgByteArray, StgByteArray);

#endif
