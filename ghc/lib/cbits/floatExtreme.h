#ifndef FLOATEXTREME_H
#define FLOATEXTREME_H

StgInt isDoubleNaN            PROTO((StgDouble));
StgInt isDoubleInfinite       PROTO((StgDouble));
StgInt isDoubleDenormalized   PROTO((StgDouble));
StgInt isDoubleNegativeZero   PROTO((StgDouble));
StgInt isFloatNaN             PROTO((StgFloat));
StgInt isFloatInfinite        PROTO((StgFloat));
StgInt isFloatDenormalized    PROTO((StgFloat));
StgInt isFloatNegativeZero    PROTO((StgFloat));

#endif /* FLOATEXTREME_H */
