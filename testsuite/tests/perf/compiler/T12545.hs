{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T12545 where

import T12545a

data A

type instance ElemsOf A = [ T1, T2, T3, T4, T5, T6, T7, T8
                          , T9, T10, T11, T12, T13, T14, T15, T16
                          , T17, T18, T19, T20, T21, T22, T23, T24
                          , T25, T26, T27, T28, T29, T30, T31, T32
                          ]

data T1; instance ElemOf A T1 where
data T2; instance ElemOf A T2 where
data T3; instance ElemOf A T3 where
data T4; instance ElemOf A T4 where
data T5; instance ElemOf A T5 where
data T6; instance ElemOf A T6 where
data T7; instance ElemOf A T7 where
data T8; instance ElemOf A T8 where
data T9; instance ElemOf A T9 where
data T10; instance ElemOf A T10 where
data T11; instance ElemOf A T11 where
data T12; instance ElemOf A T12 where
data T13; instance ElemOf A T13 where
data T14; instance ElemOf A T14 where
data T15; instance ElemOf A T15 where
data T16; instance ElemOf A T16 where
data T17; instance ElemOf A T17 where
data T18; instance ElemOf A T18 where
data T19; instance ElemOf A T19 where
data T20; instance ElemOf A T20 where
data T21; instance ElemOf A T21 where
data T22; instance ElemOf A T22 where
data T23; instance ElemOf A T23 where
data T24; instance ElemOf A T24 where
data T25; instance ElemOf A T25 where
data T26; instance ElemOf A T26 where
data T27; instance ElemOf A T27 where
data T28; instance ElemOf A T28 where
data T29; instance ElemOf A T29 where
data T30; instance ElemOf A T30 where
data T31; instance ElemOf A T31 where
data T32; instance ElemOf A T32 where
