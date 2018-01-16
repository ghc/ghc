#include "unboxery.h"

module Types where

#ifdef STR
#define S {-# STRICT #-}
#else
#define S
#endif

type FloatT = FLOAT_TY

#define FloatS FloatT S

data Pt = Pt FloatS FloatS FloatS

data Tfo = Tfo FloatS FloatS FloatS FloatS FloatS FloatS FloatS FloatS FloatS FloatS FloatS FloatS

-- These would be much better --SDM
-- data Pt = Pt !Float !Float !Float
-- data Tfo = Tfo !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float


data Nuc = Nuc
          Tfo Tfo Tfo Tfo
          Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt
          Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt Pt
          Nuc_specific

data Nuc_specific
      = A Pt Pt Pt Pt Pt Pt Pt Pt
      | C Pt Pt Pt Pt Pt Pt
      | G Pt Pt Pt Pt Pt Pt Pt Pt Pt
      | U Pt Pt Pt Pt Pt

-- A n6 n7 n9 c8 h2 h61 h62 h8
-- C n4 o2 h41 h42 h5 h6
-- G n2 n7 n9 c8 o6 h1 h21 h22 h8
-- U o2 o4 h3 h5 h6

