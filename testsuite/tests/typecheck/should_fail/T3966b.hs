{-# OPTIONS -Werror=unusable-unpack-pragmas #-}
{-# LANGUAGE TypeFamilies #-}

module T3966b where

data T = MkT {-# UNPACK #-} !(Int -> Bool)

data G where
  MkG :: {-# UNPACK #-} !G -> G

type family F a where {}
data R a = MkR { fld :: {-# UNPACK #-} !(F a) }
