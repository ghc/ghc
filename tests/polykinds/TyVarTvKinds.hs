{-# LANGUAGE PolyKinds #-}

module TyVarTvKinds where

data T (a :: k1) x = MkT (S a ())
data S (b :: k2) y = MkS (T b ())
  -- tests GHC.Tc.TyCl.no_sig_tv
