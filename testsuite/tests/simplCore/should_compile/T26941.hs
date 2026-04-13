{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module T26941 where

import GHC.TypeLits

import T26941_aux ( SMayNat(SKnown), ListH, shxHead )

shsHead :: ListH (Just n : sh) Int -> SNat n
shsHead shx =
  case shxHead shx of
    SKnown SNat -> SNat
