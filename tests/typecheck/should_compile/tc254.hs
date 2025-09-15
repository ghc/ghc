{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-missing-methods #-}
module AssocTyDef01 where

class Cls a where
    type Typ a

instance Cls Int where
    -- No default: should get warning