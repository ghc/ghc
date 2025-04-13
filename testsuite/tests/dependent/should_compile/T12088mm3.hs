{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module T12088mm3 where

import Data.Kind
import T12088mm3_helper

type instance Open1 () = Bool
type instance Open2 () 'True = Char

