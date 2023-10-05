{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module T18170a where

import Language.Haskell.TH.Lib

import T18170c

import Data.Proxy
import GHC.Exts
import GHC.Types


-- Generating a monomorphic program is sound, and hence a workaround for
-- normal representation polymorphic restrictions.

workaround1 :: Int
workaround1 = $$repPolyApp id 5

workaround2 :: () -> Int#
workaround2 _ = $$repPolyApp ( \ (x :: Int#) -> x +# 6# ) 11#
