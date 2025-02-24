{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes, NoTypeAbstractions #-}
{-# LANGUAGE TemplateHaskell #-}

module T17594b_th where

import qualified Language.Haskell.TH as TH

id10 :: a -> a
id10 @($(TH.varT (TH.mkName "t"))) x = x :: t
