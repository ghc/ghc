{-# LANGUAGE TemplateHaskell #-}

module Td where

import Language.Haskell.TH

-- we actually want this example to work
-- but currently invisible patterns aren't allowed
-- inside a pattern splices

f :: forall a. a -> a
f $(invisP (varT (mkName "t"))) x = x :: t
