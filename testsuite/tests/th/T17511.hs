{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T17511 where

import Language.Haskell.TH

f :: $(pure (ForallT [] [TupleT 1 `AppT` (ConT ''Show `AppT` ConT ''Int)] (ConT ''String)))
f = "abc"
