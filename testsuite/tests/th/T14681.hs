{-# LANGUAGE TemplateHaskell #-}
module T14681 where

import Data.Functor.Identity
import Language.Haskell.TH

$([d| f = \(Identity x) -> x |])
$([d| g = $(pure $ VarE '(+) `AppE` LitE (IntegerL (-1))
                             `AppE` (LitE (IntegerL (-1)))) |])
