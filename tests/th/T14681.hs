{-# LANGUAGE TemplateHaskell, MagicHash #-}
module T14681 where

import Data.Functor.Identity
import Language.Haskell.TH
import GHC.Exts

$([d| f = \(Identity x) -> x |])
$([d| g = $(pure $ VarE '(+) `AppE` LitE (IntegerL (-1))
                             `AppE` (LitE (IntegerL (-1)))) |])
$([d| h _ = $(pure $ VarE '(+#) `AppE` LitE (IntPrimL (-1))
                                `AppE` (LitE (IntPrimL (-1)))) |])
