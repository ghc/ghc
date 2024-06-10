{-# LANGUAGE TemplateHaskell, PartialTypeSignatures #-}
module T24769c_aux where

import Data.Kind
import GHC.Exts
import Language.Haskell.TH (Code, Q)

g :: Code Q _
g = [||let f :: forall (r :: Type -> RuntimeRep) s (a :: TYPE (r s)). () -> a
           f = f
           in f () ||]
