{-# LANGUAGE MagicHash, TypeInType, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O #-}
module T13458 where
import GHC.Exts
import Data.Kind
import Unsafe.Coerce

unsafeCoerce' :: forall (r :: RuntimeRep)
                       (a :: TYPE r) (b :: TYPE r).
                a -> b
unsafeCoerce' = unsafeCoerce id
