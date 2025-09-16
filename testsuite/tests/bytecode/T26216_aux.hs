{-# LANGUAGE GHC2024         #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds       #-}

module T26216_aux (Some, data Some, mkSome, withSome) where
import Data.Kind           (Type)
import GHC.Exts            (Any)
import Unsafe.Coerce       (unsafeCoerce)

type Some :: (k -> Type) -> Type
newtype Some tag = UnsafeSome (tag Any)
type role Some representational

{-# COMPLETE Some #-}
pattern Some :: tag a -> Some tag
pattern Some x <- UnsafeSome x
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome = \x -> UnsafeSome (unsafeCoerce x)

-- | Eliminator.
withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (UnsafeSome thing) some = some (unsafeCoerce thing)
