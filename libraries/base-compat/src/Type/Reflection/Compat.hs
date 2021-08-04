{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,11,0))
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
#endif
module Type.Reflection.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
, withTypeable
#endif
) where

#if MIN_VERSION_base(4,11,0)
import Type.Reflection as Base
#elif MIN_VERSION_base(4,10,0)
import Type.Reflection as Base hiding (withTypeable)
#endif

#if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,11,0))
import GHC.Exts (TYPE)
import Type.Reflection (Typeable, TypeRep)
import Unsafe.Coerce (unsafeCoerce)

-- | Use a 'TypeRep' as 'Typeable' evidence.
withTypeable :: forall (a :: k) (r :: TYPE rep). ()
             => TypeRep a -> (Typeable a => r) -> r
withTypeable rep k = unsafeCoerce k' rep
  where k' :: Gift a r
        k' = Gift k

-- | A helper to satisfy the type checker in 'withTypeable'.
newtype Gift a (r :: TYPE rep) = Gift (Typeable a => r)
#endif
