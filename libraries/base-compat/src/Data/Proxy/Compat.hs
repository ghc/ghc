{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Proxy.Compat (
#if MIN_VERSION_base(4,7,0)
  module Base,
#endif
  asProxyTypeOf
) where

#if MIN_VERSION_base(4,7,0)
# if MIN_VERSION_base(4,10,0)
import Data.Proxy as Base
# else
import Data.Proxy as Base hiding (asProxyTypeOf)
# endif
#endif

#if !(MIN_VERSION_base(4,10,0))
import Prelude (const)

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
#endif
