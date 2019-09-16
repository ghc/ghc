{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module CustomTypeErrors06 where

import GHC.TypeLits


class Foo a where
    foo :: a
    default foo :: TypeError
        (Text "Please define foo manually. Suggested definitions:" :$$:
         Text "  foo = something complicated" :$$:
         Text "  foo = something else complicated")
        => a
    foo = error "unreachable"


instance Foo Int

