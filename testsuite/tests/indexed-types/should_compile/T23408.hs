{-# LANGUAGE TypeFamilies, TypeApplications, GADTs, FunctionalDependencies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module T23408 where

import Data.Coerce
import Data.Proxy

f :: Proxy a -> Key a -> Maybe ()
f _ _ = Nothing

g :: Key a -> Proxy a -> Maybe ()
g _ _ = Nothing

data User

data family Key a

newtype instance Key User = UserKey String

class Convert lhs result where
    convert :: Proxy lhs -> Proxy result

instance (rec ~ rec') => Convert rec rec' where
    convert _ = Proxy

a :: Maybe ()
a = f (convert @User Proxy) (coerce "asdf")

{- Typechecking `a`

   convert @User Proxy :: Proxy alpha
   [W] Convert User alpha
   coerce "asdf" :: Key alpha
   [W] Coercible String (Key alpha)

   Solve [W] Convert User alpha  ==>  [W] User ~ alpha
   [W] Coercible String (Key User)
-}

b :: Maybe ()
b = g (coerce "asdf") (convert @User Proxy)

