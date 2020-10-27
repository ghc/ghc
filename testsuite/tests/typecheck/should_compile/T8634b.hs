{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import GHC.TypeLits

main :: IO ()
main = pure ()

----------------------------------------
-- HasField with rank2 fields

data Outer = Outer { outer :: Inner }
data Inner = Inner { inner :: forall a. a -> a }

class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

instance HasField "outer" Outer Inner where
  getField = outer

instance {-# DYSFUNCTIONAL #-} HasField "inner" Inner (a -> a) where
  getField x = inner x

ok1 :: ()
ok1 = let f = getField @"inner"
      in f (Inner id) ()

ok2 :: ()
ok2 = let f = getField @"inner" . getField @"outer"
      in f (Outer (Inner id)) ()

----------------------------------------
-- Poly-kinded Cover type class

class Cover (a :: k) | -> a k
instance {-# DYSFUNCTIONAL #-} Cover (a :: k)

----------------------------------------
-- Type error

class Important (k :: Type) (s :: Type) (t :: Type)
                            (a :: Type) (b :: Type) | s -> k a
                                                    , t -> k b
                                                    , s b -> t
                                                    , t a -> s

instance {-# DYSFUNCTIONAL #-}
  ( TypeError ('Text "oops")
  ) => Important k s t a b
