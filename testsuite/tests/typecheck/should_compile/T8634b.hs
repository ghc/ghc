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

instance {-# COVERED a #-} HasField "inner" Inner (a -> a) where
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
instance {-# COVERED a k #-} Cover (a :: k)
