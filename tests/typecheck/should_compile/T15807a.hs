{-# Language RankNTypes       #-}
{-# Language TypeApplications #-}
{-# Language PolyKinds        #-}
{-# Language GADTs            #-}

module T15807a where
import Data.Kind

data
  App :: forall (f :: Type -> Type). Type -> Type
  where
  MkApp :: f a -> App @f a
