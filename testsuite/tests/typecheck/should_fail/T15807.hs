{-# Language RankNTypes       #-}
{-# Language TypeApplications #-}
{-# Language PolyKinds        #-}
{-# Language GADTs            #-}

module T15807 where
import Data.Kind

data
  App :: forall (f :: k -> Type). k -> Type
  where
  MkApp :: f a -> App @f a
