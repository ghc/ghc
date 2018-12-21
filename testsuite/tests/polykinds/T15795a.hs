{-# Language    RankNTypes       #-}
{-# Language    PolyKinds        #-}
{-# Language    GADTs            #-}

module T15795a where
import Data.Kind

data F :: forall (cat1 :: ob1). ob1 -> Type where
  Prod :: F (a :: u)
