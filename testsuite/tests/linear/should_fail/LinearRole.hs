{-# LANGUAGE LinearTypes, GADTs, DataKinds #-}

module LinearRole where -- #18799

import GHC.Types (Multiplicity(..))
import Data.Coerce

data T (m :: Multiplicity) a where
  MkT :: a %m -> T m a

f :: T 'One a -> T 'Many a
f x = coerce x
