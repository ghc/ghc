{-# LANGUAGE GADTs #-}

module Opt where
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

data Opt arr a b where
  Lift  :: arr a b -> Opt arr a b
  First :: Opt arr a b -> Opt arr (a,c) (b,c)

runOpt :: Arrow arr => Opt arr a b -> arr a b
runOpt (Lift f) = f
runOpt (First f) = first (runOpt f)

instance Arrow arr => Category (Opt arr) where
  id = Lift id
  First f . First g = First (f . g)
  f . g = Lift (runOpt f . runOpt g)

instance Arrow arr => Arrow (Opt arr) where
  arr = Lift . arr

  first = First
