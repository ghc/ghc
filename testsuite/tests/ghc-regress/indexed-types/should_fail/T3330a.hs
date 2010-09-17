{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- A very bogus program (multiple errors) but
-- sent GHC 6.12 into a loop

module T3330a where

import Control.Monad.Writer

data AnyF (s :: * -> *) = AnyF
class HFunctor (f :: (* -> *) -> * -> *)
type family PF (phi :: * -> *) :: (* -> *) -> * -> *

children :: s ix -> (PF s) r ix -> [AnyF s]
children p x = execWriter (hmapM p collect x)

collect :: HFunctor (PF s) => s ix -> r ix -> Writer [AnyF s] (r ix)
collect = undefined

hmapM :: (forall ix. phi ix -> r ix -> m (r' ix))
      -> phi ix -> f r ix -> m (f r' ix)
hmapM = undefined

