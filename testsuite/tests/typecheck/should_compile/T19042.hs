{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module T19042 where

import Data.Functor
import Data.Typeable

type family N a

data StyledPlot n where
  StyledPlot :: Typeable p => p -> StyledPlot (N p)

styledPlot :: forall p f.
              (Typeable p, Applicative f)
           => (p -> f p)
           -> StyledPlot (N p) -> f (StyledPlot (N p))
styledPlot f s@(StyledPlot p) =
  case eq p of
    Just Refl -> f p <&> \p' -> StyledPlot p'
    Nothing   -> pure s
  where eq :: Typeable a => a -> Maybe (a :~: p)
        eq _ = eqT
