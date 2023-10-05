{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

module T23147 where

import qualified Control.Monad as M
import Prelude hiding (return, (>>=))

type Exis f = (forall r. (forall t. f t -> r) -> r)

data Indexed t where
    Indexed :: Indexed Int

(>>=) :: Monad m => m (Exis f) -> (forall t. f t -> m (Exis g)) -> m (Exis g)
x >>= f = x M.>>= (\x' -> x' f)

return :: Monad m => Exis f -> m (Exis f)
return = M.return

test :: (Monad m) => Exis Indexed -> m (Exis Indexed)
test x =
  T23147.do
    (reified :: Indexed t) <- return x
    return (\g -> g reified)
