{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}

module T12668 where

import GHC.Exts

data Some r = Some (TYPE r -> TYPE r)

doSomething :: forall (r :: RuntimeRep). forall (a :: TYPE r). ()
            => Int -> (a -> Int) -> a -> a
doSomething n f =
    case n of
      1 -> error "hello"
      3 -> error "hello"
