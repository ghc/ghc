{-# LANGUAGE TypeFamilies, RankNTypes #-}

-- This test made the type checker produce an
-- ill-kinded coercion term.

module Roman where

import Control.Monad.ST
import Data.Kind (Type)

type family Mut (v :: Type -> Type) :: Type -> Type -> Type
type family State (m :: Type -> Type)
type instance State (ST s) = s

unsafeFreeze :: Mut v (State (ST s)) a -> ST s (v a)
unsafeFreeze = undefined

new :: (forall v s. ST s (v s a)) -> v a
new p = runST (do
                 mv <- p
                 unsafeFreeze mv)

---------------------------------------------
-- Here's a simpler version that also failed

type family FMut :: Type -> Type      -- No args
                                      -- Same thing happens with one arg

type family   FState (m :: Type)
type instance FState Char = Int

funsafeFreeze :: FMut (FState Char) -> ()
funsafeFreeze = undefined

flop :: forall mv. mv Int
flop = undefined

noo =  flop `rapp` funsafeFreeze

rapp :: a -> (a->()) -> ()
rapp arg fun = fun arg

