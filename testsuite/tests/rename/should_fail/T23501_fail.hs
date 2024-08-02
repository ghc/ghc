{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module T23501_fail where

import Prelude.Experimental

f1 :: forall _ _. Unit      -- HsOuterTyVarBndrs
f1 = ()

f2 :: (forall _ _. Unit)    -- HsForAllInvis
f2 = ()

f3 :: forall _ a _ -> a -> a
f3 t1 t2 t3 = \x -> x

f4 :: Bool -> Bool
f4 = f3 (forall _ -> Unit) Bool (forall _. Unit)

type family Fd a = (_ :: k) where