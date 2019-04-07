{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints #-}
module TcTypeNatSimple where

import GHC.TypeLits
import Data.Proxy

type family SomeFun (n :: Nat)

-- See the ticket; whether this succeeds or fails is distinctly random

-- upon creation, commit f861fc6ad8e5504a4fecfc9bb0945fe2d313687c, this failed

-- with Simon's optimization to the flattening algorithm, commit
-- 37b3646c9da4da62ae95aa3a9152335e485b261e, this succeeded

-- with the change to stop Deriveds from rewriting Deriveds (around Dec. 12, 2014),
-- this failed again

-- 2016-01-23: it just started passing again, when
-- -fwarn-redundant-constraints was removed from the default warning set.
-- Turning the warning back on for this module, ghc reports (and probably has
-- for some time):
--      Redundant constraints: (x <= y, y <= x)
--      In the type signature for:
--            ti7 :: (x <= y, y <= x) => Proxy (SomeFun x) -> Proxy y -> ()

ti7 :: (x <= y, y <= x) => Proxy (SomeFun x) -> Proxy y -> ()
ti7 _ _ = ()
