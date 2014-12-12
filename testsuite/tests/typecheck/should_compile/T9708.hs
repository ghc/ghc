{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module TcTypeNatSimple where

import GHC.TypeLits
import Data.Proxy

type family SomeFun (n :: Nat)

-- See the Trac ticket; whether this suceeds or fails is distintly random

-- upon creation, commit f861fc6ad8e5504a4fecfc9bb0945fe2d313687c, this failed

-- with Simon's optimization to the flattening algorithm, commit
-- 37b3646c9da4da62ae95aa3a9152335e485b261e, this succeeded

-- with the change to stop Deriveds from rewriting Deriveds (around Dec. 12, 2014),
-- this failed again

ti7 :: (x <= y, y <= x) => Proxy (SomeFun x) -> Proxy y -> ()
ti7 _ _ = ()
