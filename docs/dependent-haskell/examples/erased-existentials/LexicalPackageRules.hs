{-# LANGUAGE ExperimentalDependentTypes #-}

module LexicalPackageRules (readHidden, hideInt, project, increment, local) where

import PackageRules (Hidden)

-- | Apply the stored callback to the value of its input type.
{-# NOINLINE readHidden #-}
readHidden :: Hidden -> Int
readHidden (x, k) = k x

-- | Store an integer with the identity callback.
{-# NOINLINE hideInt #-}
hideInt :: Int -> Hidden
hideInt x = (x, id)

-- | Read the integer from any package.
{-# NOINLINE project #-}
project :: Hidden -> Int
project p = readHidden p

-- | Add one to the observed result.
{-# NOINLINE increment #-}
increment :: Int -> Int
increment x = x + 1

{-# RULES "read/int-pair" forall (x :: Int) (k :: Int -> Int). readHidden (x, k) = k x #-}
{-# RULES "hide/int" forall (x :: Int). hideInt x = ((x, privateIdentity) :: Hidden) #-}
{-# RULES "project/hidden" forall (p :: Hidden). project p = case p of (x, k) -> k x #-}
{-# RULES "increment/hidden" forall (p :: Hidden).
      increment (case p of (x, k) -> k x) = project p + 1 #-}

-- | Identity callback referenced only by the exported packing rule.
{-# NOINLINE privateIdentity #-}
privateIdentity :: Int -> Int
privateIdentity x = x

-- | Apply the rules in the module that defines them.
local :: Int -> Hidden -> (Int, Int, Int)
local x p = (readHidden (x, (+ 1)), project (hideInt x),
             increment (case p of (v, f) -> f v))
