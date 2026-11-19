{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
-- | Select a type from the constructor of a type-level scrutinee.
module NativeCase (F, Field, Same, Capture, Braces, roadmap) where

-- | Choose an ordinary result type from a Boolean.
type F a = case a of
  True -> Int
  False -> Bool

-- | Bind the field of Just in its alternative.
type Field x = case x of
  Just x -> '(x,x)
  Nothing -> '( 'False, 'False)

-- | Keep the case stuck until a constructor is known.
type Same a = case a of
  True -> Int
  False -> Int

-- | Refer to both an enclosing parameter and a pattern-bound field.
type Capture outer input = case input of
  Just x -> '(outer,x)
  Nothing -> '(outer, 'False)

-- | Use explicit braces for the alternatives.
type Braces a = case a of { True -> Int; False -> Bool; }

-- | Observe the two selected result types.
roadmap :: (F 'True, Braces 'False)
roadmap = (7, False)
