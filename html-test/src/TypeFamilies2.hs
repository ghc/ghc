{-# LANGUAGE TypeFamilies #-}
-- This tests what happens if we have unexported types
-- in type instances. The expected behaviour is
-- that we get the instance, Y is not linked and
-- Haddock shows a linking warning.
--
-- The other families and instances that are not exported should not
-- show up at all
module TypeFamilies2 (W, Foo, Bar) where

-- | Exported type
data W

-- | Hidden type
data Z

-- | Exported type family
type family Foo a

-- | Should be visible, but with a hidden right hand side
type instance Foo W = Z

-- | Should be hidden
type instance Foo Z = W

-- | Exported data family
data family Bar a

-- | Shown because BarX is still exported despite Z being hidden
data instance Bar W = BarX Z

-- | Should be completely invisible, including instances
type family Invisible a
type instance Invisible W = Z
type instance Invisible Z = W

data family Invisible2 a
data instance Invisible2 W = Invis  Z
data instance Invisible2 Z = Invis' W
