{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
-- | This module provides
--
-- * specialised versions of class members e.g. 'bitraverseThese'
-- * non-lens variants of "Data.These.Lens" things, e.g 'justHere'
module Data.These.Combinators (
    -- * Specialised combinators
    -- ** Bifunctor
    bimapThese,
    mapHere,
    mapThere,
    -- ** Bitraversable
    bitraverseThese,
    -- ** Associativity and commutativity
    swapThese,
    assocThese,
    unassocThese,

    -- * Other operations
    -- ** preview
    --
    -- |
    -- @
    -- 'justThis'  = 'Control.Lens.preview' '_This'
    -- 'justThat'  = 'Control.Lens.preview' '_That'
    -- 'justThese' = 'Control.Lens.preview' '_These'
    -- 'justHere'  = 'Control.Lens.preview' 'here'
    -- 'justThere' = 'Control.Lens.preview' 'there'
    -- @
    justThis,
    justThat,
    justThese,
    justHere,
    justThere,

    -- ** toListOf
    --
    -- |
    -- @
    -- 'catThis'  = 'Control.Lens.toListOf' ('Control.Lens.folded' . '_This')
    -- 'catThat'  = 'Control.Lens.toListOf' ('Control.Lens.folded' . '_That')
    -- 'catThese' = 'Control.Lens.toListOf' ('Control.Lens.folded' . '_These')
    -- 'catHere'  = 'Control.Lens.toListOf' ('Control.Lens.folded' . 'here')
    -- 'catThere' = 'Control.Lens.toListOf' ('Control.Lens.folded' . 'there')
    -- @
    catThis,
    catThat,
    catThese,
    catHere,
    catThere,

    -- * is / has
    --
    -- |
    -- @
    -- 'isThis'   = 'Control.Lens.Extra.is' '_This'
    -- 'isThat'   = 'Control.Lens.Extra.is' '_That'
    -- 'isThese'  = 'Control.Lens.Extra.is' '_These'
    -- 'hasHere'  = 'Control.Lens.has' 'here'
    -- 'hasThere' = 'Control.Lens.has' 'there'
    -- @
    isThis,
    isThat,
    isThese,
    hasHere,
    hasThere,

    -- * over / map
    --
    -- @
    -- 'mapThis'  = 'Control.Lens.over' '_This'
    -- 'mapThat'  = 'Control.Lens.over' '_That'
    -- 'mapThese' = 'Control.Lens.over' '_These'
    -- 'mapHere'  = 'Control.Lens.over' 'here'
    -- 'mapThere' = 'Control.Lens.over' 'there'
    -- @
    mapThis,
    mapThat,
    mapThese,
    ) where

import Control.Applicative (Applicative (..))
import Data.Bifunctor      (bimap, first, second)
import Data.Bitraversable  (bitraverse)
import Data.Maybe          (isJust, mapMaybe)
import Data.These
import Prelude             (Bool (..), Maybe (..), curry, uncurry, (.))

#ifdef MIN_VERSION_assoc
import Data.Bifunctor.Assoc (assoc, unassoc)
import Data.Bifunctor.Swap  (swap)
#endif

-------------------------------------------------------------------------------
-- bifunctors
-------------------------------------------------------------------------------

-- | 'Bifunctor' 'bimap'.
bimapThese :: (a -> c) -> (b -> d) -> These a b -> These c d
bimapThese = bimap

-- | @'mapHere' = 'Control.Lens.over' 'here'@
mapHere :: (a -> c) -> These a b -> These c b
mapHere = first

-- | @'mapThere' = 'Control.Lens.over' 'there'@
mapThere :: (b -> d) -> These a b -> These a d
mapThere = second

-- | 'Bitraversable' 'bitraverse'.
bitraverseThese :: Applicative f => (a -> f c) -> (b -> f d) -> These a b -> f (These c d)
bitraverseThese = bitraverse

-------------------------------------------------------------------------------
-- assoc
-------------------------------------------------------------------------------

-- | 'These' is commutative.
--
-- @
-- 'swapThese' . 'swapThese' = 'id'
-- @
--
-- @since 0.8
swapThese :: These a b -> These b a
#ifdef MIN_VERSION_assoc
swapThese = swap
#else
swapThese (This a)    = That a
swapThese (That b)    = This b
swapThese (These a b) = These b a
#endif

-- | 'These' is associative.
--
-- @
-- 'assocThese' . 'unassocThese' = 'id'
-- 'unassocThese' . 'assocThese' = 'id'
-- @
--
-- @since 0.8
assocThese :: These (These a b) c -> These a (These b c)
#ifdef MIN_VERSION_assoc
assocThese = assoc
#else
assocThese (This (This a))       = This a
assocThese (This (That b))       = That (This b)
assocThese (That c)              = That (That c)
assocThese (These (That b) c)    = That (These b c)
assocThese (This (These a b))    = These a (This b)
assocThese (These (This a) c)    = These a (That c)
assocThese (These (These a b) c) = These a (These b c)
#endif

-- | 'These' is associative. See 'assocThese'.
--
-- @since 0.8
unassocThese :: These a (These b c) -> These (These a b) c
#ifdef MIN_VERSION_assoc
unassocThese = unassoc
#else
unassocThese (This a)              = This (This a)
unassocThese (That (This b))       = This (That b)
unassocThese (That (That c))       = That c
unassocThese (That (These b c))    = These (That b) c
unassocThese (These a (This b))    = This (These a b)
unassocThese (These a (That c))    = These (This a) c
unassocThese (These a (These b c)) = These (These a b) c
#endif

-------------------------------------------------------------------------------
-- preview
-------------------------------------------------------------------------------

-- |
--
-- >>> justHere (This 'x')
-- Just 'x'
--
-- >>> justHere (That 'y')
-- Nothing
--
-- >>> justHere (These 'x' 'y')
-- Just 'x'
--
justHere :: These a b -> Maybe a
justHere (This a)    = Just a
justHere (That _)    = Nothing
justHere (These a _) = Just a

-- |
--
-- >>> justThere (This 'x')
-- Nothing
--
-- >>> justThere (That 'y')
-- Just 'y'
--
-- >>> justThere (These 'x' 'y')
-- Just 'y'
--
justThere :: These a b -> Maybe b
justThere (This _)    = Nothing
justThere (That b)    = Just b
justThere (These _ b) = Just b

justThis :: These a b -> Maybe a
justThis (This a) = Just a
justThis _        = Nothing

justThat :: These a b -> Maybe b
justThat (That x) = Just x
justThat _        = Nothing

justThese :: These a b -> Maybe (a, b)
justThese (These a x) = Just (a, x)
justThese _           = Nothing

-------------------------------------------------------------------------------
-- toListOf
-------------------------------------------------------------------------------

-- | Select all 'This' constructors from a list.
catThis :: [These a b] -> [a]
catThis = mapMaybe justThis

-- | Select all 'That' constructors from a list.
catThat :: [These a b] -> [b]
catThat = mapMaybe justThat

-- | Select all 'These' constructors from a list.
catThese :: [These a b] -> [(a, b)]
catThese = mapMaybe justThese

catHere :: [These a b] -> [a]
catHere = mapMaybe justHere

catThere :: [These a b] -> [b]
catThere = mapMaybe justThere

-------------------------------------------------------------------------------
-- is
-------------------------------------------------------------------------------

isThis, isThat, isThese :: These a b -> Bool
-- | @'isThis' = 'isJust' . 'justThis'@
isThis  = isJust . justThis

-- | @'isThat' = 'isJust' . 'justThat'@
isThat  = isJust . justThat

-- | @'isThese' = 'isJust' . 'justThese'@
isThese = isJust . justThese

hasHere, hasThere :: These a b -> Bool
-- | @'hasHere' = 'isJust' . 'justHere'@
hasHere = isJust . justHere

-- | @'hasThere' = 'isJust' . 'justThere'@
hasThere = isJust . justThere

-------------------------------------------------------------------------------
-- over / map
-------------------------------------------------------------------------------

mapThis :: (a -> a) -> These a b -> These a b
mapThis f (This x) = This (f x)
mapThis _ y        = y

mapThat :: (b -> b) -> These a b -> These a b
mapThat f (That x) = That (f x)
mapThat _ y        = y

mapThese :: ((a, b) -> (a, b)) -> These a b -> These a b
mapThese f (These x y) = uncurry These (curry f x y)
mapThese _ z           = z
