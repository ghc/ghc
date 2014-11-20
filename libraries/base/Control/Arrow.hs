{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow
-- Copyright   :  (c) Ross Paterson 2002
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Basic arrow definitions, based on
--
--  * /Generalising Monads to Arrows/, by John Hughes,
--    /Science of Computer Programming/ 37, pp67-111, May 2000.
--
-- plus a couple of definitions ('returnA' and 'loop') from
--
--  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
--    Firenze, Italy, pp229-240.
--
-- These papers and more information on arrows can be found at
-- <http://www.haskell.org/arrows/>.

module Control.Arrow (
    -- * Arrows
    Arrow(..), Kleisli(..),
    -- ** Derived combinators
    returnA,
    (^>>), (>>^),
    (>>>), (<<<), -- reexported
    -- ** Right-to-left variants
    (<<^), (^<<),
    -- * Monoid operations
    ArrowZero(..), ArrowPlus(..),
    -- * Conditionals
    ArrowChoice(..),
    -- * Arrow application
    ArrowApply(..), ArrowMonad(..), leftApp,
    -- * Feedback
    ArrowLoop(..)
    ) where

import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 ^>>, >>^
infixr 1 ^<<, <<^

-- | The basic arrow class.
--
-- Instances should satisfy the following laws:
--
--  * @'arr' id = 'id'@
--
--  * @'arr' (f >>> g) = 'arr' f >>> 'arr' g@
--
--  * @'first' ('arr' f) = 'arr' ('first' f)@
--
--  * @'first' (f >>> g) = 'first' f >>> 'first' g@
--
--  * @'first' f >>> 'arr' 'fst' = 'arr' 'fst' >>> f@
--
--  * @'first' f >>> 'arr' ('id' *** g) = 'arr' ('id' *** g) >>> 'first' f@
--
--  * @'first' ('first' f) >>> 'arr' 'assoc' = 'arr' 'assoc' >>> 'first' f@
--
-- where
--
-- > assoc ((a,b),c) = (a,(b,c))
--
-- The other combinators have sensible default definitions,
-- which may be overridden for efficiency.

class Category a => Arrow a where

    -- | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    -- | Send the first component of the input through the argument
    --   arrow, and copy the rest unchanged to the output.
    first :: a b c -> a (b,d) (c,d)

    -- | A mirror image of 'first'.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    second :: a b c -> a (d,b) (d,c)
    second f = arr swap >>> first f >>> arr swap
      where
        swap :: (x,y) -> (y,x)
        swap ~(x,y) = (y,x)

    -- | Split the input between the two argument arrows and combine
    --   their output.  Note that this is in general not a functor.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    f *** g = first f >>> second g

    -- | Fanout: send the input to both argument arrows and combine
    --   their output.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (&&&) :: a b c -> a b c' -> a b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> f *** g

{-# RULES
"compose/arr"   forall f g .
                (arr f) . (arr g) = arr (f . g)
"first/arr"     forall f .
                first (arr f) = arr (first f)
"second/arr"    forall f .
                second (arr f) = arr (second f)
"product/arr"   forall f g .
                arr f *** arr g = arr (f *** g)
"fanout/arr"    forall f g .
                arr f &&& arr g = arr (f &&& g)
"compose/first" forall f g .
                (first f) . (first g) = first (f . g)
"compose/second" forall f g .
                (second f) . (second g) = second (f . g)
 #-}

-- Ordinary functions are arrows.

instance Arrow (->) where
    arr f = f
    first f = f *** id
    second f = id *** f
--  (f *** g) ~(x,y) = (f x, g y)
--  sorry, although the above defn is fully H'98, nhc98 can't parse it.
    (***) f g ~(x,y) = (f x, g y)

-- | Kleisli arrows of a monad.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (\b -> g b >>= f)

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
    second (Kleisli f) = Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))

-- | The identity arrow, which plays the role of 'return' in arrow notation.
returnA :: Arrow a => a b b
returnA = arr id

-- | Precomposition with a pure function.
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

-- | Postcomposition with a pure function.
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a

class Arrow a => ArrowZero a where
    zeroArrow :: a b c

instance MonadPlus m => ArrowZero (Kleisli m) where
    zeroArrow = Kleisli (\_ -> mzero)

-- | A monoid on arrows.
class ArrowZero a => ArrowPlus a where
    -- | An associative operation with identity 'zeroArrow'.
    (<+>) :: a b c -> a b c -> a b c

instance MonadPlus m => ArrowPlus (Kleisli m) where
    Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

-- | Choice, for arrows that support it.  This class underlies the
-- @if@ and @case@ constructs in arrow notation.
--
-- Instances should satisfy the following laws:
--
--  * @'left' ('arr' f) = 'arr' ('left' f)@
--
--  * @'left' (f >>> g) = 'left' f >>> 'left' g@
--
--  * @f >>> 'arr' 'Left' = 'arr' 'Left' >>> 'left' f@
--
--  * @'left' f >>> 'arr' ('id' +++ g) = 'arr' ('id' +++ g) >>> 'left' f@
--
--  * @'left' ('left' f) >>> 'arr' 'assocsum' = 'arr' 'assocsum' >>> 'left' f@
--
-- where
--
-- > assocsum (Left (Left x)) = Left x
-- > assocsum (Left (Right y)) = Right (Left y)
-- > assocsum (Right z) = Right (Right z)
--
-- The other combinators have sensible default definitions, which may
-- be overridden for efficiency.

class Arrow a => ArrowChoice a where

    -- | Feed marked inputs through the argument arrow, passing the
    --   rest through unchanged to the output.
    left :: a b c -> a (Either b d) (Either c d)

    -- | A mirror image of 'left'.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    right :: a b c -> a (Either d b) (Either d c)
    right f = arr mirror >>> left f >>> arr mirror
      where
        mirror :: Either x y -> Either y x
        mirror (Left x) = Right x
        mirror (Right y) = Left y

    -- | Split the input between the two argument arrows, retagging
    --   and merging their outputs.
    --   Note that this is in general not a functor.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
    f +++ g = left f >>> right g

    -- | Fanin: Split the input between the two argument arrows and
    --   merge their outputs.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (|||) :: a b d -> a c d -> a (Either b c) d
    f ||| g = f +++ g >>> arr untag
      where
        untag (Left x) = x
        untag (Right y) = y

{-# RULES
"left/arr"      forall f .
                left (arr f) = arr (left f)
"right/arr"     forall f .
                right (arr f) = arr (right f)
"sum/arr"       forall f g .
                arr f +++ arr g = arr (f +++ g)
"fanin/arr"     forall f g .
                arr f ||| arr g = arr (f ||| g)
"compose/left"  forall f g .
                left f . left g = left (f . g)
"compose/right" forall f g .
                right f . right g = right (f . g)
 #-}

instance ArrowChoice (->) where
    left f = f +++ id
    right f = id +++ f
    f +++ g = (Left . f) ||| (Right . g)
    (|||) = either

instance Monad m => ArrowChoice (Kleisli m) where
    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    Kleisli f ||| Kleisli g = Kleisli (either f g)

-- | Some arrows allow application of arrow inputs to other inputs.
-- Instances should satisfy the following laws:
--
--  * @'first' ('arr' (\\x -> 'arr' (\\y -> (x,y)))) >>> 'app' = 'id'@
--
--  * @'first' ('arr' (g >>>)) >>> 'app' = 'second' g >>> 'app'@
--
--  * @'first' ('arr' (>>> h)) >>> 'app' = 'app' >>> h@
--
-- Such arrows are equivalent to monads (see 'ArrowMonad').

class Arrow a => ArrowApply a where
    app :: a (a b c, b) c

instance ArrowApply (->) where
    app (f,x) = f x

instance Monad m => ArrowApply (Kleisli m) where
    app = Kleisli (\(Kleisli f, x) -> f x)

-- | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
--   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.

newtype ArrowMonad a b = ArrowMonad (a () b)

instance Arrow a => Functor (ArrowMonad a) where
    fmap f (ArrowMonad m) = ArrowMonad $ m >>> arr f

instance Arrow a => Applicative (ArrowMonad a) where
   pure x = ArrowMonad (arr (const x))
   ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry id))

instance ArrowApply a => Monad (ArrowMonad a) where
    return x = ArrowMonad (arr (\_ -> x))
    ArrowMonad m >>= f = ArrowMonad $
        m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app

instance ArrowPlus a => Alternative (ArrowMonad a) where
   empty = ArrowMonad zeroArrow
   ArrowMonad x <|> ArrowMonad y = ArrowMonad (x <+> y)

instance (ArrowApply a, ArrowPlus a) => MonadPlus (ArrowMonad a) where
   mzero = ArrowMonad zeroArrow
   ArrowMonad x `mplus` ArrowMonad y = ArrowMonad (x <+> y)

-- | Any instance of 'ArrowApply' can be made into an instance of
--   'ArrowChoice' by defining 'left' = 'leftApp'.

leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
             (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app

-- | The 'loop' operator expresses computations in which an output value
-- is fed back as input, although the computation occurs only once.
-- It underlies the @rec@ value recursion construct in arrow notation.
-- 'loop' should satisfy the following laws:
--
-- [/extension/]
--      @'loop' ('arr' f) = 'arr' (\\ b -> 'fst' ('fix' (\\ (c,d) -> f (b,d))))@
--
-- [/left tightening/]
--      @'loop' ('first' h >>> f) = h >>> 'loop' f@
--
-- [/right tightening/]
--      @'loop' (f >>> 'first' h) = 'loop' f >>> h@
--
-- [/sliding/]
--      @'loop' (f >>> 'arr' ('id' *** k)) = 'loop' ('arr' ('id' *** k) >>> f)@
--
-- [/vanishing/]
--      @'loop' ('loop' f) = 'loop' ('arr' unassoc >>> f >>> 'arr' assoc)@
--
-- [/superposing/]
--      @'second' ('loop' f) = 'loop' ('arr' assoc >>> 'second' f >>> 'arr' unassoc)@
--
-- where
--
-- > assoc ((a,b),c) = (a,(b,c))
-- > unassoc (a,(b,c)) = ((a,b),c)
--
class Arrow a => ArrowLoop a where
    loop :: a (b,d) (c,d) -> a b c

instance ArrowLoop (->) where
    loop f b = let (c,d) = f (b,d) in c

-- | Beware that for many monads (those for which the '>>=' operation
-- is strict) this instance will /not/ satisfy the right-tightening law
-- required by the 'ArrowLoop' class.
instance MonadFix m => ArrowLoop (Kleisli m) where
    loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
      where f' x y = f (x, snd y)
