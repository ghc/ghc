-----------------------------------------------------------------------------
--
-- Module      :  Control.Arrow
-- Copyright   :  (c) Ross Paterson 2002
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Arrow.hs,v 1.1 2002/02/26 18:19:17 ross Exp $
--
-- Basic arrow definitions, based on
--
--	"Generalising Monads to Arrows", by John Hughes, Science of
--	Computer Programming 37, pp67-111, May 2000.
--
-- plus a couple of definitions (returnA and loop) from
--
--	"A New Notation for Arrows", by Ross Paterson, in ICFP 2001,
--	Firenze, Italy, pp229-240.
--
-- See these papers for the equations these combinators are expected to
-- satisfy.  These papers and more information on arrows can be found at
--
--	http://www.soi.city.ac.uk/~ross/arrows/
--
-----------------------------------------------------------------------------

module Control.Arrow where

import Prelude

import Control.Monad
import Control.Monad.Fix

infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>
infixr 1 <<<

-----------------------------------------------------------------------------
-- Arrow classes

class Arrow a where
	arr :: (b -> c) -> a b c
	(>>>) :: a b c -> a c d -> a b d
	first :: a b c -> a (b,d) (c,d)

	-- The following combinators are placed in the class so that they
	-- can be overridden with more efficient versions if required.
	-- Any replacements should satisfy these equations.

	second :: a b c -> a (d,b) (d,c)
	second f = arr swap >>> first f >>> arr swap
			where	swap ~(x,y) = (y,x)

	(***) :: a b c -> a b' c' -> a (b,b') (c,c')
	f *** g = first f >>> second g

	(&&&) :: a b c -> a b c' -> a b (c,c')
	f &&& g = arr (\b -> (b,b)) >>> f *** g

	-- Some people prefer the name pure to arr, so both are allowed,
	-- but you must define one of them:

	pure :: (b -> c) -> a b c
	pure = arr
	arr = pure

-----------------------------------------------------------------------------
-- Derived combinators

-- The counterpart of return in arrow notation:

returnA :: Arrow a => a b b
returnA = arr id

-- Mirror image of >>>, for a better fit with arrow notation:

(<<<) :: Arrow a => a c d -> a b c -> a b d
f <<< g = g >>> f

-----------------------------------------------------------------------------
-- Monoid operations

class Arrow a => ArrowZero a where
	zeroArrow :: a b c

class ArrowZero a => ArrowPlus a where
	(<+>) :: a b c -> a b c -> a b c

-----------------------------------------------------------------------------
-- Conditionals

class Arrow a => ArrowChoice a where
	left :: a b c -> a (Either b d) (Either c d)

	-- The following combinators are placed in the class so that they
	-- can be overridden with more efficient versions if required.
	-- Any replacements should satisfy these equations.

	right :: a b c -> a (Either d b) (Either d c)
	right f = arr mirror >>> left f >>> arr mirror
			where	mirror (Left x) = Right x
				mirror (Right y) = Left y

	(+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
	f +++ g = left f >>> right g

	(|||) :: a b d -> a c d -> a (Either b c) d
	f ||| g = f +++ g >>> arr untag
			where	untag (Left x) = x
				untag (Right y) = y

-----------------------------------------------------------------------------
-- Arrow application

class Arrow a => ArrowApply a where
	app :: a (a b c, b) c

-- Any instance of ArrowApply can be made into an instance if ArrowChoice
-- by defining left = leftApp, where

leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
		 (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app

-- The ArrowApply class is equivalent to Monad: any monad gives rise to
-- a Kliesli arrow (see below), and any instance of ArrowApply defines
-- a monad:

newtype ArrowApply a => ArrowMonad a b = ArrowMonad (a () b)

instance ArrowApply a => Monad (ArrowMonad a) where
	return x = ArrowMonad (arr (\z -> x))
	ArrowMonad m >>= f = ArrowMonad (m >>>
			arr (\x -> let ArrowMonad h = f x in (h, ())) >>>
			app)

-----------------------------------------------------------------------------
-- Feedback

-- The following operator expresses computations in which a value is
-- recursively defined through the computation, even though the computation
-- occurs only once:

class Arrow a => ArrowLoop a where
	loop :: a (b,d) (c,d) -> a b c

-----------------------------------------------------------------------------
-- Arrow instances

-- Ordinary functions are arrows.

instance Arrow (->) where
	arr f = f
	f >>> g = g . f
	first f = f *** id
	second f = id *** f
	(f *** g) ~(x,y) = (f x, g y)

instance ArrowChoice (->) where
	left f = f +++ id
	right f = id +++ f
	f +++ g = (Left . f) ||| (Right . g)
	(|||) = either

instance ArrowApply (->) where
	app (f,x) = f x

instance ArrowLoop (->) where
	loop f b = let (c,d) = f (b,d) in c

-----------------------------------------------------------------------------
-- Kleisli arrows of a monad

newtype Kleisli m a b = Kleisli (a -> m b)

instance Monad m => Arrow (Kleisli m) where
	arr f = Kleisli (return . f)
	Kleisli f >>> Kleisli g = Kleisli (\b -> f b >>= g)
	first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
	second (Kleisli f) = Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))

instance MonadPlus m => ArrowZero (Kleisli m) where
	zeroArrow = Kleisli (\x -> mzero)

instance MonadPlus m => ArrowPlus (Kleisli m) where
	Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

instance Monad m => ArrowChoice (Kleisli m) where
	left f = f +++ arr id
	right f = arr id +++ f
	f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
	Kleisli f ||| Kleisli g = Kleisli (either f g)

instance Monad m => ArrowApply (Kleisli m) where
	app = Kleisli (\(Kleisli f, x) -> f x)

instance MonadFix m => ArrowLoop (Kleisli m) where
	loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
		where	f' x y = f (x, snd y)
