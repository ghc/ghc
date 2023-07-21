{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base.Functor
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The functor class hierarchy.
--
-----------------------------------------------------------------------------

module GHC.Base.Functor
    ( Functor(..)
    , Applicative(..)
    , Monad(..)
    , liftA
    , liftA3
    , join
    , when
    , sequence
    , mapM
    , liftM
    , liftM2
    , liftM3
    , liftM4
    , liftM5
    , ap
    , (<**>)
    , (=<<)
      -- * Alternative
    , Alternative(..)
    , MonadPlus(..)
      -- * 'IO' helpers
    , returnIO
    , bindIO
    , thenIO
    , failIO
    , unIO
    ) where

-- ghc-prim
import GHC.Types (Bool, IO(..))
import GHC.Prim (State#, RealWorld, raiseIO#)
import GHC.Tuple (Solo(..))

-- base
import GHC.Base.FunOps (const, id, (.))
import GHC.Base.List
import GHC.Base.NonEmpty (NonEmpty(..))
import GHC.Base.Semigroup (Monoid(mempty), Semigroup((<>)))
import GHC.Base.String (String)
import GHC.Maybe (Maybe(..))

import {-# SOURCE #-} GHC.IO (mkUserError, mplusIO)

default ()              -- Double isn't available yet

infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixl 4 <*>, <*, *>, <**>
infixl 3 <|>

{- | A type @f@ is a Functor if it provides a function @fmap@ which, given any types @a@ and @b@
lets you apply any function from @(a -> b)@ to turn an @f a@ into an @f b@, preserving the
structure of @f@. Furthermore @f@ needs to adhere to the following:

[Identity]    @'fmap' 'id' == 'id'@
[Composition] @'fmap' (f . g) == 'fmap' f . 'fmap' g@

Note, that the second law follows from the free theorem of the type 'fmap' and
the first law, so you need only check that the former condition holds.
See <https://www.schoolofhaskell.com/user/edwardk/snippets/fmap> or
<https://github.com/quchen/articles/blob/master/second_functor_law.md>
for an explanation.
-}

class Functor f where
    -- | 'fmap' is used to apply a function of type @(a -> b)@ to a value of type @f a@,
    -- where f is a functor, to produce a value of type @f b@.
    -- Note that for any type constructor with more than one parameter (e.g., `Either`),
    -- only the last type parameter can be modified with `fmap` (e.g., `b` in `Either a b`).
    --
    -- Some type constructors with two parameters or more have a @'Data.Bifunctor'@ instance that allows
    -- both the last and the penultimate parameters to be mapped over.
    --
    -- ==== __Examples__
    --
    -- Convert from a @'Data.Maybe.Maybe' Int@ to a @Maybe String@
    -- using 'Prelude.show':
    --
    -- >>> fmap show Nothing
    -- Nothing
    -- >>> fmap show (Just 3)
    -- Just "3"
    --
    -- Convert from an @'Data.Either.Either' Int Int@ to an
    -- @Either Int String@ using 'Prelude.show':
    --
    -- >>> fmap show (Left 17)
    -- Left 17
    -- >>> fmap show (Right 17)
    -- Right "17"
    --
    -- Double each element of a list:
    --
    -- >>> fmap (*2) [1,2,3]
    -- [2,4,6]
    --
    -- Apply 'Prelude.even' to the second element of a pair:
    --
    -- >>> fmap even (2,2)
    -- (2,True)
    --
    -- It may seem surprising that the function is only applied to the last element of the tuple
    -- compared to the list example above which applies it to every element in the list.
    -- To understand, remember that tuples are type constructors with multiple type parameters:
    -- a tuple of 3 elements @(a,b,c)@ can also be written @(,,) a b c@ and its @Functor@ instance
    -- is defined for @Functor ((,,) a b)@ (i.e., only the third parameter is free to be mapped over
    -- with @fmap@).
    --
    -- It explains why @fmap@ can be used with tuples containing values of different types as in the
    -- following example:
    --
    -- >>> fmap even ("hello", 1.0, 4)
    -- ("hello",1.0,True)

    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    --
    -- ==== __Examples__
    --
    -- Perform a computation with 'Maybe' and replace the result with a
    -- constant value if it is 'Just':
    --
    -- >>> 'a' <$ Just 2
    -- Just 'a'
    -- >>> 'a' <$ Nothing
    -- Nothing
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const

-- | A functor with application, providing operations to
--
-- * embed pure expressions ('pure'), and
--
-- * sequence computations and combine their results ('<*>' and 'liftA2').
--
-- A minimal complete definition must include implementations of 'pure'
-- and of either '<*>' or 'liftA2'. If it defines both, then they must behave
-- the same as their default definitions:
--
--      @('<*>') = 'liftA2' 'id'@
--
--      @'liftA2' f x y = f 'Prelude.<$>' x '<*>' y@
--
-- Further, any definition must satisfy the following:
--
-- [Identity]
--
--      @'pure' 'id' '<*>' v = v@
--
-- [Composition]
--
--      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- [Homomorphism]
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- [Interchange]
--
--      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
--
--
-- The other methods have the following default definitions, which may
-- be overridden with equivalent specialized implementations:
--
--   * @u '*>' v = ('id' '<$' u) '<*>' v@
--
--   * @u '<*' v = 'liftA2' 'const' u v@
--
-- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
--
--   * @'fmap' f x = 'pure' f '<*>' x@
--
--
-- It may be useful to note that supposing
--
--      @forall x y. p (q x y) = f x . g y@
--
-- it follows from the above that
--
--      @'liftA2' p ('liftA2' q u v) = 'liftA2' f u . 'liftA2' g v@
--
--
-- If @f@ is also a 'Monad', it should satisfy
--
--   * @'pure' = 'return'@
--
--   * @m1 '<*>' m2 = m1 '>>=' (\\x1 -> m2 '>>=' (\\x2 -> 'return' (x1 x2)))@
--
--   * @('*>') = ('>>')@
--
-- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).

class Functor f => Applicative f where
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    --
    -- A few functors support an implementation of '<*>' that is more
    -- efficient than the default one.
    --
    -- ==== __Example__
    -- Used in combination with @('<$>')@, @('<*>')@ can be used to build a record.
    --
    -- >>> data MyState = MyState {arg1 :: Foo, arg2 :: Bar, arg3 :: Baz}
    --
    -- >>> produceFoo :: Applicative f => f Foo
    --
    -- >>> produceBar :: Applicative f => f Bar
    -- >>> produceBaz :: Applicative f => f Baz
    --
    -- >>> mkState :: Applicative f => f MyState
    -- >>> mkState = MyState <$> produceFoo <*> produceBar <*> produceBaz
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    -- | Lift a binary function to actions.
    --
    -- Some functors support an implementation of 'liftA2' that is more
    -- efficient than the default one. In particular, if 'fmap' is an
    -- expensive operation, it is likely better to use 'liftA2' than to
    -- 'fmap' over the structure and then use '<*>'.
    --
    -- This became a typeclass method in 4.10.0.0. Prior to that, it was
    -- a function defined in terms of '<*>' and 'fmap'.
    --
    -- ==== __Example__
    -- >>> liftA2 (,) (Just 3) (Just 5)
    -- Just (3,5)

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- | Sequence actions, discarding the value of the first argument.
    --
    -- ==== __Examples__
    -- If used in conjunction with the Applicative instance for 'Maybe',
    -- you can chain Maybe computations, with a possible "early return"
    -- in case of 'Nothing'.
    --
    -- >>> Just 2 *> Just 3
    -- Just 3
    --
    -- >>> Nothing *> Just 3
    -- Nothing
    --
    -- Of course a more interesting use case would be to have effectful
    -- computations instead of just returning pure values.
    --
    -- >>> import Data.Char
    -- >>> import Text.ParserCombinators.ReadP
    -- >>> let p = string "my name is " *> munch1 isAlpha <* eof
    -- >>> readP_to_S p "my name is Simon"
    -- [("Simon","")]

    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    -- This is essentially the same as liftA2 (flip const), but if the
    -- Functor instance has an optimized (<$), it may be better to use
    -- that instead. Before liftA2 became a method, this definition
    -- was strictly better, but now it depends on the functor. For a
    -- functor supporting a sharing-enhancing (<$), this definition
    -- may reduce allocation by preventing a1 from ever being fully
    -- realized. In an implementation with a boring (<$) but an optimizing
    -- liftA2, it would likely be better to define (*>) using liftA2.

    -- | Sequence actions, discarding the value of the second argument.
    --
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Instances of 'Monad' should satisfy the following:

[Left identity]  @'return' a '>>=' k  =  k a@
[Right identity] @m '>>=' 'return'  =  m@
[Associativity]  @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

* @'pure' = 'return'@
* @m1 '<*>' m2 = m1 '>>=' (\\x1 -> m2 '>>=' (\\x2 -> 'return' (x1 x2)))@

The above laws imply:

* @'fmap' f xs  =  xs '>>=' 'return' . f@
* @('>>') = ('*>')@

and that 'pure' and ('<*>') satisfy the applicative functor laws.

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}
class Applicative m => Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    --
    -- \'@as '>>=' bs@\' can be understood as the @do@ expression
    --
    -- @
    -- do a <- as
    --    bs a
    -- @
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    --
    -- \'@as '>>' bs@\' can be understood as the @do@ expression
    --
    -- @
    -- do as
    --    bs
    -- @
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure

-- | @since 4.15
instance Applicative Solo where
  pure = MkSolo

  -- Note: we really want to match strictly here. This lets us write,
  -- for example,
  --
  -- forceSpine :: Foldable f => f a -> ()
  -- forceSpine xs
  --   | MkSolo r <- traverse_ MkSolo xs
  --   = r
  MkSolo f <*> MkSolo x = MkSolo (f x)
  liftA2 f (MkSolo x) (MkSolo y) = MkSolo (f x y)

-- | For tuples, the 'Monoid' constraint on @a@ determines
-- how the first values merge.
-- For example, 'String's concatenate:
--
-- > ("hello ", (+15)) <*> ("world!", 2002)
-- > ("hello world!",2017)
--
-- @since 2.01
instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)
    liftA2 f (u, x) (v, y) = (u <> v, f x y)

-- | @since 4.15
instance Monad Solo where
  MkSolo x >>= f = f x

-- | @since 4.9.0.0
instance Monoid a => Monad ((,) a) where
    (u, a) >>= k = case k a of (v, b) -> (u <> v, b)

-- | @since 4.14.0.0
instance Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b) => Applicative ((,,) a b) where
    pure x = (mempty, mempty, x)
    (a, b, f) <*> (a', b', x) = (a <> a', b <> b', f x)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b) => Monad ((,,) a b) where
    (u, v, a) >>= k = case k a of (u', v', b) -> (u <> u', v <> v', b)

-- | @since 4.14.0.0
instance Functor ((,,,) a b c) where
    fmap f (a, b, c, d) = (a, b, c, f d)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Applicative ((,,,) a b c) where
    pure x = (mempty, mempty, mempty, x)
    (a, b, c, f) <*> (a', b', c', x) = (a <> a', b <> b', c <> c', f x)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c) where
    (u, v, w, a) >>= k = case k a of (u', v', w', b) -> (u <> u', v <> v', w <> w', b)

-- | @since 4.18.0.0
instance Functor ((,,,,) a b c d) where
    fmap f (a, b, c, d, e) = (a, b, c, d, f e)

-- | @since 4.18.0.0
instance Functor ((,,,,,) a b c d e) where
    fmap fun (a, b, c, d, e, f) = (a, b, c, d, e, fun f)

-- | @since 4.18.0.0
instance Functor ((,,,,,,) a b c d e f) where
    fmap fun (a, b, c, d, e, f, g) = (a, b, c, d, e, f, fun g)

-- | A variant of '<*>' with the arguments reversed.
--
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)
-- Don't use $ here, see the note at the top of the page

-- | Lift a function to actions.
-- Equivalent to Functor's `fmap` but implemented using only `Applicative`'s methods:
-- @'liftA' f a = 'pure' f '<*>' a@
--
-- As such this function may be used to implement a `Functor` instance from an `Applicative` one.
--
-- ==== __Examples__
-- Using the Applicative instance for Lists:
--
-- >>> liftA (+1) [1, 2]
-- [2,3]
--
-- Or the Applicative instance for 'Maybe'
--
-- >>> liftA (+1) (Just 3)
-- Just 4

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a
-- Caution: since this may be used for `fmap`, we can't use the obvious
-- definition of liftA = fmap.

-- | Lift a ternary function to actions.

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c


{-# INLINABLE liftA #-}
{-# SPECIALISE liftA :: (a1->r) -> IO a1 -> IO r #-}
{-# SPECIALISE liftA :: (a1->r) -> Maybe a1 -> Maybe r #-}
{-# INLINABLE liftA3 #-}
{-# SPECIALISE liftA3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
{-# SPECIALISE liftA3 :: (a1->a2->a3->r) ->
                                Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}

-- | The 'join' function is the conventional monad join operator. It
-- is used to remove one level of monadic structure, projecting its
-- bound argument into the outer level.
--
--
-- \'@'join' bss@\' can be understood as the @do@ expression
--
-- @
-- do bs <- bss
--    bs
-- @
--
-- ==== __Examples__
--
-- A common use of 'join' is to run an 'IO' computation returned from
-- an 'GHC.Conc.STM' transaction, since 'GHC.Conc.STM' transactions
-- can't perform 'IO' directly. Recall that
--
-- @
-- 'GHC.Conc.atomically' :: STM a -> IO a
-- @
--
-- is used to run 'GHC.Conc.STM' transactions atomically. So, by
-- specializing the types of 'GHC.Conc.atomically' and 'join' to
--
-- @
-- 'GHC.Conc.atomically' :: STM (IO b) -> IO (IO b)
-- 'join'       :: IO (IO b)  -> IO b
-- @
--
-- we can compose them as
--
-- @
-- 'join' . 'GHC.Conc.atomically' :: STM (IO b) -> IO b
-- @
--
-- to run an 'GHC.Conc.STM' transaction and the 'IO' action it
-- returns.
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id


{- Note [Recursive bindings for Applicative/Monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The original Applicative/Monad proposal stated that after
implementation, the designated implementation of (>>) would become

  (>>) :: forall a b. m a -> m b -> m b
  (>>) = (*>)

by default. You might be inclined to change this to reflect the stated
proposal, but you really shouldn't! Why? Because people tend to define
such instances the /other/ way around: in particular, it is perfectly
legitimate to define an instance of Applicative (*>) in terms of (>>),
which would lead to an infinite loop for the default implementation of
Monad! And people do this in the wild.

This turned into a nasty bug that was tricky to track down, and rather
than eliminate it everywhere upstream, it's easier to just retain the
original default.

-}

-- | Same as '>>=', but with the arguments interchanged.
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f

-- | Conditional execution of 'Applicative' expressions. For example,
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when      :: (Applicative f) => Bool -> f () -> f ()
{-# INLINABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s  = if p then s else pure ()

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence :: Monad m => [m a] -> m [a]
{-# INLINE sequence #-}
sequence = mapM id
-- Note: [sequence and mapM]

-- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM #-}
mapM f as = foldr k (return []) as
            where
              k a r = do { x <- f a; xs <- r; return (x:xs) }

{-
Note: [sequence and mapM]
~~~~~~~~~~~~~~~~~~~~~~~~~
Originally, we defined

mapM f = sequence . map f

This relied on list fusion to produce efficient code for mapM, and led to
excessive allocation in cryptarithm2. Defining

sequence = mapM id

relies only on inlining a tiny function (id) and beta reduction, which tends to
be a more reliable aspect of simplification. Indeed, this does not lead to
similar problems in nofib.
-}

-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- > liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- > liftM2 (+) (Just 1) Nothing = Nothing
--
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
-- Caution: since this may be used for `liftA2`, we can't use the obvious
-- definition of liftM2 = liftA2.

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

{-# INLINABLE liftM #-}
{-# SPECIALISE liftM :: (a1->r) -> IO a1 -> IO r #-}
{-# SPECIALISE liftM :: (a1->r) -> Maybe a1 -> Maybe r #-}
{-# INLINABLE liftM2 #-}
{-# SPECIALISE liftM2 :: (a1->a2->r) -> IO a1 -> IO a2 -> IO r #-}
{-# SPECIALISE liftM2 :: (a1->a2->r) -> Maybe a1 -> Maybe a2 -> Maybe r #-}
{-# INLINABLE liftM3 #-}
{-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
{-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}
{-# INLINABLE liftM4 #-}
{-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO r #-}
{-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe r #-}
{-# INLINABLE liftM5 #-}
{-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO a5 -> IO r #-}
{-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe r #-}

{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application.

> return f `ap` x1 `ap` ... `ap` xn

is equivalent to

> liftMn f x1 x2 ... xn

-}

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- Since many Applicative instances define (<*>) = ap, we
-- cannot define ap = (<*>)
{-# INLINABLE ap #-}
{-# SPECIALISE ap :: IO (a -> b) -> IO a -> IO b #-}
{-# SPECIALISE ap :: Maybe (a -> b) -> Maybe a -> Maybe b #-}

-- instances for Prelude types

-- | @since 2.01
instance Functor ((->) r) where
    fmap = (.)

-- | @since 2.01
instance Applicative ((->) r) where
    pure = const
    (<*>) f g x = f x (g x)
    liftA2 q f g x = q (f x) (g x)

-- | @since 2.01
instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

-- | @since 4.15
instance Functor Solo where
  fmap f (MkSolo a) = MkSolo (f a)

  -- Being strict in the `Solo` argument here seems most consistent
  -- with the concept behind `Solo`: always strict in the wrapper and lazy
  -- in the contents.
  x <$ MkSolo _ = MkSolo x

-- | @since 2.01
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

-- | @since 2.01
instance Functor Maybe where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

-- | @since 2.01
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

-- | @since 2.01
instance Monad Maybe where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (>>) = (*>)

-- | @since 2.01
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Applicative [] where
    {-# INLINE pure #-}
    pure x    = [x]
    {-# INLINE (<*>) #-}
    fs <*> xs = [f x | f <- fs, x <- xs]
    {-# INLINE liftA2 #-}
    liftA2 f xs ys = [f x y | x <- xs, y <- ys]
    {-# INLINE (*>) #-}
    xs *> ys  = [y | _ <- xs, y <- ys]

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    {-# INLINE (>>) #-}
    (>>) = (*>)

-- | Combines lists by concatenation, starting from the empty list.
--
-- @since 2.01
instance Alternative [] where
    empty = []
    (<|>) = (++)
-- | @since 4.9.0.0
instance Functor NonEmpty where
  fmap f ~(a :| as) = f a :| fmap f as
  b <$ ~(_ :| as)   = b   :| (b <$ as)

-- | @since 4.9.0.0
instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap
  liftA2 = liftM2

-- | @since 4.9.0.0
instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f
          toList ~(c :| cs) = c : cs

-- | A monoid on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @'some' v = (:) 'Prelude.<$>' v '<*>' 'many' v@
--
-- * @'many' v = 'some' v '<|>' 'pure' []@
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v


-- | Picks the leftmost 'Just' value, or, alternatively, 'Nothing'.
--
-- @since 2.01
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

-- -----------------------------------------------------------------------------
-- The MonadPlus class definition

-- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
   -- | The identity of 'mplus'.  It should also satisfy the equations
   --
   -- > mzero >>= f  =  mzero
   -- > v >> mzero   =  mzero
   --
   -- The default definition is
   --
   -- @
   -- mzero = 'empty'
   -- @
   mzero :: m a
   mzero = empty

   -- | An associative operation. The default definition is
   --
   -- @
   -- mplus = ('<|>')
   -- @
   mplus :: m a -> m a -> m a
   mplus = (<|>)

-- | Picks the leftmost 'Just' value, or, alternatively, 'Nothing'.
--
-- @since 2.01
instance MonadPlus Maybe

-- | Combines lists by concatenation, starting from the empty list.
--
-- @since 2.01
instance MonadPlus []

----------------------------------------------
-- Functor/Applicative/Monad instances for IO
----------------------------------------------

-- | @since 2.01
instance  Functor IO where
   fmap f x = x >>= (pure . f)

-- | @since 2.01
instance Applicative IO where
    {-# INLINE pure #-}
    {-# INLINE (*>) #-}
    {-# INLINE liftA2 #-}
    pure  = returnIO
    (*>)  = thenIO
    (<*>) = ap
    liftA2 = liftM2

-- | @since 2.01
instance  Monad IO  where
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    (>>)      = (*>)
    (>>=)     = bindIO

-- | Takes the first non-throwing 'IO' action\'s result.
-- 'empty' throws an exception.
--
-- @since 4.9.0.0
instance Alternative IO where
    empty = failIO "mzero"
    (<|>) = mplusIO

-- | Takes the first non-throwing 'IO' action\'s result.
-- 'mzero' throws an exception.
--
-- @since 4.9.0.0
instance MonadPlus IO

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)

-- Note that it is import that we do not SOURCE import this as
-- its demand signature encodes knowledge of its bottoming
-- behavior, which can expose useful simplifications. See
-- #16588.
failIO :: String -> IO a
failIO s = IO (raiseIO# (mkUserError s))

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

