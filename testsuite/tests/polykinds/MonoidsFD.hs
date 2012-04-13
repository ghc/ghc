-- From a blog post: http://www.jonmsterling.com/posts/2012-01-12-unifying-monoids-and-monads-with-polymorphic-kinds.html

-------------------- FUNCTIONAL DEPENDENCY VERSION ----------------

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where
import Control.Monad (Monad(..), join)
import Data.Monoid (Monoid(..))

-- First we define the type class Monoidy:

class Monoidy to comp id m | m to → comp id where
  munit :: id `to` m
  mjoin :: (m `comp` m) `to` m

-- We use functional dependencies to help the typechecker understand that
-- m and ~> uniquely determine comp (times) and id.
-- 
-- This kind of type class would not have been possible in previous
-- versions of GHC; with the new kind system, however, we can abstract
-- over kinds!2 Now, let’s create types for the additive and
-- multiplicative monoids over the natural numbers:

newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show
instance Num a ⇒ Monoidy (→) (,) () (Sum a) where
  munit _ = Sum 0
  mjoin (Sum x, Sum y) = Sum $ x + y
instance Num a ⇒ Monoidy (→) (,) () (Product a) where
  munit _ = Product 1
  mjoin (Product x, Product y) = Product $ x * y

-- It will be slightly more complicated to make a monadic instance with
-- Monoidy. First, we need to define the identity functor, a type for
-- natural transformations, and a type for functor composition:

data Id α = Id { runId :: α } deriving Functor

-- A natural transformation (Λ f g α. (f α) → (g α)) may be encoded in Haskell as follows:

data NT f g = NT { runNT :: ∀ α. f α → g α }

-- Functor composition (Λ f g α. f (g α)) is encoded as follows:

data FC f g α = FC { runFC :: f (g α) }

-- Now, let us define some type T which should be a monad:

data Wrapper a = Wrapper { runWrapper :: a } deriving (Show, Functor)
instance Monoidy NT FC Id Wrapper where
  munit = NT $ Wrapper . runId
  mjoin = NT $ runWrapper . runFC

-- With these defined, we can use them as follows:

test1 = do { print (mjoin (munit (), Sum 2))
                 -- Sum 2
           ; print (mjoin (Product 2, Product 3))
                 -- Product 6
           ; print (runNT mjoin $ FC $ Wrapper (Wrapper "hello, world"))
                 -- Wrapper {runWrapper = "hello, world" }
           }

-- We can even provide a special binary operator for the appropriate monoids as follows:

(<+>) :: Monoidy (→) (,) () m ⇒ m → m → m
(<+>) = curry mjoin

test2 = print (Sum 1 <+> Sum 2 <+> Sum 4)  -- Sum 7

-- Now, all the extra wrapping that Haskell requires for encoding this is
-- rather cumbersome in actual use. So, we can give traditional Monad and
-- Monoid instances for instances of Monoidy:

instance Monoidy (→) (,) () m ⇒ Monoid m where
  mempty = munit ()
  mappend = curry mjoin

-- instance (Functor m, Monoidy NT FC Id m) ⇒ Monad m where
instance Monad Wrapper where
   return x = runNT munit $ Id x
   x >>= f = runNT mjoin $ FC (f `fmap` x)

-- And so the following works:

test3
 = do { print (mappend mempty (Sum 2))  
             -- Sum 2
      ; print (mappend (Product 2) (Product 3))
             -- Product 6
      ; print (join $ Wrapper $ Wrapper "hello")
             -- Wrapper {runWrapper = "hello" }
      ; print (Wrapper "hello, world" >>= return)
             -- Wrapper {runWrapper = "hello, world" }
      }

main = test1 >> test2 >> test3
