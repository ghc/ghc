{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(a,b,c) 1
#endif

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
# elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif
# if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
# endif
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013, Edward Kmett 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Liftings of the Prelude classes 'Eq', 'Ord', 'Read' and 'Show' to
-- unary and binary type constructors.
--
-- These classes are needed to express the constraints on arguments of
-- transformers in portable Haskell.  Thus for a new transformer @T@,
-- one might write instances like
--
-- > instance (Eq1 f) => Eq1 (T f) where ...
-- > instance (Ord1 f) => Ord1 (T f) where ...
-- > instance (Read1 f) => Read1 (T f) where ...
-- > instance (Show1 f) => Show1 (T f) where ...
--
-- If these instances can be defined, defining instances of the base
-- classes is mechanical:
--
-- > instance (Eq1 f, Eq a) => Eq (T f a) where (==) = eq1
-- > instance (Ord1 f, Ord a) => Ord (T f a) where compare = compare1
-- > instance (Read1 f, Read a) => Read (T f a) where readsPrec = readsPrec1
-- > instance (Show1 f, Show a) => Show (T f a) where showsPrec = showsPrec1
--
-----------------------------------------------------------------------------

module Data.Functor.Classes (
    -- * Liftings of Prelude classes
    -- ** For unary constructors
    Eq1(..), eq1,
    Ord1(..), compare1,
    Read1(..), readsPrec1,
    Show1(..), showsPrec1,
    -- ** For binary constructors
    Eq2(..), eq2,
    Ord2(..), compare2,
    Read2(..), readsPrec2,
    Show2(..), showsPrec2,
    -- * Helper functions
    -- $example
    readsData,
    readsUnaryWith,
    readsBinaryWith,
    showsUnaryWith,
    showsBinaryWith,
    -- ** Obsolete helpers
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  ) where

import Control.Applicative (Const(Const))
import Data.Functor.Identity (Identity(Identity))
import Data.Monoid (mappend)
#if MIN_VERSION_base(4,7,0)
import Data.Proxy (Proxy(Proxy))
#endif
import Text.Show (showListWith)

import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Product

#if MIN_VERSION_transformers(0,3,0)
import Control.Applicative.Lift
import Control.Applicative.Backwards
import Data.Functor.Reverse
#endif

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
# endif
#endif

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    -- | Lift an equality test through the type constructor.
    --
    -- The function will usually be applied to an equality function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

-- | Lifting of the 'Ord' class to unary type constructors.
class (Eq1 f) => Ord1 f where
    -- | Lift a 'compare' function through the type constructor.
    --
    -- The function will usually be applied to a comparison function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

-- | Lifting of the 'Read' class to unary type constructors.
class Read1 f where
    -- | 'readsPrec' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument type.
    liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)

    -- | 'readList' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument type.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    liftReadList :: (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
    liftReadList rp rl = readListWith (liftReadsPrec rp rl 0)

-- | Read a list (using square brackets and commas), given a function
-- for reading elements.
readListWith :: ReadS a -> ReadS [a]
readListWith rp =
    readParen False (\r -> [pr | ("[",s) <- lex r, pr <- readl s])
  where
    readl s = [([],t) | ("]",t) <- lex s] ++
        [(x:xs,u) | (x,t) <- rp s, (xs,u) <- readl' t]
    readl' s = [([],t) | ("]",t) <- lex s] ++
        [(x:xs,v) | (",",t) <- lex s, (x,u) <- rp t, (xs,v) <- readl' u]

-- | Lift the standard 'readsPrec' and 'readList' functions through the
-- type constructor.
readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = liftReadsPrec readsPrec readList

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    -- | 'showsPrec' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument type.
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        Int -> f a -> ShowS

    -- | 'showList' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument type.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        [f a] -> ShowS
    liftShowList sp sl = showListWith (liftShowsPrec sp sl 0)

-- | Lift the standard 'showsPrec' and 'showList' functions through the
-- type constructor.
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

-- | Lifting of the 'Eq' class to binary type constructors.
class Eq2 f where
    -- | Lift equality tests through the type constructor.
    --
    -- The function will usually be applied to equality functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = liftEq2 (==) (==)

-- | Lifting of the 'Ord' class to binary type constructors.
class (Eq2 f) => Ord2 f where
    -- | Lift 'compare' functions through the type constructor.
    --
    -- The function will usually be applied to comparison functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) ->
        f a c -> f b d -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = liftCompare2 compare compare

-- | Lifting of the 'Read' class to binary type constructors.
class Read2 f where
    -- | 'readsPrec' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument types.
    liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] ->
        (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)

    -- | 'readList' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument types.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    liftReadList2 :: (Int -> ReadS a) -> ReadS [a] ->
        (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
    liftReadList2 rp1 rl1 rp2 rl2 =
        readListWith (liftReadsPrec2 rp1 rl1 rp2 rl2 0)

-- | Lift the standard 'readsPrec' function through the type constructor.
readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = liftReadsPrec2 readsPrec readList readsPrec readList

-- | Lifting of the 'Show' class to binary type constructors.
class Show2 f where
    -- | 'showsPrec' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument types.
    liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS

    -- | 'showList' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument types.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    liftShowList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        (Int -> b -> ShowS) -> ([b] -> ShowS) -> [f a b] -> ShowS
    liftShowList2 sp1 sl1 sp2 sl2 =
        showListWith (liftShowsPrec2 sp1 sl1 sp2 sl2 0)

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = liftShowsPrec2 showsPrec showList showsPrec showList

-- Instances for Prelude type constructors

instance Eq1 Maybe where
    liftEq _ Nothing Nothing = True
    liftEq _ Nothing (Just _) = False
    liftEq _ (Just _) Nothing = False
    liftEq eq (Just x) (Just y) = eq x y

instance Ord1 Maybe where
    liftCompare _ Nothing Nothing = EQ
    liftCompare _ Nothing (Just _) = LT
    liftCompare _ (Just _) Nothing = GT
    liftCompare comp (Just x) (Just y) = comp x y

instance Read1 Maybe where
    liftReadsPrec rp _ d =
         readParen False (\ r -> [(Nothing,s) | ("Nothing",s) <- lex r])
         `mappend`
         readsData (readsUnaryWith rp "Just" Just) d

instance Show1 Maybe where
    liftShowsPrec _ _ _ Nothing = showString "Nothing"
    liftShowsPrec sp _ d (Just x) = showsUnaryWith sp "Just" d x

instance Eq1 [] where
    liftEq _ [] [] = True
    liftEq _ [] (_:_) = False
    liftEq _ (_:_) [] = False
    liftEq eq (x:xs) (y:ys) = eq x y && liftEq eq xs ys

instance Ord1 [] where
    liftCompare _ [] [] = EQ
    liftCompare _ [] (_:_) = LT
    liftCompare _ (_:_) [] = GT
    liftCompare comp (x:xs) (y:ys) = comp x y `mappend` liftCompare comp xs ys

instance Read1 [] where
    liftReadsPrec _ rl _ = rl

instance Show1 [] where
    liftShowsPrec _ sl _ = sl

instance Eq2 (,) where
    liftEq2 e1 e2 (x1, y1) (x2, y2) = e1 x1 x2 && e2 y1 y2

instance Ord2 (,) where
    liftCompare2 comp1 comp2 (x1, y1) (x2, y2) =
        comp1 x1 x2 `mappend` comp2 y1 y2

instance Read2 (,) where
    liftReadsPrec2 rp1 _ rp2 _ _ = readParen False $ \ r ->
        [((x,y), w) | ("(",s) <- lex r,
                      (x,t)   <- rp1 0 s,
                      (",",u) <- lex t,
                      (y,v)   <- rp2 0 u,
                      (")",w) <- lex v]

instance Show2 (,) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x, y) =
        showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

instance (Eq a) => Eq1 ((,) a) where
    liftEq = liftEq2 (==)

instance (Ord a) => Ord1 ((,) a) where
    liftCompare = liftCompare2 compare

instance (Read a) => Read1 ((,) a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a) => Show1 ((,) a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq2 Either where
    liftEq2 e1 _ (Left x) (Left y) = e1 x y
    liftEq2 _ _ (Left _) (Right _) = False
    liftEq2 _ _ (Right _) (Left _) = False
    liftEq2 _ e2 (Right x) (Right y) = e2 x y

instance Ord2 Either where
    liftCompare2 comp1 _ (Left x) (Left y) = comp1 x y
    liftCompare2 _ _ (Left _) (Right _) = LT
    liftCompare2 _ _ (Right _) (Left _) = GT
    liftCompare2 _ comp2 (Right x) (Right y) = comp2 x y

instance Read2 Either where
    liftReadsPrec2 rp1 _ rp2 _ = readsData $
         readsUnaryWith rp1 "Left" Left `mappend`
         readsUnaryWith rp2 "Right" Right

instance Show2 Either where
    liftShowsPrec2 sp1 _ _ _ d (Left x) = showsUnaryWith sp1 "Left" d x
    liftShowsPrec2 _ _ sp2 _ d (Right x) = showsUnaryWith sp2 "Right" d x

instance (Eq a) => Eq1 (Either a) where
    liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Either a) where
    liftCompare = liftCompare2 compare

instance (Read a) => Read1 (Either a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a) => Show1 (Either a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- Instances for other functors defined in the base package

instance Eq1 Identity where
    liftEq eq (Identity x) (Identity y) = eq x y

instance Ord1 Identity where
    liftCompare comp (Identity x) (Identity y) = comp x y

instance Read1 Identity where
    liftReadsPrec rp _ = readsData $
         readsUnaryWith rp "Identity" Identity

instance Show1 Identity where
    liftShowsPrec sp _ d (Identity x) = showsUnaryWith sp "Identity" d x

instance Eq2 Const where
    liftEq2 eq _ (Const x) (Const y) = eq x y

instance Ord2 Const where
    liftCompare2 comp _ (Const x) (Const y) = comp x y

instance Read2 Const where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "Const" Const

instance Show2 Const where
    liftShowsPrec2 sp _ _ _ d (Const x) = showsUnaryWith sp "Const" d x

instance (Eq a) => Eq1 (Const a) where
    liftEq = liftEq2 (==)
instance (Ord a) => Ord1 (Const a) where
    liftCompare = liftCompare2 compare
instance (Read a) => Read1 (Const a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Show a) => Show1 (Const a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

#if MIN_VERSION_base(4,7,0)
instance Eq1 Proxy where
    liftEq _ _ _ = True

instance Ord1 Proxy where
    liftCompare _ _ _ = EQ

instance Show1 Proxy where
    liftShowsPrec _ _ _ _ = showString "Proxy"

instance Read1 Proxy where
    liftReadsPrec _ _ d =
        readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
#endif

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readsUnary', 'readsUnary1' and 'readsBinary1', and combined with
-- @mappend@ from the @Monoid@ class.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \ r -> [res | (kw,s) <- lex r, res <- reader kw s]

-- | @'readsUnaryWith' rp n c n'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith rp name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- rp 11 s]

-- | @'readsBinaryWith' rp1 rp2 n c n'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) ->
    String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith rp1 rp2 name cons kw s =
    [(cons x y,u) | kw == name, (x,t) <- rp1 11 s, (y,u) <- rp2 11 t]

-- | @'showsUnaryWith' sp n d x@ produces the string representation of a
-- unary data constructor with name @n@ and argument @x@, in precedence
-- context @d@.
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith sp name d x = showParen (d > 10) $
    showString name . showChar ' ' . sp 11 x

-- | @'showsBinaryWith' sp1 sp2 n d x y@ produces the string
-- representation of a binary data constructor with name @n@ and arguments
-- @x@ and @y@, in precedence context @d@.
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
    String -> Int -> a -> b -> ShowS
showsBinaryWith sp1 sp2 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y

-- Obsolete building blocks

-- | @'readsUnary' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec'.
{-# DEPRECATED readsUnary "Use readsUnaryWith to define liftReadsPrec" #-}
readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

-- | @'readsUnary1' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec1'.
{-# DEPRECATED readsUnary1 "Use readsUnaryWith to define liftReadsPrec" #-}
readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

-- | @'readsBinary1' n c n'@ matches the name of a binary data constructor
-- and then parses its arguments using 'readsPrec1'.
{-# DEPRECATED readsBinary1 "Use readsBinaryWith to define liftReadsPrec" #-}
readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

-- | @'showsUnary' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
{-# DEPRECATED showsUnary "Use showsUnaryWith to define liftShowsPrec" #-}
showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

-- | @'showsUnary1' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
{-# DEPRECATED showsUnary1 "Use showsUnaryWith to define liftShowsPrec" #-}
showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

-- | @'showsBinary1' n d x y@ produces the string representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @d@.
{-# DEPRECATED showsBinary1 "Use showsBinaryWith to define liftShowsPrec" #-}
showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y


instance (Eq e, Eq1 m) => Eq1 (ErrorT e m) where
    liftEq eq (ErrorT x) (ErrorT y) = liftEq (liftEq eq) x y

instance (Ord e, Ord1 m) => Ord1 (ErrorT e m) where
    liftCompare comp (ErrorT x) (ErrorT y) = liftCompare (liftCompare comp) x y

instance (Read e, Read1 m) => Read1 (ErrorT e m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "ErrorT" ErrorT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (ErrorT e m) where
    liftShowsPrec sp sl d (ErrorT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "ErrorT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (ErrorT e m a) where (==) = eq1
instance (Ord e, Ord1 m, Ord a) => Ord (ErrorT e m a) where compare = compare1
instance (Read e, Read1 m, Read a) => Read (ErrorT e m a) where
    readsPrec = readsPrec1
instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
    showsPrec = showsPrec1

instance (Eq1 f) => Eq1 (IdentityT f) where
    liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y

instance (Ord1 f) => Ord1 (IdentityT f) where
    liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y

instance (Read1 f) => Read1 (IdentityT f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "IdentityT" IdentityT

instance (Show1 f) => Show1 (IdentityT f) where
    liftShowsPrec sp sl d (IdentityT m) =
        showsUnaryWith (liftShowsPrec sp sl) "IdentityT" d m

instance (Eq1 f, Eq a) => Eq (IdentityT f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where compare = compare1
instance (Read1 f, Read a) => Read (IdentityT f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (IdentityT f a) where showsPrec = showsPrec1

instance (Eq1 m) => Eq1 (ListT m) where
    liftEq eq (ListT x) (ListT y) = liftEq (liftEq eq) x y

instance (Ord1 m) => Ord1 (ListT m) where
    liftCompare comp (ListT x) (ListT y) = liftCompare (liftCompare comp) x y

instance (Read1 m) => Read1 (ListT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "ListT" ListT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (ListT m) where
    liftShowsPrec sp sl d (ListT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "ListT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (ListT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (ListT m a) where compare = compare1
instance (Read1 m, Read a) => Read (ListT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (ListT m a) where showsPrec = showsPrec1

instance (Eq1 m) => Eq1 (MaybeT m) where
    liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y

instance (Ord1 m) => Ord1 (MaybeT m) where
    liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y

instance (Read1 m) => Read1 (MaybeT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "MaybeT" MaybeT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MaybeT m) where
    liftShowsPrec sp sl d (MaybeT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "MaybeT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MaybeT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MaybeT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MaybeT m a) where showsPrec = showsPrec1

instance (Eq w, Eq1 m) => Eq1 (Lazy.WriterT w m) where
    liftEq eq (Lazy.WriterT m1) (Lazy.WriterT m2) =
        liftEq (liftEq2 eq (==)) m1 m2

instance (Ord w, Ord1 m) => Ord1 (Lazy.WriterT w m) where
    liftCompare comp (Lazy.WriterT m1) (Lazy.WriterT m2) =
        liftCompare (liftCompare2 comp compare) m1 m2

instance (Read w, Read1 m) => Read1 (Lazy.WriterT w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT" Lazy.WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (Lazy.WriterT w m) where
    liftShowsPrec sp sl d (Lazy.WriterT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a) => Eq (Lazy.WriterT w m a) where
    (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (Lazy.WriterT w m a) where
    compare = compare1
instance (Read w, Read1 m, Read a) => Read (Lazy.WriterT w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (Lazy.WriterT w m a) where
    showsPrec = showsPrec1

instance (Eq w, Eq1 m) => Eq1 (Strict.WriterT w m) where
    liftEq eq (Strict.WriterT m1) (Strict.WriterT m2) =
        liftEq (liftEq2 eq (==)) m1 m2

instance (Ord w, Ord1 m) => Ord1 (Strict.WriterT w m) where
    liftCompare comp (Strict.WriterT m1) (Strict.WriterT m2) =
        liftCompare (liftCompare2 comp compare) m1 m2

instance (Read w, Read1 m) => Read1 (Strict.WriterT w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT" Strict.WriterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (Strict.WriterT w m) where
    liftShowsPrec sp sl d (Strict.WriterT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a) => Eq (Strict.WriterT w m a) where
    (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (Strict.WriterT w m a) where
    compare = compare1
instance (Read w, Read1 m, Read a) => Read (Strict.WriterT w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (Strict.WriterT w m a) where
    showsPrec = showsPrec1

instance (Eq1 f, Eq1 g) => Eq1 (Compose f g) where
    liftEq eq (Compose x) (Compose y) = liftEq (liftEq eq) x y

instance (Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    liftCompare comp (Compose x) (Compose y) =
        liftCompare (liftCompare comp) x y

instance (Read1 f, Read1 g) => Read1 (Compose f g) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "Compose" Compose
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 f, Show1 g) => Show1 (Compose f g) where
    liftShowsPrec sp sl d (Compose x) =
        showsUnaryWith (liftShowsPrec sp' sl') "Compose" d x
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare = compare1
instance (Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec = showsPrec1

instance (Eq1 f, Eq1 g) => Eq1 (Product f g) where
    liftEq eq (Pair x1 y1) (Pair x2 y2) = liftEq eq x1 x2 && liftEq eq y1 y2

instance (Ord1 f, Ord1 g) => Ord1 (Product f g) where
    liftCompare comp (Pair x1 y1) (Pair x2 y2) =
        liftCompare comp x1 x2 `mappend` liftCompare comp y1 y2

instance (Read1 f, Read1 g) => Read1 (Product f g) where
    liftReadsPrec rp rl = readsData $
        readsBinaryWith (liftReadsPrec rp rl) (liftReadsPrec rp rl) "Pair" Pair

instance (Show1 f, Show1 g) => Show1 (Product f g) where
    liftShowsPrec sp sl d (Pair x y) =
        showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Pair" d x y

instance (Eq1 f, Eq1 g, Eq a) => Eq (Product f g a)
    where (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (Product f g a) where
    compare = compare1
instance (Read1 f, Read1 g, Read a) => Read (Product f g a) where
    readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show (Product f g a) where
    showsPrec = showsPrec1

instance Eq2 Constant where
    liftEq2 eq _ (Constant x) (Constant y) = eq x y
instance Ord2 Constant where
    liftCompare2 comp _ (Constant x) (Constant y) = comp x y
instance Read2 Constant where
    liftReadsPrec2 rp _ _ _ = readsData $
         readsUnaryWith rp "Constant" Constant
instance Show2 Constant where
    liftShowsPrec2 sp _ _ _ d (Constant x) = showsUnaryWith sp "Constant" d x

instance (Eq a) => Eq1 (Constant a) where
    liftEq = liftEq2 (==)
instance (Ord a) => Ord1 (Constant a) where
    liftCompare = liftCompare2 compare
instance (Read a) => Read1 (Constant a) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Show a) => Show1 (Constant a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq a => Eq (Constant a b) where
    Constant a == Constant b = a == b
instance Ord a => Ord (Constant a b) where
    compare (Constant a) (Constant b) = compare a b
instance (Read a) => Read (Constant a b) where
    readsPrec = readsData $
         readsUnaryWith readsPrec "Constant" Constant
instance (Show a) => Show (Constant a b) where
    showsPrec d (Constant x) = showsUnaryWith showsPrec "Constant" d x

instance Show a => Show (Identity a) where
  showsPrec d (Identity a) = showParen (d > 10) $
    showString "Identity " . showsPrec 11 a
instance Read a => Read (Identity a) where
  readsPrec d = readParen (d > 10) (\r -> [(Identity m,t) | ("Identity",s) <- lex r, (m,t) <- readsPrec 11 s])
instance Eq a   => Eq (Identity a) where
  Identity a == Identity b = a == b
instance Ord a  => Ord (Identity a) where
  compare (Identity a) (Identity b) = compare a b

#if MIN_VERSION_transformers(0,3,0)
instance (Eq1 f) => Eq1 (Lift f) where
    liftEq eq (Pure x1) (Pure x2) = eq x1 x2
    liftEq _ (Pure _) (Other _) = False
    liftEq _ (Other _) (Pure _) = False
    liftEq eq (Other y1) (Other y2) = liftEq eq y1 y2

instance (Ord1 f) => Ord1 (Lift f) where
    liftCompare comp (Pure x1) (Pure x2) = comp x1 x2
    liftCompare _ (Pure _) (Other _) = LT
    liftCompare _ (Other _) (Pure _) = GT
    liftCompare comp (Other y1) (Other y2) = liftCompare comp y1 y2

instance (Read1 f) => Read1 (Lift f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith rp "Pure" Pure `mappend`
        readsUnaryWith (liftReadsPrec rp rl) "Other" Other

instance (Show1 f) => Show1 (Lift f) where
    liftShowsPrec sp _ d (Pure x) = showsUnaryWith sp "Pure" d x
    liftShowsPrec sp sl d (Other y) =
        showsUnaryWith (liftShowsPrec sp sl) "Other" d y

instance (Eq1 f, Eq a) => Eq (Lift f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Lift f a) where compare = compare1
instance (Read1 f, Read a) => Read (Lift f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Lift f a) where showsPrec = showsPrec1

instance (Eq1 f) => Eq1 (Backwards f) where
    liftEq eq (Backwards x) (Backwards y) = liftEq eq x y

instance (Ord1 f) => Ord1 (Backwards f) where
    liftCompare comp (Backwards x) (Backwards y) = liftCompare comp x y

instance (Read1 f) => Read1 (Backwards f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Backwards" Backwards

instance (Show1 f) => Show1 (Backwards f) where
    liftShowsPrec sp sl d (Backwards x) =
        showsUnaryWith (liftShowsPrec sp sl) "Backwards" d x

instance (Eq1 f, Eq a) => Eq (Backwards f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Backwards f a) where compare = compare1
instance (Read1 f, Read a) => Read (Backwards f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Backwards f a) where showsPrec = showsPrec1

instance (Eq1 f) => Eq1 (Reverse f) where
    liftEq eq (Reverse x) (Reverse y) = liftEq eq x y

instance (Ord1 f) => Ord1 (Reverse f) where
    liftCompare comp (Reverse x) (Reverse y) = liftCompare comp x y

instance (Read1 f) => Read1 (Reverse f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Reverse" Reverse

instance (Show1 f) => Show1 (Reverse f) where
    liftShowsPrec sp sl d (Reverse x) =
        showsUnaryWith (liftShowsPrec sp sl) "Reverse" d x

instance (Eq1 f, Eq a) => Eq (Reverse f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Reverse f a) where compare = compare1
instance (Read1 f, Read a) => Read (Reverse f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Reverse f a) where showsPrec = showsPrec1
#endif

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Eq1
deriving instance Typeable Eq2
deriving instance Typeable Ord1
deriving instance Typeable Ord2
deriving instance Typeable Read1
deriving instance Typeable Read2
deriving instance Typeable Show1
deriving instance Typeable Show2
# endif
#endif

{- $example
These functions can be used to assemble 'Read' and 'Show' instances for
new algebraic types.  For example, given the definition

> data T f a = Zero a | One (f a) | Two a (f a)

a standard 'Read1' instance may be defined as

> instance (Read1 f) => Read1 (T f) where
>     liftReadsPrec rp rl = readsData $
>         readsUnaryWith rp "Zero" Zero `mappend`
>         readsUnaryWith (liftReadsPrec rp rl) "One" One `mappend`
>         readsBinaryWith rp (liftReadsPrec rp rl) "Two" Two

and the corresponding 'Show1' instance as

> instance (Show1 f) => Show1 (T f) where
>     liftShowsPrec sp _ d (Zero x) =
>         showsUnaryWith sp "Zero" d x
>     liftShowsPrec sp sl d (One x) =
>         showsUnaryWith (liftShowsPrec sp sl) "One" d x
>     liftShowsPrec sp sl d (Two x y) =
>         showsBinaryWith sp (liftShowsPrec sp sl) "Two" d x y

-}
