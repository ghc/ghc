{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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
-- > instance (Read1 f, Read a) => Read (T f a) where
-- >   readPrec     = readPrec1
-- >   readListPrec = readListPrecDefault
-- > instance (Show1 f, Show a) => Show (T f a) where showsPrec = showsPrec1
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module Data.Functor.Classes (
    -- * Liftings of Prelude classes
    -- ** For unary constructors
    Eq1(..), eq1,
    Ord1(..), compare1,
    Read1(..), readsPrec1, readPrec1,
    liftReadListDefault, liftReadListPrecDefault,
    Show1(..), showsPrec1,
    -- ** For binary constructors
    Eq2(..), eq2,
    Ord2(..), compare2,
    Read2(..), readsPrec2, readPrec2,
    liftReadList2Default, liftReadListPrec2Default,
    Show2(..), showsPrec2,
    -- * Helper functions
    -- $example
    readsData, readData,
    readsUnaryWith, readUnaryWith,
    readsBinaryWith, readBinaryWith,
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

import Control.Applicative (Alternative((<|>)), Const(Const))

import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (Down(Down))
import Data.Complex (Complex((:+)))

import GHC.Generics (Generic1(..), Generically1(..))
import GHC.Tuple (Solo (..))
import GHC.Read (expectP, list, paren)

import Text.ParserCombinators.ReadPrec (ReadPrec, readPrec_to_S, readS_to_Prec)
import Text.Read (Read(..), parens, prec, step)
import Text.Read.Lex (Lexeme(..))
import Text.Show (showListWith)

-- $setup
-- >>> import Prelude
-- >>> import Data.Complex (Complex (..))
-- >>> import Text.ParserCombinators.ReadPrec

-- | Lifting of the 'Eq' class to unary type constructors.
--
-- @since 4.9.0.0
class Eq1 f where
    -- | Lift an equality test through the type constructor.
    --
    -- The function will usually be applied to an equality function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    --
    -- @since 4.9.0.0
    liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
--
-- @since 4.9.0.0
eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

-- | Lifting of the 'Ord' class to unary type constructors.
--
-- @since 4.9.0.0
class (Eq1 f) => Ord1 f where
    -- | Lift a 'compare' function through the type constructor.
    --
    -- The function will usually be applied to a comparison function,
    -- but the more general type ensures that the implementation uses
    -- it to compare elements of the first container with elements of
    -- the second.
    --
    -- @since 4.9.0.0
    liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
--
-- @since 4.9.0.0
compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

-- | Lifting of the 'Read' class to unary type constructors.
--
-- Both 'liftReadsPrec' and 'liftReadPrec' exist to match the interface
-- provided in the 'Read' type class, but it is recommended to implement
-- 'Read1' instances using 'liftReadPrec' as opposed to 'liftReadsPrec', since
-- the former is more efficient than the latter. For example:
--
-- @
-- instance 'Read1' T where
--   'liftReadPrec'     = ...
--   'liftReadListPrec' = 'liftReadListPrecDefault'
-- @
--
-- For more information, refer to the documentation for the 'Read' class.
--
-- @since 4.9.0.0
class Read1 f where
    {-# MINIMAL liftReadsPrec | liftReadPrec #-}

    -- | 'readsPrec' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument type.
    --
    -- @since 4.9.0.0
    liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
    liftReadsPrec rp rl = readPrec_to_S $
        liftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl))

    -- | 'readList' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument type.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- @since 4.9.0.0
    liftReadList :: (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
    liftReadList rp rl = readPrec_to_S
        (list $ liftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl))) 0

    -- | 'readPrec' function for an application of the type constructor
    -- based on 'readPrec' and 'readListPrec' functions for the argument type.
    --
    -- @since 4.10.0.0
    liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)
    liftReadPrec rp rl = readS_to_Prec $
        liftReadsPrec (readPrec_to_S rp) (readPrec_to_S rl 0)

    -- | 'readListPrec' function for an application of the type constructor
    -- based on 'readPrec' and 'readListPrec' functions for the argument type.
    --
    -- The default definition uses 'liftReadList'. Instances that define
    -- 'liftReadPrec' should also define 'liftReadListPrec' as
    -- 'liftReadListPrecDefault'.
    --
    -- @since 4.10.0.0
    liftReadListPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]
    liftReadListPrec rp rl = readS_to_Prec $ \_ ->
        liftReadList (readPrec_to_S rp) (readPrec_to_S rl 0)

-- | Lift the standard 'readsPrec' and 'readList' functions through the
-- type constructor.
--
-- @since 4.9.0.0
readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = liftReadsPrec readsPrec readList

-- | Lift the standard 'readPrec' and 'readListPrec' functions through the
-- type constructor.
--
-- @since 4.10.0.0
readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = liftReadPrec readPrec readListPrec

-- | A possible replacement definition for the 'liftReadList' method.
-- This is only needed for 'Read1' instances where 'liftReadListPrec' isn't
-- defined as 'liftReadListPrecDefault'.
--
-- @since 4.10.0.0
liftReadListDefault :: Read1 f => (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
liftReadListDefault rp rl = readPrec_to_S
    (liftReadListPrec (readS_to_Prec rp) (readS_to_Prec (const rl))) 0

-- | A possible replacement definition for the 'liftReadListPrec' method,
-- defined using 'liftReadPrec'.
--
-- @since 4.10.0.0
liftReadListPrecDefault :: Read1 f => ReadPrec a -> ReadPrec [a]
                        -> ReadPrec [f a]
liftReadListPrecDefault rp rl = list (liftReadPrec rp rl)

-- | Lifting of the 'Show' class to unary type constructors.
--
-- @since 4.9.0.0
class Show1 f where
    -- | 'showsPrec' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument type.
    --
    -- @since 4.9.0.0
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        Int -> f a -> ShowS

    -- | 'showList' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument type.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- @since 4.9.0.0
    liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        [f a] -> ShowS
    liftShowList sp sl = showListWith (liftShowsPrec sp sl 0)

-- | Lift the standard 'showsPrec' and 'showList' functions through the
-- type constructor.
--
-- @since 4.9.0.0
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

-- | Lifting of the 'Eq' class to binary type constructors.
--
-- @since 4.9.0.0
class Eq2 f where
    -- | Lift equality tests through the type constructor.
    --
    -- The function will usually be applied to equality functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    --
    -- @since 4.9.0.0
    liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
--
-- @since 4.9.0.0
eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = liftEq2 (==) (==)

-- | Lifting of the 'Ord' class to binary type constructors.
--
-- @since 4.9.0.0
class (Eq2 f) => Ord2 f where
    -- | Lift 'compare' functions through the type constructor.
    --
    -- The function will usually be applied to comparison functions,
    -- but the more general type ensures that the implementation uses
    -- them to compare elements of the first container with elements of
    -- the second.
    --
    -- @since 4.9.0.0
    liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) ->
        f a c -> f b d -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
--
-- @since 4.9.0.0
compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = liftCompare2 compare compare

-- | Lifting of the 'Read' class to binary type constructors.
--
-- Both 'liftReadsPrec2' and 'liftReadPrec2' exist to match the interface
-- provided in the 'Read' type class, but it is recommended to implement
-- 'Read2' instances using 'liftReadPrec2' as opposed to 'liftReadsPrec2',
-- since the former is more efficient than the latter. For example:
--
-- @
-- instance 'Read2' T where
--   'liftReadPrec2'     = ...
--   'liftReadListPrec2' = 'liftReadListPrec2Default'
-- @
--
-- For more information, refer to the documentation for the 'Read' class.
--
-- @since 4.9.0.0
class Read2 f where
    {-# MINIMAL liftReadsPrec2 | liftReadPrec2 #-}

    -- | 'readsPrec' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument types.
    --
    -- @since 4.9.0.0
    liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] ->
        (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)
    liftReadsPrec2 rp1 rl1 rp2 rl2 = readPrec_to_S $
        liftReadPrec2 (readS_to_Prec rp1) (readS_to_Prec (const rl1))
                      (readS_to_Prec rp2) (readS_to_Prec (const rl2))

    -- | 'readList' function for an application of the type constructor
    -- based on 'readsPrec' and 'readList' functions for the argument types.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- @since 4.9.0.0
    liftReadList2 :: (Int -> ReadS a) -> ReadS [a] ->
        (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
    liftReadList2 rp1 rl1 rp2 rl2 = readPrec_to_S
       (list $ liftReadPrec2 (readS_to_Prec rp1) (readS_to_Prec (const rl1))
                             (readS_to_Prec rp2) (readS_to_Prec (const rl2))) 0

    -- | 'readPrec' function for an application of the type constructor
    -- based on 'readPrec' and 'readListPrec' functions for the argument types.
    --
    -- @since 4.10.0.0
    liftReadPrec2 :: ReadPrec a -> ReadPrec [a] ->
        ReadPrec b -> ReadPrec [b] -> ReadPrec (f a b)
    liftReadPrec2 rp1 rl1 rp2 rl2 = readS_to_Prec $
        liftReadsPrec2 (readPrec_to_S rp1) (readPrec_to_S rl1 0)
                       (readPrec_to_S rp2) (readPrec_to_S rl2 0)

    -- | 'readListPrec' function for an application of the type constructor
    -- based on 'readPrec' and 'readListPrec' functions for the argument types.
    --
    -- The default definition uses 'liftReadList2'. Instances that define
    -- 'liftReadPrec2' should also define 'liftReadListPrec2' as
    -- 'liftReadListPrec2Default'.
    --
    -- @since 4.10.0.0
    liftReadListPrec2 :: ReadPrec a -> ReadPrec [a] ->
        ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
    liftReadListPrec2 rp1 rl1 rp2 rl2 = readS_to_Prec $ \_ ->
        liftReadList2 (readPrec_to_S rp1) (readPrec_to_S rl1 0)
                      (readPrec_to_S rp2) (readPrec_to_S rl2 0)

-- | Lift the standard 'readsPrec' function through the type constructor.
--
-- @since 4.9.0.0
readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = liftReadsPrec2 readsPrec readList readsPrec readList

-- | Lift the standard 'readPrec' function through the type constructor.
--
-- @since 4.10.0.0
readPrec2 :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2 = liftReadPrec2 readPrec readListPrec readPrec readListPrec

-- | A possible replacement definition for the 'liftReadList2' method.
-- This is only needed for 'Read2' instances where 'liftReadListPrec2' isn't
-- defined as 'liftReadListPrec2Default'.
--
-- @since 4.10.0.0
liftReadList2Default :: Read2 f => (Int -> ReadS a) -> ReadS [a] ->
    (Int -> ReadS b) -> ReadS [b] ->ReadS [f a b]
liftReadList2Default rp1 rl1 rp2 rl2 = readPrec_to_S
    (liftReadListPrec2 (readS_to_Prec rp1) (readS_to_Prec (const rl1))
                       (readS_to_Prec rp2) (readS_to_Prec (const rl2))) 0

-- | A possible replacement definition for the 'liftReadListPrec2' method,
-- defined using 'liftReadPrec2'.
--
-- @since 4.10.0.0
liftReadListPrec2Default :: Read2 f => ReadPrec a -> ReadPrec [a] ->
    ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
liftReadListPrec2Default rp1 rl1 rp2 rl2 = list (liftReadPrec2 rp1 rl1 rp2 rl2)

-- | Lifting of the 'Show' class to binary type constructors.
--
-- @since 4.9.0.0
class Show2 f where
    -- | 'showsPrec' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument types.
    --
    -- @since 4.9.0.0
    liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS

    -- | 'showList' function for an application of the type constructor
    -- based on 'showsPrec' and 'showList' functions for the argument types.
    -- The default implementation using standard list syntax is correct
    -- for most types.
    --
    -- @since 4.9.0.0
    liftShowList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) ->
        (Int -> b -> ShowS) -> ([b] -> ShowS) -> [f a b] -> ShowS
    liftShowList2 sp1 sl1 sp2 sl2 =
        showListWith (liftShowsPrec2 sp1 sl1 sp2 sl2 0)

-- | Lift the standard 'showsPrec' function through the type constructor.
--
-- @since 4.9.0.0
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = liftShowsPrec2 showsPrec showList showsPrec showList

-- Instances for Prelude type constructors

-- | @since 4.9.0.0
instance Eq1 Maybe where
    liftEq _ Nothing Nothing = True
    liftEq _ Nothing (Just _) = False
    liftEq _ (Just _) Nothing = False
    liftEq eq (Just x) (Just y) = eq x y

-- | @since 4.9.0.0
instance Ord1 Maybe where
    liftCompare _ Nothing Nothing = EQ
    liftCompare _ Nothing (Just _) = LT
    liftCompare _ (Just _) Nothing = GT
    liftCompare comp (Just x) (Just y) = comp x y

-- | @since 4.9.0.0
instance Read1 Maybe where
    liftReadPrec rp _ =
        parens (expectP (Ident "Nothing") *> pure Nothing)
        <|>
        readData (readUnaryWith rp "Just" Just)

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance Show1 Maybe where
    liftShowsPrec _ _ _ Nothing = showString "Nothing"
    liftShowsPrec sp _ d (Just x) = showsUnaryWith sp "Just" d x

-- | @since 4.9.0.0
instance Eq1 [] where
    liftEq _ [] [] = True
    liftEq _ [] (_:_) = False
    liftEq _ (_:_) [] = False
    liftEq eq (x:xs) (y:ys) = eq x y && liftEq eq xs ys

-- | @since 4.9.0.0
instance Ord1 [] where
    liftCompare _ [] [] = EQ
    liftCompare _ [] (_:_) = LT
    liftCompare _ (_:_) [] = GT
    liftCompare comp (x:xs) (y:ys) = comp x y `mappend` liftCompare comp xs ys

-- | @since 4.9.0.0
instance Read1 [] where
    liftReadPrec _ rl = rl
    liftReadListPrec  = liftReadListPrecDefault
    liftReadList      = liftReadListDefault

-- | @since 4.9.0.0
instance Show1 [] where
    liftShowsPrec _ sl _ = sl

-- | @since 4.10.0.0
instance Eq1 NonEmpty where
  liftEq eq (a :| as) (b :| bs) = eq a b && liftEq eq as bs

-- | @since 4.10.0.0
instance Ord1 NonEmpty where
  liftCompare cmp (a :| as) (b :| bs) = cmp a b `mappend` liftCompare cmp as bs

-- | @since 4.10.0.0
instance Read1 NonEmpty where
  liftReadsPrec rdP rdL p s = readParen (p > 5) (\s' -> do
    (a, s'') <- rdP 6 s'
    (":|", s''') <- lex s''
    (as, s'''') <- rdL s'''
    return (a :| as, s'''')) s

-- | @since 4.10.0.0
instance Show1 NonEmpty where
  liftShowsPrec shwP shwL p (a :| as) = showParen (p > 5) $
    shwP 6 a . showString " :| " . shwL as


-- | @since 4.9.0.0
instance Eq2 (,) where
    liftEq2 e1 e2 (x1, y1) (x2, y2) = e1 x1 x2 && e2 y1 y2

-- | @since 4.9.0.0
instance Ord2 (,) where
    liftCompare2 comp1 comp2 (x1, y1) (x2, y2) =
        comp1 x1 x2 `mappend` comp2 y1 y2

-- | @since 4.9.0.0
instance Read2 (,) where
    liftReadPrec2 rp1 _ rp2 _ = parens $ paren $ do
        x <- rp1
        expectP (Punc ",")
        y <- rp2
        return (x,y)

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

-- | @since 4.9.0.0
instance Show2 (,) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x, y) =
        showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

-- | @since 4.15
instance Eq1 Solo where
  liftEq eq (Solo a) (Solo b) = a `eq` b

-- | @since 4.9.0.0
instance (Eq a) => Eq1 ((,) a) where
    liftEq = liftEq2 (==)

-- | @since 4.15
instance Ord1 Solo where
  liftCompare cmp (Solo a) (Solo b) = cmp a b

-- | @since 4.9.0.0
instance (Ord a) => Ord1 ((,) a) where
    liftCompare = liftCompare2 compare

-- | @since 4.15
instance Read1 Solo where
    liftReadPrec rp _ = readData (readUnaryWith rp "Solo" Solo)

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance (Read a) => Read1 ((,) a) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.15
instance Show1 Solo where
    liftShowsPrec sp _ d (Solo x) = showsUnaryWith sp "Solo" d x

-- | @since 4.9.0.0
instance (Show a) => Show1 ((,) a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList


-- | @since 4.16.0.0
--
-- >>> eq2 ('x', True, "str") ('x', True, "str")
-- True
--
instance Eq a => Eq2 ((,,) a) where
    liftEq2 e1 e2 (u1, x1, y1) (v1, x2, y2) =
        u1 == v1 &&
        e1 x1 x2 && e2 y1 y2

-- | @since 4.16.0.0
--
-- >>> compare2 ('x', True, "aaa") ('x', True, "zzz")
-- LT
instance Ord a => Ord2 ((,,) a) where
    liftCompare2 comp1 comp2 (u1, x1, y1) (v1, x2, y2) =
        compare u1 v1 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

-- | @since 4.16.0.0
--
-- >>> readPrec_to_S readPrec2 0 "('x', True, 2)" :: [((Char, Bool, Int), String)]
-- [(('x',True,2),"")]
--
instance Read a => Read2 ((,,) a) where
    liftReadPrec2 rp1 _ rp2 _ = parens $ paren $ do
        x1 <- readPrec
        expectP (Punc ",")
        y1 <- rp1
        expectP (Punc ",")
        y2 <- rp2
        return (x1,y1,y2)

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

-- | @since 4.16.0.0
--
-- >>> showsPrec2 0 ('x', True, 2 :: Int) ""
-- "('x',True,2)"
--
instance Show a => Show2 ((,,) a) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

-- | @since 4.16.0.0
instance (Eq a, Eq b) => Eq1 ((,,) a b) where
    liftEq = liftEq2 (==)

-- | @since 4.16.0.0
instance (Ord a, Ord b) => Ord1 ((,,) a b) where
    liftCompare = liftCompare2 compare

-- | @since 4.16.0.0
instance (Read a, Read b) => Read1 ((,,) a b) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.16.0.0
instance (Show a, Show b) => Show1 ((,,) a b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList


-- | @since 4.16.0.0
--
-- >>> eq2 ('x', True, "str", 2) ('x', True, "str", 2 :: Int)
-- True
--
instance (Eq a, Eq b) => Eq2 ((,,,) a b) where
    liftEq2 e1 e2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        u1 == v1 &&
        u2 == v2 &&
        e1 x1 x2 && e2 y1 y2

-- | @since 4.16.0.0
--
-- >>> compare2 ('x', True, "str", 2) ('x', True, "str", 3 :: Int)
-- LT
--
instance (Ord a, Ord b) => Ord2 ((,,,) a b) where
    liftCompare2 comp1 comp2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        compare u1 v1 `mappend`
        compare u2 v2 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

-- | @since 4.16.0.0
--
-- >>> readPrec_to_S readPrec2 0 "('x', True, 2, 4.5)" :: [((Char, Bool, Int, Double), String)]
-- [(('x',True,2,4.5),"")]
--
instance (Read a, Read b) => Read2 ((,,,) a b) where
    liftReadPrec2 rp1 _ rp2 _ = parens $ paren $ do
        x1 <- readPrec
        expectP (Punc ",")
        x2 <- readPrec
        expectP (Punc ",")
        y1 <- rp1
        expectP (Punc ",")
        y2 <- rp2
        return (x1,x2,y1,y2)

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

-- | @since 4.16.0.0
--
-- >>> showsPrec2 0 ('x', True, 2 :: Int, 4.5 :: Double) ""
-- "('x',True,2,4.5)"
--
instance (Show a, Show b) => Show2 ((,,,) a b) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,x2,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . showsPrec 0 x2
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

-- | @since 4.16.0.0
instance (Eq a, Eq b, Eq c) => Eq1 ((,,,) a b c) where
    liftEq = liftEq2 (==)

-- | @since 4.16.0.0
instance (Ord a, Ord b, Ord c) => Ord1 ((,,,) a b c) where
    liftCompare = liftCompare2 compare

-- | @since 4.16.0.0
instance (Read a, Read b, Read c) => Read1 ((,,,) a b c) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.16.0.0
instance (Show a, Show b, Show c) => Show1 ((,,,) a b c) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- | @since 4.17.0.0
instance (Generic1 f, Eq1 (Rep1 f)) => Eq1 (Generically1 f) where
  liftEq :: (a1 -> a2 -> Bool) -> (Generically1 f a1 -> Generically1 f a2 -> Bool)
  liftEq (===) (Generically1 as1) (Generically1 as2) = liftEq (===) (from1 as1) (from1 as2)

-- | @since 4.17.0.0
instance (Generic1 f, Ord1 (Rep1 f)) => Ord1 (Generically1 f) where
  liftCompare :: (a1 -> a2 -> Ordering) -> (Generically1 f a1 -> Generically1 f a2 -> Ordering)
  liftCompare cmp (Generically1 as1) (Generically1 as2) = liftCompare cmp (from1 as1) (from1 as2)

-- | @since 4.9.0.0
instance Eq2 Either where
    liftEq2 e1 _ (Left x) (Left y) = e1 x y
    liftEq2 _ _ (Left _) (Right _) = False
    liftEq2 _ _ (Right _) (Left _) = False
    liftEq2 _ e2 (Right x) (Right y) = e2 x y

-- | @since 4.9.0.0
instance Ord2 Either where
    liftCompare2 comp1 _ (Left x) (Left y) = comp1 x y
    liftCompare2 _ _ (Left _) (Right _) = LT
    liftCompare2 _ _ (Right _) (Left _) = GT
    liftCompare2 _ comp2 (Right x) (Right y) = comp2 x y

-- | @since 4.9.0.0
instance Read2 Either where
    liftReadPrec2 rp1 _ rp2 _ = readData $
         readUnaryWith rp1 "Left" Left <|>
         readUnaryWith rp2 "Right" Right

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

-- | @since 4.9.0.0
instance Show2 Either where
    liftShowsPrec2 sp1 _ _ _ d (Left x) = showsUnaryWith sp1 "Left" d x
    liftShowsPrec2 _ _ sp2 _ d (Right x) = showsUnaryWith sp2 "Right" d x

-- | @since 4.9.0.0
instance (Eq a) => Eq1 (Either a) where
    liftEq = liftEq2 (==)

-- | @since 4.9.0.0
instance (Ord a) => Ord1 (Either a) where
    liftCompare = liftCompare2 compare

-- | @since 4.9.0.0
instance (Read a) => Read1 (Either a) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance (Show a) => Show1 (Either a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- Instances for other functors defined in the base package

-- | @since 4.9.0.0
instance Eq1 Identity where
    liftEq eq (Identity x) (Identity y) = eq x y

-- | @since 4.9.0.0
instance Ord1 Identity where
    liftCompare comp (Identity x) (Identity y) = comp x y

-- | @since 4.9.0.0
instance Read1 Identity where
    liftReadPrec rp _ = readData $
         readUnaryWith rp "Identity" Identity

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
instance Show1 Identity where
    liftShowsPrec sp _ d (Identity x) = showsUnaryWith sp "Identity" d x

-- | @since 4.9.0.0
instance Eq2 Const where
    liftEq2 eq _ (Const x) (Const y) = eq x y

-- | @since 4.9.0.0
instance Ord2 Const where
    liftCompare2 comp _ (Const x) (Const y) = comp x y

-- | @since 4.9.0.0
instance Read2 Const where
    liftReadPrec2 rp _ _ _ = readData $
         readUnaryWith rp "Const" Const

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

-- | @since 4.9.0.0
instance Show2 Const where
    liftShowsPrec2 sp _ _ _ d (Const x) = showsUnaryWith sp "Const" d x

-- | @since 4.9.0.0
instance (Eq a) => Eq1 (Const a) where
    liftEq = liftEq2 (==)
-- | @since 4.9.0.0
instance (Ord a) => Ord1 (Const a) where
    liftCompare = liftCompare2 compare
-- | @since 4.9.0.0
instance (Read a) => Read1 (Const a) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
-- | @since 4.9.0.0
instance (Show a) => Show1 (Const a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

-- Proxy unfortunately imports this module, hence these instances are placed
-- here,
-- | @since 4.9.0.0
instance Eq1 Proxy where
  liftEq _ _ _ = True

-- | @since 4.9.0.0
instance Ord1 Proxy where
  liftCompare _ _ _ = EQ

-- | @since 4.9.0.0
instance Show1 Proxy where
  liftShowsPrec _ _ _ _ = showString "Proxy"

-- | @since 4.9.0.0
instance Read1 Proxy where
  liftReadPrec _ _ = parens (expectP (Ident "Proxy") *> pure Proxy)

  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

-- | @since 4.12.0.0
instance Eq1 Down where
    liftEq eq (Down x) (Down y) = eq x y

-- | @since 4.12.0.0
instance Ord1 Down where
    liftCompare comp (Down x) (Down y) = case comp x y of
        LT -> GT
        EQ -> EQ
        GT -> LT

-- | @since 4.12.0.0
instance Read1 Down where
    liftReadsPrec rp _ = readsData $
         readsUnaryWith rp "Down" Down

-- | @since 4.12.0.0
instance Show1 Down where
    liftShowsPrec sp _ d (Down x) = showsUnaryWith sp "Down" d x

-- | @since 4.16.0.0
--
-- >>> eq1 (1 :+ 2) (1 :+ 2)
-- True
--
-- >>> eq1 (1 :+ 2) (1 :+ 3)
-- False
--
instance Eq1 Complex where
    liftEq eq (x :+ y) (u :+ v) = eq x u && eq y v

-- | @since 4.16.0.0
--
-- >>> readPrec_to_S readPrec1 0 "(2 % 3) :+ (3 % 4)" :: [(Complex Rational, String)]
-- [(2 % 3 :+ 3 % 4,"")]
--
instance Read1 Complex where
    liftReadPrec rp _  = parens $ prec complexPrec $ do
        x <- step rp
        expectP (Symbol ":+")
        y <- step rp
        return (x :+ y)
      where
        complexPrec = 6

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.16.0.0
--
-- >>> showsPrec1 0 (2 :+ 3) ""
-- "2 :+ 3"
--
instance Show1 Complex where
    liftShowsPrec sp _ d (x :+ y) = showParen (d > complexPrec) $
        sp (complexPrec+1) x . showString " :+ " . sp (complexPrec+1) y
      where
        complexPrec = 6

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readsUnary', 'readsUnary1' and 'readsBinary1', and combined with
-- @mappend@ from the @Monoid@ class.
--
-- @since 4.9.0.0
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \ r -> [res | (kw,s) <- lex r, res <- reader kw s]

-- | @'readData' p@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readUnaryWith' and 'readBinaryWith', and combined with
-- '(<|>)' from the 'Alternative' class.
--
-- @since 4.10.0.0
readData :: ReadPrec a -> ReadPrec a
readData reader = parens $ prec 10 reader

-- | @'readsUnaryWith' rp n c n'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
--
-- @since 4.9.0.0
readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith rp name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- rp 11 s]

-- | @'readUnaryWith' rp n c'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
--
-- @since 4.10.0.0
readUnaryWith :: ReadPrec a -> String -> (a -> t) -> ReadPrec t
readUnaryWith rp name cons = do
    expectP $ Ident name
    x <- step rp
    return $ cons x

-- | @'readsBinaryWith' rp1 rp2 n c n'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
--
-- @since 4.9.0.0
readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) ->
    String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith rp1 rp2 name cons kw s =
    [(cons x y,u) | kw == name, (x,t) <- rp1 11 s, (y,u) <- rp2 11 t]

-- | @'readBinaryWith' rp1 rp2 n c'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
--
-- @since 4.10.0.0
readBinaryWith :: ReadPrec a -> ReadPrec b ->
    String -> (a -> b -> t) -> ReadPrec t
readBinaryWith rp1 rp2 name cons = do
    expectP $ Ident name
    x <- step rp1
    y <- step rp2
    return $ cons x y

-- | @'showsUnaryWith' sp n d x@ produces the string representation of a
-- unary data constructor with name @n@ and argument @x@, in precedence
-- context @d@.
--
-- @since 4.9.0.0
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith sp name d x = showParen (d > 10) $
    showString name . showChar ' ' . sp 11 x

-- | @'showsBinaryWith' sp1 sp2 n d x y@ produces the string
-- representation of a binary data constructor with name @n@ and arguments
-- @x@ and @y@, in precedence context @d@.
--
-- @since 4.9.0.0
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
    String -> Int -> a -> b -> ShowS
showsBinaryWith sp1 sp2 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y

-- Obsolete building blocks

-- | @'readsUnary' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec'.
--
-- @since 4.9.0.0
{-# DEPRECATED readsUnary "Use 'readsUnaryWith' to define 'liftReadsPrec'" #-}
readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

-- | @'readsUnary1' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec1'.
--
-- @since 4.9.0.0
{-# DEPRECATED readsUnary1 "Use 'readsUnaryWith' to define 'liftReadsPrec'" #-}
readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

-- | @'readsBinary1' n c n'@ matches the name of a binary data constructor
-- and then parses its arguments using 'readsPrec1'.
--
-- @since 4.9.0.0
{-# DEPRECATED readsBinary1
      "Use 'readsBinaryWith' to define 'liftReadsPrec'" #-}
readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

-- | @'showsUnary' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
--
-- @since 4.9.0.0
{-# DEPRECATED showsUnary "Use 'showsUnaryWith' to define 'liftShowsPrec'" #-}
showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

-- | @'showsUnary1' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
--
-- @since 4.9.0.0
{-# DEPRECATED showsUnary1 "Use 'showsUnaryWith' to define 'liftShowsPrec'" #-}
showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

-- | @'showsBinary1' n d x y@ produces the string representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @d@.
--
-- @since 4.9.0.0
{-# DEPRECATED showsBinary1
      "Use 'showsBinaryWith' to define 'liftShowsPrec'" #-}
showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y

{- $example
These functions can be used to assemble 'Read' and 'Show' instances for
new algebraic types.  For example, given the definition

> data T f a = Zero a | One (f a) | Two a (f a)

a standard 'Read1' instance may be defined as

> instance (Read1 f) => Read1 (T f) where
>     liftReadPrec rp rl = readData $
>         readUnaryWith rp "Zero" Zero <|>
>         readUnaryWith (liftReadPrec rp rl) "One" One <|>
>         readBinaryWith rp (liftReadPrec rp rl) "Two" Two
>     liftReadListPrec = liftReadListPrecDefault

and the corresponding 'Show1' instance as

> instance (Show1 f) => Show1 (T f) where
>     liftShowsPrec sp _ d (Zero x) =
>         showsUnaryWith sp "Zero" d x
>     liftShowsPrec sp sl d (One x) =
>         showsUnaryWith (liftShowsPrec sp sl) "One" d x
>     liftShowsPrec sp sl d (Two x y) =
>         showsBinaryWith sp (liftShowsPrec sp sl) "Two" d x y

-}
