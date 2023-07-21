{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Base.FunOps
    ( id
    , const
    , (.)
    , flip
    , ($)
    , ($!)
    , asTypeOf
    ) where

import GHC.Types

infixr 9  .
infixr 0  $, $!

-- | Identity function.
--
-- > id x = x
id                      :: a -> a
id x                    =  x

-- | @const x y@ always evaluates to @x@, ignoring its second argument.
--
-- >>> const 42 "hello"
-- 42
--
-- >>> map (const 42) [0..3]
-- [42,42,42,42]
const                   :: a -> b -> a
const x _               =  x

-- | Function composition.
{-# INLINE (.) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
--
-- >>> flip (++) "hello" "world"
-- "worldhello"
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- Note: Before base-4.19, ($) was not representation polymorphic
-- in both type parameters but only in the return type.
-- The generalization forced a change to the implementation,
-- changing its laziness, affecting expressions like (($) undefined): before
-- base-4.19 the expression (($) undefined) `seq` () was equivalent to
-- (\x -> undefined x) `seq` () and thus would just evaluate to (), but now
-- it is equivalent to undefined `seq` () which diverges.

{- | @($)@ is the __function application__ operator.

Applying @($)@ to a function @f@ and an argument @x@ gives the same result as applying @f@ to @x@ directly. The definition is akin to this:

@
($) :: (a -> b) -> a -> b
($) f x = f x
@

On the face of it, this may appear pointless! But it's actually one of the most useful and important operators in Haskell.

The order of operations is very different between @($)@ and normal function application. Normal function application has precedence 10 - higher than any operator - and associates to the left. So these two definitions are equivalent:

@
expr = min 5 1 + 5
expr = ((min 5) 1) + 5
@

@($)@ has precedence 0 (the lowest) and associates to the right, so these are equivalent:

@
expr = min 5 $ 1 + 5
expr = (min 5) (1 + 5)
@

=== Uses

A common use cases of @($)@ is to avoid parentheses in complex expressions.

For example, instead of using nested parentheses in the following
 Haskell function:

@
-- | Sum numbers in a string: strSum "100  5 -7" == 98
strSum :: 'String' -> 'Int'
strSum s = 'sum' ('Data.Maybe.mapMaybe' 'Text.Read.readMaybe' ('words' s))
@

we can deploy the function application operator:

@
-- | Sum numbers in a string: strSum "100  5 -7" == 98
strSum :: 'String' -> 'Int'
strSum s = 'sum' '$' 'Data.Maybe.mapMaybe' 'Text.Read.readMaybe' '$' 'words' s
@

@($)@ is also used as a section (a partially applied operator), in order to indicate that we wish to apply some yet-unspecified function to a given value. For example, to apply the argument @5@ to a list of functions:

@
applyFive :: [Int]
applyFive = map ($ 5) [(+1), (2^)]
>>> [6, 32]
@

=== Technical Remark (Representation Polymorphism)

@($)@ is fully representation-polymorphic. This allows it to also be used with arguments of unlifted and even unboxed kinds, such as unboxed integers:

@
fastMod :: Int -> Int -> Int
fastMod (I# x) (I# m) = I# $ remInt# x m
@
-}
{-# INLINE ($) #-}
($) :: forall repa repb (a :: TYPE repa) (b :: TYPE repb). (a -> b) -> a -> b
($) f = f

-- | Strict (call-by-value) application operator. It takes a function and an
-- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- the function with that value.

($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
{-# INLINE ($!) #-}
f $! x = let !vx = x in f vx  -- see #2273

-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf                :: a -> a -> a
asTypeOf                =  const

