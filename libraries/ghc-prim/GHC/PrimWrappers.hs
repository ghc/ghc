{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PrimWrappers
-- Copyright   :  (c) Lodz University of Technology 2013
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Wrappers for comparison primops
--
-----------------------------------------------------------------------------

module GHC.PrimWrappers (
    gtChar#, geChar#, eqChar#,
    neChar#, ltChar#, leChar#,

    (>#), (>=#), (==#), (/=#), (<#), (<=#),

    gtWord#, geWord#, eqWord#,
    neWord#, ltWord#, leWord#,

    (>##), (>=##), (==##), (/=##), (<##), (<=##),

    gtFloat#, geFloat#, eqFloat#,
    neFloat#, ltFloat#, leFloat#,

    gtAddr#, geAddr#, eqAddr#,
    neAddr#, ltAddr#, leAddr#,

    sameMutableArray, sameMutableByteArray, sameMutableArrayArray,
    sameMutVar, sameTVar, sameMVar
  ) where

import GHC.Prim
import GHC.Types

gtChar# :: Char# -> Char# -> Bool
gtChar# a b = tagToEnum# (a `gtCharI#` b)
geChar# :: Char# -> Char# -> Bool
geChar# a b = tagToEnum# (a `geCharI#` b)
eqChar# :: Char# -> Char# -> Bool
eqChar# a b = tagToEnum# (a `eqCharI#` b)
neChar# :: Char# -> Char# -> Bool
neChar# a b = tagToEnum# (a `neCharI#` b)
ltChar# :: Char# -> Char# -> Bool
ltChar# a b = tagToEnum# (a `ltCharI#` b)
leChar# :: Char# -> Char# -> Bool
leChar# a b = tagToEnum# (a `leCharI#` b)

infix 4 >#, >=#, ==#, /=#, <#, <=#

(>#) :: Int# -> Int# -> Bool
(>#) a b = tagToEnum# (a >$# b)
(>=#) :: Int# -> Int# -> Bool
(>=#) a b = tagToEnum# (a >=$# b)
(==#) :: Int# -> Int# -> Bool
(==#) a b = tagToEnum# (a ==$# b)
(/=#) :: Int# -> Int# -> Bool
(/=#) a b = tagToEnum# (a /=$# b)
(<#)  :: Int# -> Int# -> Bool
(<#) a b = tagToEnum# (a <$# b)
(<=#) :: Int# -> Int# -> Bool
(<=#) a b = tagToEnum# (a <=$# b)

gtWord# :: Word# -> Word# -> Bool
gtWord# a b = tagToEnum# (a `gtWordI#` b)
geWord# :: Word# -> Word# -> Bool
geWord# a b = tagToEnum# (a `geWordI#` b)
eqWord# :: Word# -> Word# -> Bool
eqWord# a b = tagToEnum# (a `eqWordI#` b)
neWord# :: Word# -> Word# -> Bool
neWord# a b = tagToEnum# (a `neWordI#` b)
ltWord# :: Word# -> Word# -> Bool
ltWord# a b = tagToEnum# (a `ltWordI#` b)
leWord# :: Word# -> Word# -> Bool
leWord# a b = tagToEnum# (a `leWordI#` b)

infix 4 >##, >=##, ==##, /=##, <##, <=##

(>##)  :: Double# -> Double# -> Bool
(>##) a b = tagToEnum# (a >$## b)
(>=##) :: Double# -> Double# -> Bool
(>=##) a b = tagToEnum# (a >=$## b)
(==##) :: Double# -> Double# -> Bool
(==##) a b = tagToEnum# (a ==$## b)
(/=##) :: Double# -> Double# -> Bool
(/=##) a b = tagToEnum# (a /=$## b)
(<##)  :: Double# -> Double# -> Bool
(<##) a b = tagToEnum# (a <$## b)
(<=##) :: Double# -> Double# -> Bool
(<=##) a b = tagToEnum# (a <=$## b)

gtFloat# :: Float# -> Float# -> Bool
gtFloat# a b = tagToEnum# (a `gtFloatI#` b)
geFloat# :: Float# -> Float# -> Bool
geFloat# a b = tagToEnum# (a `geFloatI#` b)
eqFloat# :: Float# -> Float# -> Bool
eqFloat# a b = tagToEnum# (a `eqFloatI#` b)
neFloat# :: Float# -> Float# -> Bool
neFloat# a b = tagToEnum# (a `neFloatI#` b)
ltFloat# :: Float# -> Float# -> Bool
ltFloat# a b = tagToEnum# (a `ltFloatI#` b)
leFloat# :: Float# -> Float# -> Bool
leFloat# a b = tagToEnum# (a `leFloatI#` b)

gtAddr# :: Addr# -> Addr# -> Bool
gtAddr# a b = tagToEnum# (a `gtAddrI#` b)
geAddr# :: Addr# -> Addr# -> Bool
geAddr# a b = tagToEnum# (a `geAddrI#` b)
eqAddr# :: Addr# -> Addr# -> Bool
eqAddr# a b = tagToEnum# (a `eqAddrI#` b)
neAddr# :: Addr# -> Addr# -> Bool
neAddr# a b = tagToEnum# (a `neAddrI#` b)
ltAddr# :: Addr# -> Addr# -> Bool
ltAddr# a b = tagToEnum# (a `ltAddrI#` b)
leAddr# :: Addr# -> Addr# -> Bool
leAddr# a b = tagToEnum# (a `leAddrI#` b)

sameMutableArray :: MutableArray# s a -> MutableArray# s a -> Bool
sameMutableArray a b = tagToEnum# (sameMutableArray# a b)
sameMutableByteArray :: MutableByteArray# s -> MutableByteArray# s -> Bool
sameMutableByteArray a b = tagToEnum# (sameMutableByteArray# a b)
sameMutableArrayArray :: MutableArrayArray# s -> MutableArrayArray# s -> Bool
sameMutableArrayArray a b = tagToEnum# (sameMutableArrayArray# a b)

sameMutVar :: MutVar# s a -> MutVar# s a -> Bool
sameMutVar a b = tagToEnum# (sameMutVar# a b)
sameTVar :: TVar# s a -> TVar# s a -> Bool
sameTVar a b = tagToEnum# (sameTVar# a b)
sameMVar :: MVar# s a -> MVar# s a -> Bool
sameMVar a b = tagToEnum# (sameMVar# a b)
