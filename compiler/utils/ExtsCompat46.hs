{-# LANGUAGE BangPatterns, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ExtsCompat46
-- Copyright   :  (c) Lodz University of Technology 2013
-- License     :  see LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC internal)
--
-- Compatibility module to encapsulate primops API change between GHC 7.6
-- GHC 7.8.
--
-- In GHC we use comparison primops in a couple of modules, but that primops
-- have different type signature in GHC 7.6 (where they return Bool) than
-- in GHC 7.8 (where they return Int#). As long as we allow bootstrapping
-- with GHC 7.6 or earlier we need to have this compatibility module, so that
-- we can compile stage1 compiler using the old API and then continue with
-- stage2 using the new API. When we set GHC 7.8 as the minimum version
-- required for bootstrapping, we should remove this module.
--
-----------------------------------------------------------------------------

module ExtsCompat46 (
    module GHC.Exts,

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

    sameMutableArray#, sameMutableByteArray#, sameMutableArrayArray#,
    sameMutVar#, sameTVar#, sameMVar#

 ) where

import GHC.Exts hiding (
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

    sameMutableArray#, sameMutableByteArray#, sameMutableArrayArray#,
    sameMutVar#, sameTVar#, sameMVar#
 )

import qualified GHC.Exts as E (
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

    sameMutableArray#, sameMutableByteArray#, sameMutableArrayArray#,
    sameMutVar#, sameTVar#, sameMVar#
 )

-- See #8330
#if __GLASGOW_HASKELL__ > 710
#error What is minimal version of GHC required for bootstraping? If it's GHC 7.8 we should remove this module and use GHC.Exts instead.
#endif

#if __GLASGOW_HASKELL__ > 706

gtChar# :: Char# -> Char# -> Bool
gtChar# a b = isTrue# (a `E.gtChar#` b)
geChar# :: Char# -> Char# -> Bool
geChar# a b = isTrue# (a `E.geChar#` b)
eqChar# :: Char# -> Char# -> Bool
eqChar# a b = isTrue# (a `E.eqChar#` b)
neChar# :: Char# -> Char# -> Bool
neChar# a b = isTrue# (a `E.neChar#` b)
ltChar# :: Char# -> Char# -> Bool
ltChar# a b = isTrue# (a `E.ltChar#` b)
leChar# :: Char# -> Char# -> Bool
leChar# a b = isTrue# (a `E.leChar#` b)

infix 4 >#, >=#, ==#, /=#, <#, <=#

(>#) :: Int# -> Int# -> Bool
(>#) a b = isTrue# (a E.># b)
(>=#) :: Int# -> Int# -> Bool
(>=#) a b = isTrue# (a E.>=# b)
(==#) :: Int# -> Int# -> Bool
(==#) a b = isTrue# (a E.==# b)
(/=#) :: Int# -> Int# -> Bool
(/=#) a b = isTrue# (a E./=# b)
(<#)  :: Int# -> Int# -> Bool
(<#) a b = isTrue# (a E.<# b)
(<=#) :: Int# -> Int# -> Bool
(<=#) a b = isTrue# (a E.<=# b)

gtWord# :: Word# -> Word# -> Bool
gtWord# a b = isTrue# (a `E.gtWord#` b)
geWord# :: Word# -> Word# -> Bool
geWord# a b = isTrue# (a `E.geWord#` b)
eqWord# :: Word# -> Word# -> Bool
eqWord# a b = isTrue# (a `E.eqWord#` b)
neWord# :: Word# -> Word# -> Bool
neWord# a b = isTrue# (a `E.neWord#` b)
ltWord# :: Word# -> Word# -> Bool
ltWord# a b = isTrue# (a `E.ltWord#` b)
leWord# :: Word# -> Word# -> Bool
leWord# a b = isTrue# (a `E.leWord#` b)

infix 4 >##, >=##, ==##, /=##, <##, <=##

(>##)  :: Double# -> Double# -> Bool
(>##) a b = isTrue# (a E.>## b)
(>=##) :: Double# -> Double# -> Bool
(>=##) a b = isTrue# (a E.>=## b)
(==##) :: Double# -> Double# -> Bool
(==##) a b = isTrue# (a E.==## b)
(/=##) :: Double# -> Double# -> Bool
(/=##) a b = isTrue# (a E./=## b)
(<##)  :: Double# -> Double# -> Bool
(<##) a b = isTrue# (a E.<## b)
(<=##) :: Double# -> Double# -> Bool
(<=##) a b = isTrue# (a E.<=## b)

gtFloat# :: Float# -> Float# -> Bool
gtFloat# a b = isTrue# (a `E.gtFloat#` b)
geFloat# :: Float# -> Float# -> Bool
geFloat# a b = isTrue# (a `E.geFloat#` b)
eqFloat# :: Float# -> Float# -> Bool
eqFloat# a b = isTrue# (a `E.eqFloat#` b)
neFloat# :: Float# -> Float# -> Bool
neFloat# a b = isTrue# (a `E.neFloat#` b)
ltFloat# :: Float# -> Float# -> Bool
ltFloat# a b = isTrue# (a `E.ltFloat#` b)
leFloat# :: Float# -> Float# -> Bool
leFloat# a b = isTrue# (a `E.leFloat#` b)

gtAddr# :: Addr# -> Addr# -> Bool
gtAddr# a b = isTrue# (a `E.gtAddr#` b)
geAddr# :: Addr# -> Addr# -> Bool
geAddr# a b = isTrue# (a `E.geAddr#` b)
eqAddr# :: Addr# -> Addr# -> Bool
eqAddr# a b = isTrue# (a `E.eqAddr#` b)
neAddr# :: Addr# -> Addr# -> Bool
neAddr# a b = isTrue# (a `E.neAddr#` b)
ltAddr# :: Addr# -> Addr# -> Bool
ltAddr# a b = isTrue# (a `E.ltAddr#` b)
leAddr# :: Addr# -> Addr# -> Bool
leAddr# a b = isTrue# (a `E.leAddr#` b)

sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Bool
sameMutableArray# a b = isTrue# (E.sameMutableArray# a b)
sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Bool
sameMutableByteArray# a b = isTrue# (E.sameMutableByteArray# a b)
sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Bool
sameMutableArrayArray# a b = isTrue# (E.sameMutableArrayArray# a b)

sameMutVar# :: MutVar# s a -> MutVar# s a -> Bool
sameMutVar# a b = isTrue# (E.sameMutVar# a b)
sameTVar# :: TVar# s a -> TVar# s a -> Bool
sameTVar# a b = isTrue# (E.sameTVar# a b)
sameMVar# :: MVar# s a -> MVar# s a -> Bool
sameMVar# a b = isTrue# (E.sameMVar# a b)

#else

gtChar# :: Char# -> Char# -> Bool
gtChar# a b = a `E.gtChar#` b
geChar# :: Char# -> Char# -> Bool
geChar# a b = a `E.geChar#` b
eqChar# :: Char# -> Char# -> Bool
eqChar# a b = a `E.eqChar#` b
neChar# :: Char# -> Char# -> Bool
neChar# a b = a `E.neChar#` b
ltChar# :: Char# -> Char# -> Bool
ltChar# a b = a `E.ltChar#` b
leChar# :: Char# -> Char# -> Bool
leChar# a b = a `E.leChar#` b

infix 4 >#, >=#, ==#, /=#, <#, <=#

(>#)  :: Int# -> Int# -> Bool
(>#) a b = a E.># b
(>=#) :: Int# -> Int# -> Bool
(>=#) a b = a E.>=# b
(==#) :: Int# -> Int# -> Bool
(==#) a b = a E.==# b
(/=#) :: Int# -> Int# -> Bool
(/=#) a b = a E./=# b
(<#)  :: Int# -> Int# -> Bool
(<#) a b = a E.<# b
(<=#) :: Int# -> Int# -> Bool
(<=#) a b = a E.<=# b

gtWord# :: Word# -> Word# -> Bool
gtWord# a b = a `E.gtWord#` b
geWord# :: Word# -> Word# -> Bool
geWord# a b = a `E.geWord#` b
eqWord# :: Word# -> Word# -> Bool
eqWord# a b = a `E.eqWord#` b
neWord# :: Word# -> Word# -> Bool
neWord# a b = a `E.neWord#` b
ltWord# :: Word# -> Word# -> Bool
ltWord# a b = a `E.ltWord#` b
leWord# :: Word# -> Word# -> Bool
leWord# a b = a `E.leWord#` b

infix 4 >##, >=##, ==##, /=##, <##, <=##

(>##)  :: Double# -> Double# -> Bool
(>##) a b = a E.>## b
(>=##) :: Double# -> Double# -> Bool
(>=##) a b = a E.>=## b
(==##) :: Double# -> Double# -> Bool
(==##) a b = a E.==## b
(/=##) :: Double# -> Double# -> Bool
(/=##) a b = a E./=## b
(<##)  :: Double# -> Double# -> Bool
(<##) a b = a E.<## b
(<=##) :: Double# -> Double# -> Bool
(<=##) a b = a E.<=## b

gtFloat# :: Float# -> Float# -> Bool
gtFloat# a b = a `E.gtFloat#` b
geFloat# :: Float# -> Float# -> Bool
geFloat# a b = a `E.geFloat#` b
eqFloat# :: Float# -> Float# -> Bool
eqFloat# a b = a `E.eqFloat#` b
neFloat# :: Float# -> Float# -> Bool
neFloat# a b = a `E.neFloat#` b
ltFloat# :: Float# -> Float# -> Bool
ltFloat# a b = a `E.ltFloat#` b
leFloat# :: Float# -> Float# -> Bool
leFloat# a b = a `E.leFloat#` b

gtAddr# :: Addr# -> Addr# -> Bool
gtAddr# a b = a `E.gtAddr#` b
geAddr# :: Addr# -> Addr# -> Bool
geAddr# a b = a `E.geAddr#` b
eqAddr# :: Addr# -> Addr# -> Bool
eqAddr# a b = a `E.eqAddr#` b
neAddr# :: Addr# -> Addr# -> Bool
neAddr# a b = a `E.neAddr#` b
ltAddr# :: Addr# -> Addr# -> Bool
ltAddr# a b = a `E.ltAddr#` b
leAddr# :: Addr# -> Addr# -> Bool
leAddr# a b = a `E.leAddr#` b

sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Bool
sameMutableArray# a b = E.sameMutableArray# a b
sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Bool
sameMutableByteArray# a b = E.sameMutableByteArray# a b
sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Bool
sameMutableArrayArray# a b = E.sameMutableArrayArray# a b

sameMutVar# :: MutVar# s a -> MutVar# s a -> Bool
sameMutVar# a b = E.sameMutVar# a b
sameTVar# :: TVar# s a -> TVar# s a -> Bool
sameTVar# a b = E.sameTVar# a b
sameMVar# :: MVar# s a -> MVar# s a -> Bool
sameMVar# a b = E.sameMVar# a b

#endif