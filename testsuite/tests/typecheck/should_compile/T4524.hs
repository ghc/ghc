{-# LANGUAGE 
        GADTs,
        TypeOperators,
        ScopedTypeVariables,
        RankNTypes,
        NoMonoLocalBinds
 #-}
{-# OPTIONS_GHC -O2 -w #-}
{-
  Copyright (C) 2002-2003 David Roundy

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING.  If not, write to
  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
  Boston, MA 02110-1301, USA.
-}

module T4524 where

import Data.Maybe ( mapMaybe )
import Control.Monad ( MonadPlus, mplus, msum, mzero )
import Unsafe.Coerce (unsafeCoerce)

newtype FileName = FN FilePath deriving ( Eq, Ord )

data FL a x z where
    (:>:) :: a x y -> FL a y z -> FL a x z
    NilFL :: FL a x x
data RL a x z where
    (:<:) :: a y z -> RL a x y -> RL a x z
    NilRL :: RL a x x
data (a1 :> a2) x y = forall z. (a1 x z) :> (a2 z y)
infixr 1 :>
data (a1 :< a2) x y = forall z. (a1 z y) :< (a2 x z)
infix 1 :<
infixr 5 :>:, :<:

data EqCheck a b where
    IsEq :: EqCheck a a
    NotEq :: EqCheck a b

class MyEq p => Invert p where
    invert :: p x y -> p y x
    identity :: p x x

class MyEq p where
    unsafeCompare :: p a b -> p c d -> Bool
    unsafeCompare a b = IsEq == (a =/\= unsafeCoerceP b)

    (=\/=) :: p a b -> p a c -> EqCheck b c
    a =\/= b | unsafeCompare a b = unsafeCoerceP IsEq
             | otherwise = NotEq

    (=/\=) :: p a c -> p b c -> EqCheck a b
    a =/\= b | IsEq == (a =\/= unsafeCoerceP b) = unsafeCoerceP IsEq
             | otherwise = NotEq

infix 4 =\/=, =/\=

class Commute p where
    commute :: (p :> p) x y -> Maybe ((p :> p) x y)

instance (MyEq p, Commute p) => MyEq (FL p) where
instance (MyEq p, Commute p) => MyEq (RL p) where
instance Commute p => Commute (RL p) where
instance (Commute p, Invert p) => Invert (RL p) where
instance (Invert p, Commute p) => Invert (FL p) where
instance Eq (EqCheck a b) where
instance MyEq FilePatchType where
instance Invert Patch where

instance MyEq Patch where
    unsafeCompare = eqPatches

eqPatches :: Patch x y -> Patch w z -> Bool
eqPatches (PP p1) (PP p2) = undefined
eqPatches (Merger _ _ p1a p1b) (Merger _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches (Regrem _ _ p1a p1b) (Regrem _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches _ _ = False

data Prim x y where
    FP :: !FileName -> !(FilePatchType x y) -> Prim x y

data FilePatchType x y = FilePatchType
                            deriving (Eq,Ord)

data Patch x y where
    PP :: Prim x y -> Patch x y
    Merger :: FL Patch x y
           -> RL Patch x b
           -> Patch c b
           -> Patch c d
           -> Patch x y
    Regrem :: FL Patch x y
           -> RL Patch x b
           -> Patch c b
           -> Patch c a
           -> Patch y x

data Sealed a where
    Sealed :: a x -> Sealed a
data FlippedSeal a y where
    FlippedSeal :: !(a x y) -> FlippedSeal a y

mapFlipped :: (forall x. a x y -> b x z) -> FlippedSeal a y -> FlippedSeal b z
mapFlipped f (FlippedSeal x) = FlippedSeal (f x)

headPermutationsRL :: Commute p => RL p x y -> [RL p x y]
headPermutationsRL NilRL = []
headPermutationsRL (p:<:ps) =
    (p:<:ps) : mapMaybe (swapfirstRL.(p:<:)) (headPermutationsRL ps)
        where swapfirstRL (p1:<:p2:<:xs) = do p1':>p2' <- commute (p2:>p1)
                                              Just $ p2':<:p1':<:xs
              swapfirstRL _ = Nothing

is_filepatch :: Prim x y -> Maybe FileName
is_filepatch (FP f _) = Just f
is_filepatch _ = Nothing

toFwdCommute :: (Commute p, Commute q, Monad m)
             => ((p :< q) x y -> m ((q :< p) x y))
             -> (q :> p) x y -> m ((p :> q) x y)
toFwdCommute c (x :> y) = do x' :< y' <- c (y :< x)
                             return (y' :> x')

unsafeUnseal :: Sealed a -> a x
unsafeUnseal (Sealed a) = unsafeCoerceP1 a

unsafeUnsealFlipped :: FlippedSeal a y -> a x y
unsafeUnsealFlipped (FlippedSeal a) = unsafeCoerceP a

unsafeCoerceP :: a x y -> a b c
unsafeCoerceP = unsafeCoerce

unsafeCoercePStart :: a x1 y -> a x2 y
unsafeCoercePStart = unsafeCoerce

unsafeCoercePEnd :: a x y1 -> a x y2
unsafeCoercePEnd = unsafeCoerce

unsafeCoerceP1 :: a x -> a y
unsafeCoerceP1 = unsafeCoerce

data Perhaps a = Unknown | Failed | Succeeded a

instance  Monad Perhaps where
    (Succeeded x) >>= k =  k x
    Failed   >>= _      =  Failed
    Unknown  >>= _      =  Unknown
    Failed   >> _       =  Failed
    (Succeeded _) >> k  =  k
    Unknown  >> k       =  k
    return              =  Succeeded
    fail _              =  Unknown

instance  MonadPlus Perhaps where
    mzero                 = Unknown
    Unknown `mplus` ys    = ys
    Failed  `mplus` _     = Failed
    (Succeeded x) `mplus` _ = Succeeded x

toMaybe :: Perhaps a -> Maybe a
toMaybe (Succeeded x) = Just x
toMaybe _ = Nothing

cleverCommute :: CommuteFunction -> CommuteFunction
cleverCommute c (p1:<p2) =
    case c (p1 :< p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed

speedyCommute :: CommuteFunction
speedyCommute (p1 :< p2) -- Deal with common case quickly!
    | p1_modifies /= Nothing && p2_modifies /= Nothing &&
      p1_modifies /= p2_modifies = undefined
    | otherwise = Unknown
    where p1_modifies = isFilepatchMerger p1
          p2_modifies = isFilepatchMerger p2

everythingElseCommute :: MaybeCommute -> CommuteFunction
everythingElseCommute _ x = undefined

unsafeMerger :: String -> Patch x y -> Patch x z -> Patch a b
unsafeMerger x p1 p2 = unsafeCoercePStart $ unsafeUnseal $ merger x p1 p2

mergerCommute :: (Patch :< Patch) x y -> Perhaps ((Patch :< Patch) x y)
mergerCommute (Merger _ _ p1 p2 :< pA)
    | unsafeCompare pA p1 = Succeeded (unsafeMerger "0.0" p2 p1 :< unsafeCoercePStart p2)
    | unsafeCompare pA (invert (unsafeMerger "0.0" p2 p1)) = Failed
mergerCommute (Merger _ _
                (Merger _ _ c b)
                (Merger _ _ c' a) :<
                Merger _ _ b' c'')
    | unsafeCompare b' b && unsafeCompare c c' && unsafeCompare c c'' = undefined
mergerCommute _ = Unknown

instance Commute Patch where
    commute x = toMaybe $ msum
                  [toFwdCommute speedyCommute x,
                   toFwdCommute (cleverCommute mergerCommute) x,
                   toFwdCommute (everythingElseCommute undefined) x
                  ]

isFilepatchMerger :: Patch x y -> Maybe FileName
isFilepatchMerger (PP p) = is_filepatch p
isFilepatchMerger (Regrem und unw p1 p2)
    = isFilepatchMerger (Merger und unw p1 p2)

type CommuteFunction = forall x y. (Patch :< Patch) x y -> Perhaps ((Patch :< Patch) x y)
type MaybeCommute = forall x y. (Patch :< Patch) x y -> Maybe ((Patch :< Patch) x y)

{- unwind, trueUnwind, reconcleUnwindings, and merger are most likely
 where the problem lies.  Everything above is just brought in to bring
 in enough context so that those four will compile. -}
unwind :: Patch x y -> Sealed (RL Patch x) -- Recreates a patch history in reverse.
unwind (Merger _ unwindings _ _) = Sealed unwindings
unwind p = Sealed (p :<: NilRL)

trueUnwind :: Patch x y -> Sealed (RL Patch x) -- Recreates a patch history in reverse.
trueUnwind p@(Merger _ _ p1 p2) =
    case (unwind p1, unwind p2) of
    (Sealed (_:<:p1s),Sealed (_:<:p2s)) ->
         Sealed (p :<: unsafeCoerceP p1 :<: unsafeUnsealFlipped (reconcileUnwindings p1s (unsafeCoercePEnd p2s)))

reconcileUnwindings :: RL Patch x z -> RL Patch y z -> FlippedSeal (RL Patch) z
reconcileUnwindings p1s NilRL = FlippedSeal p1s
reconcileUnwindings (p1:<:_) (p2:<:_) =
    case [undefined | p1s'@(_:<:_) <- headPermutationsRL (p1:<:undefined)] of
    ((_:<:p1s', _:<:p2s'):_) ->
        mapFlipped (undefined :<:) $ reconcileUnwindings p1s' (unsafeCoercePEnd p2s')

merger :: String -> Patch x y -> Patch x z -> Sealed (Patch y)
merger "0.0" p1 p2 = Sealed $ Merger undoit unwindings p1 p2
    where fake_p = Merger identity NilRL p1 p2
          unwindings = unsafeUnseal (trueUnwind fake_p)
          p = undefined
          undoit = undefined
