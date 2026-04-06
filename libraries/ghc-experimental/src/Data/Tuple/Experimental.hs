{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, NoListTuplePuns #-}
{-# LANGUAGE ExplicitForAll, StandaloneKindSignatures, PolyKinds, DataKinds, TypeFamilyDependencies #-}

{-
Module      :  Data.Tuple.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  ghc-devs@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

This module exports the new user-syntax types for (boxed, unboxed, constraint)
tuples, which avoid the ambiguity of the old punned names.
See the proposal for motivation and explanations:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst
-}
module Data.Tuple.Experimental (
  module GHC.Internal.Tuple,
  Solo (Solo, MkSolo),

  -- * Unboxed tuples
  Unit#,
  Solo#(..),
  Tuple0#,
  Tuple1#,
  Tuple2#,
  Tuple3#,
  Tuple4#,
  Tuple5#,
  Tuple6#,
  Tuple7#,
  Tuple8#,
  Tuple9#,
  Tuple10#,
  Tuple11#,
  Tuple12#,
  Tuple13#,
  Tuple14#,
  Tuple15#,
  Tuple16#,
  Tuple17#,
  Tuple18#,
  Tuple19#,
  Tuple20#,
  Tuple21#,
  Tuple22#,
  Tuple23#,
  Tuple24#,
  Tuple25#,
  Tuple26#,
  Tuple27#,
  Tuple28#,
  Tuple29#,
  Tuple30#,
  Tuple31#,
  Tuple32#,
  Tuple33#,
  Tuple34#,
  Tuple35#,
  Tuple36#,
  Tuple37#,
  Tuple38#,
  Tuple39#,
  Tuple40#,
  Tuple41#,
  Tuple42#,
  Tuple43#,
  Tuple44#,
  Tuple45#,
  Tuple46#,
  Tuple47#,
  Tuple48#,
  Tuple49#,
  Tuple50#,
  Tuple51#,
  Tuple52#,
  Tuple53#,
  Tuple54#,
  Tuple55#,
  Tuple56#,
  Tuple57#,
  Tuple58#,
  Tuple59#,
  Tuple60#,
  Tuple61#,
  Tuple62#,
  Tuple63#,
  Tuple64#,

  -- * Constraint tuples
  CUnit,
  CSolo,
  CTuple0,
  CTuple1,
  CTuple2,
  CTuple3,
  CTuple4,
  CTuple5,
  CTuple6,
  CTuple7,
  CTuple8,
  CTuple9,
  CTuple10,
  CTuple11,
  CTuple12,
  CTuple13,
  CTuple14,
  CTuple15,
  CTuple16,
  CTuple17,
  CTuple18,
  CTuple19,
  CTuple20,
  CTuple21,
  CTuple22,
  CTuple23,
  CTuple24,
  CTuple25,
  CTuple26,
  CTuple27,
  CTuple28,
  CTuple29,
  CTuple30,
  CTuple31,
  CTuple32,
  CTuple33,
  CTuple34,
  CTuple35,
  CTuple36,
  CTuple37,
  CTuple38,
  CTuple39,
  CTuple40,
  CTuple41,
  CTuple42,
  CTuple43,
  CTuple44,
  CTuple45,
  CTuple46,
  CTuple47,
  CTuple48,
  CTuple49,
  CTuple50,
  CTuple51,
  CTuple52,
  CTuple53,
  CTuple54,
  CTuple55,
  CTuple56,
  CTuple57,
  CTuple58,
  CTuple59,
  CTuple60,
  CTuple61,
  CTuple62,
  CTuple63,
  CTuple64,

  -- * Type families
  TupleArgKind,
  Tuple,
  ConstraintsArgKind,
  Constraints,
  TupleArgKind#,
  Tuple#,
) where

import GHC.Internal.Tuple
import GHC.Internal.Types
import GHC.Internal.Classes
import GHC.TypeLits

default ()

type TupleArgKind :: Nat -> Type
type family TupleArgKind n = r | r -> n where
  TupleArgKind 0 = Unit
  TupleArgKind 1 = Type
  TupleArgKind 2 = Tuple2 Type Type
  TupleArgKind 3 = Tuple3 Type Type Type
  TupleArgKind 4 = Tuple4 Type Type Type Type
  TupleArgKind 5 = Tuple5 Type Type Type Type Type
  TupleArgKind 6 = Tuple6 Type Type Type Type Type Type
  TupleArgKind 7 = Tuple7 Type Type Type Type Type Type Type
  TupleArgKind 8 = Tuple8 Type Type Type Type Type Type Type Type
  TupleArgKind 9 = Tuple9 Type Type Type Type Type Type Type Type Type
  TupleArgKind 10 = Tuple10 Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 11 = Tuple11 Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 12 = Tuple12 Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 13 = Tuple13 Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 14 = Tuple14 Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 15 = Tuple15 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 16 = Tuple16 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 17 = Tuple17 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 18 = Tuple18 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 19 = Tuple19 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 20 = Tuple20 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 21 = Tuple21 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 22 = Tuple22 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 23 = Tuple23 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 24 = Tuple24 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 25 = Tuple25 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 26 = Tuple26 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 27 = Tuple27 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 28 = Tuple28 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 29 = Tuple29 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 30 = Tuple30 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 31 = Tuple31 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 32 = Tuple32 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 33 = Tuple33 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 34 = Tuple34 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 35 = Tuple35 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 36 = Tuple36 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 37 = Tuple37 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 38 = Tuple38 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 39 = Tuple39 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 40 = Tuple40 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 41 = Tuple41 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 42 = Tuple42 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 43 = Tuple43 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 44 = Tuple44 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 45 = Tuple45 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 46 = Tuple46 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 47 = Tuple47 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 48 = Tuple48 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 49 = Tuple49 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 50 = Tuple50 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 51 = Tuple51 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 52 = Tuple52 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 53 = Tuple53 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 54 = Tuple54 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 55 = Tuple55 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 56 = Tuple56 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 57 = Tuple57 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 58 = Tuple58 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 59 = Tuple59 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 60 = Tuple60 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 61 = Tuple61 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 62 = Tuple62 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 63 = Tuple63 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type
  TupleArgKind 64 = Tuple64 Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type Type

type Tuple :: forall (n :: Nat). TupleArgKind n -> Type
type family Tuple ts where
  Tuple () = Unit
  Tuple a = a
  Tuple (a, b) = Tuple2 a b
  Tuple (a, b, c) = Tuple3 a b c
  Tuple (a, b, c, d) = Tuple4 a b c d
  Tuple (a, b, c, d, e) = Tuple5 a b c d e
  Tuple (a, b, c, d, e, f) = Tuple6 a b c d e f
  Tuple (a, b, c, d, e, f, g) = Tuple7 a b c d e f g
  Tuple (a, b, c, d, e, f, g, h) = Tuple8 a b c d e f g h
  Tuple (a, b, c, d, e, f, g, h, i) = Tuple9 a b c d e f g h i
  Tuple (a, b, c, d, e, f, g, h, i, j) = Tuple10 a b c d e f g h i j
  Tuple (a, b, c, d, e, f, g, h, i, j, k) = Tuple11 a b c d e f g h i j k
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l) = Tuple12 a b c d e f g h i j k l
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m) = Tuple13 a b c d e f g h i j k l m
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Tuple14 a b c d e f g h i j k l m n
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Tuple15 a b c d e f g h i j k l m n o
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = Tuple16 a b c d e f g h i j k l m n o p
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = Tuple17 a b c d e f g h i j k l m n o p q
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = Tuple18 a b c d e f g h i j k l m n o p q r
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = Tuple19 a b c d e f g h i j k l m n o p q r s
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = Tuple20 a b c d e f g h i j k l m n o p q r s t
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = Tuple21 a b c d e f g h i j k l m n o p q r s t u
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = Tuple22 a b c d e f g h i j k l m n o p q r s t u v
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = Tuple23 a b c d e f g h i j k l m n o p q r s t u v w
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = Tuple24 a b c d e f g h i j k l m n o p q r s t u v w x
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = Tuple25 a b c d e f g h i j k l m n o p q r s t u v w x y
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = Tuple26 a b c d e f g h i j k l m n o p q r s t u v w x y z
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa) = Tuple27 a b c d e f g h i j k l m n o p q r s t u v w x y z aa
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab) = Tuple28 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac) = Tuple29 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad) = Tuple30 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae) = Tuple31 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af) = Tuple32 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag) = Tuple33 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah) = Tuple34 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai) = Tuple35 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj) = Tuple36 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak) = Tuple37 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al) = Tuple38 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am) = Tuple39 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an) = Tuple40 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao) = Tuple41 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap) = Tuple42 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq) = Tuple43 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar) = Tuple44 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as) = Tuple45 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at) = Tuple46 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au) = Tuple47 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av) = Tuple48 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw) = Tuple49 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax) = Tuple50 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay) = Tuple51 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az) = Tuple52 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba) = Tuple53 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb) = Tuple54 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc) = Tuple55 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd) = Tuple56 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be) = Tuple57 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf) = Tuple58 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg) = Tuple59 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh) = Tuple60 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi) = Tuple61 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj) = Tuple62 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk) = Tuple63 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk
  Tuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk, bl) = Tuple64 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk bl

type ConstraintsArgKind :: Nat -> Type
type family ConstraintsArgKind n = r | r -> n where
  ConstraintsArgKind 0 = Unit
  ConstraintsArgKind 1 = Constraint
  ConstraintsArgKind 2 = Tuple2 Constraint Constraint
  ConstraintsArgKind 3 = Tuple3 Constraint Constraint Constraint
  ConstraintsArgKind 4 = Tuple4 Constraint Constraint Constraint Constraint
  ConstraintsArgKind 5 = Tuple5 Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 6 = Tuple6 Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 7 = Tuple7 Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 8 = Tuple8 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 9 = Tuple9 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 10 = Tuple10 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 11 = Tuple11 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 12 = Tuple12 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 13 = Tuple13 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 14 = Tuple14 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 15 = Tuple15 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 16 = Tuple16 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 17 = Tuple17 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 18 = Tuple18 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 19 = Tuple19 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 20 = Tuple20 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 21 = Tuple21 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 22 = Tuple22 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 23 = Tuple23 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 24 = Tuple24 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 25 = Tuple25 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 26 = Tuple26 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 27 = Tuple27 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 28 = Tuple28 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 29 = Tuple29 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 30 = Tuple30 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 31 = Tuple31 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 32 = Tuple32 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 33 = Tuple33 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 34 = Tuple34 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 35 = Tuple35 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 36 = Tuple36 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 37 = Tuple37 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 38 = Tuple38 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 39 = Tuple39 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 40 = Tuple40 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 41 = Tuple41 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 42 = Tuple42 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 43 = Tuple43 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 44 = Tuple44 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 45 = Tuple45 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 46 = Tuple46 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 47 = Tuple47 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 48 = Tuple48 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 49 = Tuple49 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 50 = Tuple50 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 51 = Tuple51 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 52 = Tuple52 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 53 = Tuple53 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 54 = Tuple54 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 55 = Tuple55 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 56 = Tuple56 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 57 = Tuple57 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 58 = Tuple58 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 59 = Tuple59 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 60 = Tuple60 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 61 = Tuple61 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 62 = Tuple62 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 63 = Tuple63 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint
  ConstraintsArgKind 64 = Tuple64 Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint Constraint

type Constraints :: forall (n :: Nat). ConstraintsArgKind n -> Constraint
type family Constraints ts where
  Constraints () = CUnit
  Constraints a = a
  Constraints (a, b) = CTuple2 a b
  Constraints (a, b, c) = CTuple3 a b c
  Constraints (a, b, c, d) = CTuple4 a b c d
  Constraints (a, b, c, d, e) = CTuple5 a b c d e
  Constraints (a, b, c, d, e, f) = CTuple6 a b c d e f
  Constraints (a, b, c, d, e, f, g) = CTuple7 a b c d e f g
  Constraints (a, b, c, d, e, f, g, h) = CTuple8 a b c d e f g h
  Constraints (a, b, c, d, e, f, g, h, i) = CTuple9 a b c d e f g h i
  Constraints (a, b, c, d, e, f, g, h, i, j) = CTuple10 a b c d e f g h i j
  Constraints (a, b, c, d, e, f, g, h, i, j, k) = CTuple11 a b c d e f g h i j k
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l) = CTuple12 a b c d e f g h i j k l
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m) = CTuple13 a b c d e f g h i j k l m
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = CTuple14 a b c d e f g h i j k l m n
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = CTuple15 a b c d e f g h i j k l m n o
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = CTuple16 a b c d e f g h i j k l m n o p
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = CTuple17 a b c d e f g h i j k l m n o p q
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = CTuple18 a b c d e f g h i j k l m n o p q r
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = CTuple19 a b c d e f g h i j k l m n o p q r s
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = CTuple20 a b c d e f g h i j k l m n o p q r s t
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = CTuple21 a b c d e f g h i j k l m n o p q r s t u
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = CTuple22 a b c d e f g h i j k l m n o p q r s t u v
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = CTuple23 a b c d e f g h i j k l m n o p q r s t u v w
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = CTuple24 a b c d e f g h i j k l m n o p q r s t u v w x
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = CTuple25 a b c d e f g h i j k l m n o p q r s t u v w x y
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = CTuple26 a b c d e f g h i j k l m n o p q r s t u v w x y z
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa) = CTuple27 a b c d e f g h i j k l m n o p q r s t u v w x y z aa
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab) = CTuple28 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac) = CTuple29 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad) = CTuple30 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae) = CTuple31 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af) = CTuple32 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag) = CTuple33 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah) = CTuple34 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai) = CTuple35 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj) = CTuple36 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak) = CTuple37 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al) = CTuple38 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am) = CTuple39 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an) = CTuple40 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao) = CTuple41 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap) = CTuple42 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq) = CTuple43 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar) = CTuple44 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as) = CTuple45 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at) = CTuple46 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au) = CTuple47 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av) = CTuple48 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw) = CTuple49 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax) = CTuple50 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay) = CTuple51 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az) = CTuple52 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba) = CTuple53 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb) = CTuple54 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc) = CTuple55 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd) = CTuple56 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be) = CTuple57 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf) = CTuple58 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg) = CTuple59 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh) = CTuple60 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi) = CTuple61 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj) = CTuple62 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk) = CTuple63 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk
  Constraints (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk, bl) = CTuple64 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk bl

type TupleArgKind# :: List RuntimeRep -> Type
type family TupleArgKind# reps where
  TupleArgKind# [] = Unit
  TupleArgKind# [r1] = TYPE r1
  TupleArgKind# [a, b] = Tuple2 (TYPE a) (TYPE b)
  TupleArgKind# [a, b, c] = Tuple3 (TYPE a) (TYPE b) (TYPE c)
  TupleArgKind# [a, b, c, d] = Tuple4 (TYPE a) (TYPE b) (TYPE c) (TYPE d)
  TupleArgKind# [a, b, c, d, e] = Tuple5 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e)
  TupleArgKind# [a, b, c, d, e, f] = Tuple6 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f)
  TupleArgKind# [a, b, c, d, e, f, g] = Tuple7 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g)
  TupleArgKind# [a, b, c, d, e, f, g, h] = Tuple8 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h)
  TupleArgKind# [a, b, c, d, e, f, g, h, i] = Tuple9 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j] = Tuple10 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k] = Tuple11 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l] = Tuple12 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m] = Tuple13 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n] = Tuple14 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] = Tuple15 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = Tuple16 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] = Tuple17 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r] = Tuple18 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s] = Tuple19 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t] = Tuple20 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u] = Tuple21 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v] = Tuple22 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w] = Tuple23 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x] = Tuple24 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y] = Tuple25 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] = Tuple26 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa] = Tuple27 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab] = Tuple28 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac] = Tuple29 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad] = Tuple30 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae] = Tuple31 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af] = Tuple32 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag] = Tuple33 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah] = Tuple34 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai] = Tuple35 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj] = Tuple36 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak] = Tuple37 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al] = Tuple38 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am] = Tuple39 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an] = Tuple40 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao] = Tuple41 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap] = Tuple42 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq] = Tuple43 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar] = Tuple44 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as] = Tuple45 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at] = Tuple46 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au] = Tuple47 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av] = Tuple48 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw] = Tuple49 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax] = Tuple50 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay] = Tuple51 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az] = Tuple52 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba] = Tuple53 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb] = Tuple54 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc] = Tuple55 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd] = Tuple56 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be] = Tuple57 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf] = Tuple58 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg] = Tuple59 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh] = Tuple60 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg) (TYPE bh)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi] = Tuple61 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg) (TYPE bh) (TYPE bi)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj] = Tuple62 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg) (TYPE bh) (TYPE bi) (TYPE bj)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk] = Tuple63 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg) (TYPE bh) (TYPE bi) (TYPE bj) (TYPE bk)
  TupleArgKind# [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk, bl] = Tuple64 (TYPE a) (TYPE b) (TYPE c) (TYPE d) (TYPE e) (TYPE f) (TYPE g) (TYPE h) (TYPE i) (TYPE j) (TYPE k) (TYPE l) (TYPE m) (TYPE n) (TYPE o) (TYPE p) (TYPE q) (TYPE r) (TYPE s) (TYPE t) (TYPE u) (TYPE v) (TYPE w) (TYPE x) (TYPE y) (TYPE z) (TYPE aa) (TYPE ab) (TYPE ac) (TYPE ad) (TYPE ae) (TYPE af) (TYPE ag) (TYPE ah) (TYPE ai) (TYPE aj) (TYPE ak) (TYPE al) (TYPE am) (TYPE an) (TYPE ao) (TYPE ap) (TYPE aq) (TYPE ar) (TYPE as) (TYPE at) (TYPE au) (TYPE av) (TYPE aw) (TYPE ax) (TYPE ay) (TYPE az) (TYPE ba) (TYPE bb) (TYPE bc) (TYPE bd) (TYPE be) (TYPE bf) (TYPE bg) (TYPE bh) (TYPE bi) (TYPE bj) (TYPE bk) (TYPE bl)

type Tuple# :: forall (reps :: List RuntimeRep). TupleArgKind# reps -> TYPE (TupleRep reps)
type family Tuple# ts where
  Tuple# () = Unit#
  Tuple# (a :: TYPE r) = TypeError (Text "Tuple# does not work for 1-tuples; use Solo#.")
  Tuple# (a, b) = Tuple2# a b
  Tuple# (a, b, c) = Tuple3# a b c
  Tuple# (a, b, c, d) = Tuple4# a b c d
  Tuple# (a, b, c, d, e) = Tuple5# a b c d e
  Tuple# (a, b, c, d, e, f) = Tuple6# a b c d e f
  Tuple# (a, b, c, d, e, f, g) = Tuple7# a b c d e f g
  Tuple# (a, b, c, d, e, f, g, h) = Tuple8# a b c d e f g h
  Tuple# (a, b, c, d, e, f, g, h, i) = Tuple9# a b c d e f g h i
  Tuple# (a, b, c, d, e, f, g, h, i, j) = Tuple10# a b c d e f g h i j
  Tuple# (a, b, c, d, e, f, g, h, i, j, k) = Tuple11# a b c d e f g h i j k
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l) = Tuple12# a b c d e f g h i j k l
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m) = Tuple13# a b c d e f g h i j k l m
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Tuple14# a b c d e f g h i j k l m n
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Tuple15# a b c d e f g h i j k l m n o
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = Tuple16# a b c d e f g h i j k l m n o p
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = Tuple17# a b c d e f g h i j k l m n o p q
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = Tuple18# a b c d e f g h i j k l m n o p q r
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = Tuple19# a b c d e f g h i j k l m n o p q r s
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = Tuple20# a b c d e f g h i j k l m n o p q r s t
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = Tuple21# a b c d e f g h i j k l m n o p q r s t u
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = Tuple22# a b c d e f g h i j k l m n o p q r s t u v
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = Tuple23# a b c d e f g h i j k l m n o p q r s t u v w
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = Tuple24# a b c d e f g h i j k l m n o p q r s t u v w x
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = Tuple25# a b c d e f g h i j k l m n o p q r s t u v w x y
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = Tuple26# a b c d e f g h i j k l m n o p q r s t u v w x y z
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa) = Tuple27# a b c d e f g h i j k l m n o p q r s t u v w x y z aa
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab) = Tuple28# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac) = Tuple29# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad) = Tuple30# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae) = Tuple31# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af) = Tuple32# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag) = Tuple33# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah) = Tuple34# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai) = Tuple35# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj) = Tuple36# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak) = Tuple37# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al) = Tuple38# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am) = Tuple39# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an) = Tuple40# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao) = Tuple41# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap) = Tuple42# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq) = Tuple43# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar) = Tuple44# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as) = Tuple45# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at) = Tuple46# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au) = Tuple47# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av) = Tuple48# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw) = Tuple49# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax) = Tuple50# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay) = Tuple51# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az) = Tuple52# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba) = Tuple53# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb) = Tuple54# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc) = Tuple55# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd) = Tuple56# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be) = Tuple57# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf) = Tuple58# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg) = Tuple59# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh) = Tuple60# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi) = Tuple61# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj) = Tuple62# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk) = Tuple63# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk
  Tuple# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk, bl) = Tuple64# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk bl
