{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedSums, NoListTuplePuns #-}
{-# LANGUAGE ExplicitForAll, StandaloneKindSignatures, PolyKinds, DataKinds, TypeFamilyDependencies #-}

{-
Module      :  Data.Sum.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  ghc-devs@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

This module exports the new user-syntax types for unboxed sums, which avoid the
ambiguity of the old punned names.
See the proposal for motivation and explanations:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst
-}
module Data.Sum.Experimental (
  Sum2#,
  Sum3#,
  Sum4#,
  Sum5#,
  Sum6#,
  Sum7#,
  Sum8#,
  Sum9#,
  Sum10#,
  Sum11#,
  Sum12#,
  Sum13#,
  Sum14#,
  Sum15#,
  Sum16#,
  Sum17#,
  Sum18#,
  Sum19#,
  Sum20#,
  Sum21#,
  Sum22#,
  Sum23#,
  Sum24#,
  Sum25#,
  Sum26#,
  Sum27#,
  Sum28#,
  Sum29#,
  Sum30#,
  Sum31#,
  Sum32#,
  Sum33#,
  Sum34#,
  Sum35#,
  Sum36#,
  Sum37#,
  Sum38#,
  Sum39#,
  Sum40#,
  Sum41#,
  Sum42#,
  Sum43#,
  Sum44#,
  Sum45#,
  Sum46#,
  Sum47#,
  Sum48#,
  Sum49#,
  Sum50#,
  Sum51#,
  Sum52#,
  Sum53#,
  Sum54#,
  Sum55#,
  Sum56#,
  Sum57#,
  Sum58#,
  Sum59#,
  Sum60#,
  Sum61#,
  Sum62#,
  Sum63#,

  -- * Type families
  TupleArgKind#,
  Sum#,
) where

import GHC.Internal.Types
import GHC.TypeLits
import Data.Tuple.Experimental (TupleArgKind#)

default ()

type Sum# :: forall (reps :: List RuntimeRep). TupleArgKind# reps -> TYPE (SumRep reps)
type family Sum# ts where
  Sum# () = TypeError (Text "GHC does not support empty unboxed sums. Consider Data.Void.Void instead.")
  Sum# (a :: TYPE r) = TypeError (Text "GHC does not support unary unboxed sums. Consider Data.Tuple.Solo# instead.")
  Sum# (a, b) = Sum2# a b
  Sum# (a, b, c) = Sum3# a b c
  Sum# (a, b, c, d) = Sum4# a b c d
  Sum# (a, b, c, d, e) = Sum5# a b c d e
  Sum# (a, b, c, d, e, f) = Sum6# a b c d e f
  Sum# (a, b, c, d, e, f, g) = Sum7# a b c d e f g
  Sum# (a, b, c, d, e, f, g, h) = Sum8# a b c d e f g h
  Sum# (a, b, c, d, e, f, g, h, i) = Sum9# a b c d e f g h i
  Sum# (a, b, c, d, e, f, g, h, i, j) = Sum10# a b c d e f g h i j
  Sum# (a, b, c, d, e, f, g, h, i, j, k) = Sum11# a b c d e f g h i j k
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l) = Sum12# a b c d e f g h i j k l
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m) = Sum13# a b c d e f g h i j k l m
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Sum14# a b c d e f g h i j k l m n
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Sum15# a b c d e f g h i j k l m n o
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = Sum16# a b c d e f g h i j k l m n o p
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = Sum17# a b c d e f g h i j k l m n o p q
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = Sum18# a b c d e f g h i j k l m n o p q r
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = Sum19# a b c d e f g h i j k l m n o p q r s
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = Sum20# a b c d e f g h i j k l m n o p q r s t
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = Sum21# a b c d e f g h i j k l m n o p q r s t u
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = Sum22# a b c d e f g h i j k l m n o p q r s t u v
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = Sum23# a b c d e f g h i j k l m n o p q r s t u v w
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = Sum24# a b c d e f g h i j k l m n o p q r s t u v w x
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = Sum25# a b c d e f g h i j k l m n o p q r s t u v w x y
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = Sum26# a b c d e f g h i j k l m n o p q r s t u v w x y z
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa) = Sum27# a b c d e f g h i j k l m n o p q r s t u v w x y z aa
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab) = Sum28# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac) = Sum29# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad) = Sum30# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae) = Sum31# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af) = Sum32# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag) = Sum33# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah) = Sum34# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai) = Sum35# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj) = Sum36# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak) = Sum37# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al) = Sum38# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am) = Sum39# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an) = Sum40# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao) = Sum41# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap) = Sum42# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq) = Sum43# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar) = Sum44# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as) = Sum45# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at) = Sum46# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au) = Sum47# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av) = Sum48# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw) = Sum49# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax) = Sum50# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay) = Sum51# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az) = Sum52# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba) = Sum53# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb) = Sum54# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc) = Sum55# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd) = Sum56# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be) = Sum57# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf) = Sum58# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg) = Sum59# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh) = Sum60# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi) = Sum61# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj) = Sum62# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj
  Sum# (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, aq, ar, as, at, au, av, aw, ax, ay, az, ba, bb, bc, bd, be, bf, bg, bh, bi, bj, bk) = Sum63# a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk
