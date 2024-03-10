{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedSums, NoListTuplePuns #-}

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
) where

import GHC.Types

default ()
