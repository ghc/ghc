{-# LANGUAGE ExperimentalDependentTypes, UnicodeSyntax #-}
module TypeLetPrint where

type F a = let b {- bind -} = G a
           in {- reuse -} H b b

type Layout a =
  let first = G a  -- layout
      next {- binding -} = H first first
  in H next first

type Braced a =
  let { first = G a {- separator -}; next = H first first; {- close -} }
  in H next first

type SignedLayout a =
  let result {- signature -} :: Bool -- kind
      result = G a
  in H result result

type SignedBraced a =
  let { result ∷ Bool {- separator -}; {- binding -} result = G a; {- close -} }
  in H result result

type PairLayout dimensions =
  let (rows {- left field -}, cols) = dimensions
  in H rows cols

type PairBraced dimensions =
  let { (rows, _) {- equation -} = dimensions; cols = G rows }
  in H rows cols

type NestedPairLayout dimensions =
  let ((width {- inner field -}, _), height) = dimensions
  in H width height

type NestedPairBraced dimensions =
  let { (width, {- next pair -} (_, height)) = dimensions }
  in H width height
