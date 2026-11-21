{-# LANGUAGE ExperimentalDependentTypes #-}
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
