{-# OPTIONS_GHC -O2 -fforce-recomp #-}

-- | The gist here: `f` is strict in `t::T` and its field `h::Huge`, but mustn't unbox it.
-- Otherwise, `$wf` has to rebox it for the call to `$wg` (which is lazy in `t`) and that
-- also means reconstructing `h`, although most fields are absent anyway.
--
-- Solution: `g` is lazy in `t` and we can't unbox it. Thus, its signature
-- shouldn't say `Unboxed` for `t`!
module T19407 where

data Huge = Huge Bool () () () () () () () () () () () () () () () () () () () () ()
data T = T { h :: Huge, n :: Int }

f :: T -> Int -- like warnAboutOverflowedLit
f t = g (h t) t
{-# NOINLINE f #-}

g :: Huge -> T -> Int -- like warnAboutOverflowedLiterals
g (Huge b _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) t =
  if b then 0 else n t
{-# NOINLINE g #-}
