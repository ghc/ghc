{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Test11018 where

nonUnicode :: forall a . a -> IO Int
nonUnicode _ = do
  x <- readChar
  return 4

-- ^ An opaque ESD handle for recording data from the soundcard via ESD.
data Recorder fr ch (r ∷ * -> *)
    = Recorder {
        reCloseH :: !(FinalizerHandle r)
      }

f :: Arrow a => a (Int,Int,Int) Int
f = proc (x,y,z) -> returnA -< x+y

f2 :: Arrow a => a (Int,Int,Int) Int
f2 = proc (x,y,z) -> returnA >- x+y

g :: ArrowApply a => Int -> a (a Int Int,Int) Int
g y = proc (x,z) -> x -<< 2+y

g2 :: ArrowApply a => Int -> a (a Int Int,Int) Int
g2 y = proc (x,z) -> x >>- 2+y

-- -------------------------------------

unicode ∷ ∀ a . a → IO Int
unicode _ = do
  x ← readChar
  return 4

-- ^ An opaque ESD handle for recording data from the soundcard via ESD.
data RecorderU fr ch (r ∷ ★ → ★)
    = RecorderU {
        reCloseHU ∷ !(FinalizerHandle r)
      }

fU :: Arrow a  ⇒ a (Int,Int,Int) Int
fU = proc (x,y,z) -> returnA ⤙ x+y

f2U :: Arrow a ⇒ a (Int,Int,Int) Int
f2U = proc (x,y,z) -> returnA ⤚ x+y

gU :: ArrowApply a ⇒ Int -> a (a Int Int,Int) Int
gU y = proc (x,z) -> x ⤛ 2+y

g2U :: ArrowApply a ⇒ Int -> a (a Int Int,Int) Int
g2U y = proc (x,z) -> x ⤜ 2+y
