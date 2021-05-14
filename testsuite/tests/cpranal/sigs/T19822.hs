{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T19822 where

import GHC.Exts
import Data.Char

data Text = MkText !Int Char

-- | Should have the CPR property
singleton :: Char -> Text
singleton c = MkText (runRW# (\_ -> 42 + ord c)) c
{-# NOINLINE singleton #-} -- to force WW
