-- |
-- Module      :  GHC.Internal.StaticPtr
-- Copyright   :  (C) 2016 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Internal definitions needed for compiling static forms.
--

-- By omitting interface pragmas, we drop the strictness annotations
-- which otherwise would bias GHC to conclude that any code using
-- the static form would fail.
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
module GHC.Internal.StaticPtr.Internal (makeStatic) where

import GHC.Internal.Base
import GHC.Internal.StaticPtr(StaticPtr)
import GHC.Internal.Text.Show

-- 'makeStatic' should never be called by the user.
-- See Note [Grand plan for static forms] in StaticPtrTable.

makeStatic :: (Int, Int) -> a -> StaticPtr a
makeStatic (line, col) _ =
    error $ "GHC bug - makeStatic: Unresolved static form at line "
            ++ show line ++ ", column " ++ show col ++ "."
