-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2016 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Internal definitions not to be used by the user of StaticPtr's.

-- By ignoring interface pragmas, we drop the stricness annotations
-- of 'error', which otherwise biase GHC to conclude that any code
-- using the static form would fail.
{-# OPTIONS_GHC -fignore-interface-pragmas #-}
module GHC.StaticPtr.Internal (makeStatic) where

import GHC.StaticPtr(StaticPtr, StaticPtrInfo(..))

{-# NOINLINE makeStatic #-}
makeStatic :: StaticPtrInfo -> a -> StaticPtr a
makeStatic (StaticPtrInfo pkg m (line, col)) _ =
    error $ "makeStatic: Unresolved static form at " ++ pkg ++ ":" ++ m ++ ":"
            ++ show line ++ ":" ++ show col
