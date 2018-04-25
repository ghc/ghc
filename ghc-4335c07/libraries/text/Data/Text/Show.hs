{-# LANGUAGE CPP, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module      : Data.Text.Show
-- Copyright   : (c) 2009-2015 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.Show
    (
      singleton
    , unpack
    , unpackCString#
    ) where

import Control.Monad.ST (ST)
import Data.Text.Internal (Text(..), empty_, safe)
import Data.Text.Internal.Fusion (stream, unstream)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import GHC.Prim (Addr#)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Fusion.Common as S

#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.CString as GHC
#else
import qualified GHC.Base as GHC
#endif

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

-- | /O(n)/ Convert a 'Text' into a 'String'.  Subject to fusion.
unpack :: Text -> String
unpack = S.unstreamList . stream
{-# INLINE [1] unpack #-}

-- | /O(n)/ Convert a literal string into a 'Text'.  Subject to
-- fusion.
--
-- This is exposed solely for people writing GHC rewrite rules.
unpackCString# :: Addr# -> Text
unpackCString# addr# = unstream (S.streamCString# addr#)
{-# NOINLINE unpackCString# #-}

{-# RULES "TEXT literal" [1] forall a.
    unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
      = unpackCString# a #-}

{-# RULES "TEXT literal UTF8" [1] forall a.
    unstream (S.map safe (S.streamList (GHC.unpackCStringUtf8# a)))
      = unpackCString# a #-}

{-# RULES "TEXT empty literal" [1]
    unstream (S.map safe (S.streamList []))
      = empty_ #-}

{-# RULES "TEXT singleton literal" [1] forall a.
    unstream (S.map safe (S.streamList [a]))
      = singleton_ a #-}

-- | /O(1)/ Convert a character into a Text.  Subject to fusion.
-- Performs replacement on invalid scalar values.
singleton :: Char -> Text
singleton = unstream . S.singleton . safe
{-# INLINE [1] singleton #-}

{-# RULES "TEXT singleton" forall a.
    unstream (S.singleton (safe a))
      = singleton_ a #-}

-- This is intended to reduce inlining bloat.
singleton_ :: Char -> Text
singleton_ c = Text (A.run x) 0 len
  where x :: ST s (A.MArray s)
        x = do arr <- A.new len
               _ <- unsafeWrite arr 0 d
               return arr
        len | d < '\x10000' = 1
            | otherwise     = 2
        d = safe c
{-# NOINLINE singleton_ #-}
