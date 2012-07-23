{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  PChan
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- An implementation of PChan with pluggable scheduler.
--
-----------------------------------------------------------------------------

module PChan
( PChan
, newPChan  -- PTM (PChan a)
, writePChan       -- PChan a -> a -> PTM ()
, readPChan      -- PChan a -> PTM a
) where

import Prelude
import LwConc.Substrate
import qualified Data.Sequence as Seq
import GHC.Prim
import GHC.IORef


newtype PChan a = PChan (PVar (Seq.Seq a)) deriving (Eq)

newPChan :: PTM (PChan a)
newPChan = do
  ref <- newPVar $ Seq.empty
  return $ PChan ref

{-# INLINE writePChan #-}
writePChan :: PChan a -> a -> PTM ()
writePChan (PChan ref) x = do
  st <- readPVar ref
  writePVar ref $ st Seq.|> x


{-# INLINE readPChan #-}
readPChan :: PChan a -> PTM a
readPChan (PChan ref) = do
  st <- readPVar ref
  case st of
       (Seq.viewl -> Seq.EmptyL) -> do
         readPChan (PChan ref)
       (Seq.viewl -> x Seq.:< tl) -> do
         writePVar ref $ tl
         return x
