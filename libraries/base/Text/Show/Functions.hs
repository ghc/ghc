{-# LANGUAGE Safe #-}
-- This module deliberately declares orphan instances:
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Functions
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Optional instance of 'Text.Show.Show' for functions:
--
-- > instance Show (a -> b) where
-- >    showsPrec _ _ = showString \"\<function\>\"
--
-----------------------------------------------------------------------------

module Text.Show.Functions () where

instance Show (a -> b) where
        showsPrec _ _ = showString "<function>"

