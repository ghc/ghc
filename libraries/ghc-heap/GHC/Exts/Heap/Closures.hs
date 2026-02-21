{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
-- Late cost centres introduce a thunk in the asBox function, which leads to
-- an additional wrapper being added to any value placed inside a box.
-- This can be removed once our boot compiler is no longer affected by #25212
{-# OPTIONS_GHC -fno-prof-late  #-}
{-# LANGUAGE NamedFieldPuns #-}

module GHC.Exts.Heap.Closures (
    -- * Closures
      Closure
    , GenClosure(..)
    , getClosureInfoTbl
    , getClosureInfoTbl_maybe
    , getClosurePtrArgs
    , getClosurePtrArgs_maybe
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , allClosures
    , closureSize

    -- * Stack
    , StgStackClosure
    , GenStgStackClosure(..)
    , StackFrame
    , GenStackFrame(..)
    , StackField
    , GenStackField(..)

    -- * Boxes
    , Box(..)
    , areBoxesEqual
    , asBox
    ) where

import GHC.Internal.Heap.Closures

import GHC.Internal.Data.Functor
import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Traversable

deriving instance Functor GenClosure
deriving instance Foldable GenClosure
deriving instance Traversable GenClosure

deriving instance Functor GenStgStackClosure
deriving instance Foldable GenStgStackClosure
deriving instance Traversable GenStgStackClosure

deriving instance Functor GenStackField
deriving instance Foldable GenStackField
deriving instance Traversable GenStackField

deriving instance Functor GenStackFrame
deriving instance Foldable GenStackFrame
deriving instance Traversable GenStackFrame
