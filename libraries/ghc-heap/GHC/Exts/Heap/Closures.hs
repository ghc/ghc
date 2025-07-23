{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
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



