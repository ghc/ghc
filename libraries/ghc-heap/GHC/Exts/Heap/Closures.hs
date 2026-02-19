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
