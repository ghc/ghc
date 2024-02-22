{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.ForeignPtr
-- Copyright   :  (c) The University of Glasgow, 1992-2003
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's implementation of the 'ForeignPtr' data type.
--

module GHC.ForeignPtr
  (
        -- * Types
        ForeignPtr(..),
        ForeignPtrContents(..),
        Finalizers(..),
        FinalizerPtr,
        FinalizerEnvPtr,
        -- * Create
        newForeignPtr_,
        mallocForeignPtr,
        mallocPlainForeignPtr,
        mallocForeignPtrBytes,
        mallocPlainForeignPtrBytes,
        mallocForeignPtrAlignedBytes,
        mallocPlainForeignPtrAlignedBytes,
        newConcForeignPtr,
        -- * Add Finalizers
        addForeignPtrFinalizer,
        addForeignPtrFinalizerEnv,
        addForeignPtrConcFinalizer,
        -- * Conversion
        unsafeForeignPtrToPtr,
        castForeignPtr,
        plusForeignPtr,
        -- * Control over lifetype
        withForeignPtr,
        unsafeWithForeignPtr,
        touchForeignPtr,
        -- * Finalization
        finalizeForeignPtr
        -- * Commentary
        -- $commentary
  ) where

import GHC.Internal.ForeignPtr

{- $commentary

This is a high-level overview of how 'ForeignPtr' works.
The implementation of 'ForeignPtr' must accomplish several goals:

1. Invoke a finalizer once a foreign pointer becomes unreachable.
2. Support augmentation of finalizers, i.e. 'addForeignPtrFinalizer'.
   As a motivating example, suppose that the payload of a foreign
   pointer is C struct @bar@ that has an optionally NULL pointer field
   @foo@ to an unmanaged heap object. Initially, @foo@ is NULL, and
   later the program uses @malloc@, initializes the object, and assigns
   @foo@ the address returned by @malloc@. When the foreign pointer
   becomes unreachable, it is now necessary to first @free@ the object
   pointed to by @foo@ and then invoke whatever finalizer was associated
   with @bar@. That is, finalizers must be invoked in the opposite order
   they are added.
3. Allow users to invoke a finalizer promptly if they know that the
   foreign pointer is unreachable, i.e. 'finalizeForeignPtr'.

How can these goals be accomplished? Goal 1 suggests that weak references
and finalizers (via 'Weak#' and 'mkWeak#') are necessary. But how should
they be used and what should their key be?  Certainly not 'ForeignPtr' or
'ForeignPtrContents'. See the warning in "GHC.Weak" about weak pointers with
lifted (non-primitive) keys. The two finalizer-supporting data constructors of
'ForeignPtr' have an @'IORef' 'Finalizers'@ (backed by 'MutVar#') field.
This gets used in two different ways depending on the kind of finalizer:

* 'HaskellFinalizers': The first @addForeignPtrConcFinalizer_@ call uses
  'mkWeak#' to attach the finalizer @foreignPtrFinalizer@ to the 'MutVar#'.
  The resulting 'Weak#' is discarded (see @addForeignPtrConcFinalizer_@).
  Subsequent calls to @addForeignPtrConcFinalizer_@ (goal 2) just add
  finalizers onto the list in the 'HaskellFinalizers' data constructor.
* 'CFinalizers': The first 'addForeignPtrFinalizer' call uses
  'mkWeakNoFinalizer#' to create a 'Weak#'. The 'Weak#' is preserved in the
  'CFinalizers' data constructor. Both the first call and subsequent
  calls (goal 2) use 'addCFinalizerToWeak#' to attach finalizers to the
  'Weak#' itself. Also, see Note [MallocPtr finalizers] for discussion of
  the key and value of this 'Weak#'.

In either case, the runtime invokes the appropriate finalizers when the
'ForeignPtr' becomes unreachable.
-}
