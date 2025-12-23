{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.DeepSeq
  ( force,
    forceIO,
  )
where

import GHC.IO
import GHC.Internal.Exts

-- | Pure wrapper around 'forceIO'.
force :: a -> (Bool, a)
force = unsafePerformIO . forceIO

-- | Deeply evaluate a value in the 'IO' monad, returning the forced value and
-- a flag indicating whether any unevaluated closure was forced.
--
-- This is a primitive analogue of 'Control.DeepSeq.force' / @rnf@ that does
-- not require an 'NFData' constraint. It traverses algebraic data (constructor
-- fields), immutable arrays, and the contents of 'MutVar#', 'MVar#',
-- 'MutableArray#', 'SmallMutableArray#', and live 'Weak#' values.
--
-- To mimic typical 'Control.DeepSeq.NFData' instances, it stops at
-- function-like closures (e.g. functions and partial applications) and at
-- mutable objects which are not plain containers (e.g. 'TVar#'). Consequently
-- it is not a drop-in replacement for user-defined 'NFData' instances, which
-- may choose to force less (or more) depending on semantics.
--
-- === Pointer traversal policy
--
-- We only follow a pointer when doing so is also possible in Haskell via a
-- corresponding API. For example, we traverse 'MutVar#', 'MVar#', mutable
-- arrays, and live weak pointers because you can observe their contents with
-- operations like @readIORef@, @readMVar@, @readArray@, or @deRefWeak@.
-- Conversely, we do not peek inside closures whose internals are not
-- observable from Haskell, such as function closures and their captured free
-- variables.
--
-- Like any deep evaluation, it may not terminate on cyclic structures.
forceIO :: a -> IO (Bool, a)
forceIO a = IO $ \s0 -> case deepseq# a s0 of
  (# s1, flag#, a' #) -> (# s1, (isTrue# flag#, a') #)
