{-# LANGUAGE RecordWildCards #-}

-- | Information attached to Breakpoints generated from Ticks
--
-- The breakpoint information stored in 'ModBreaks' is generated during
-- desugaring from the ticks annotating the source expressions.
--
-- This information can be queried per-breakpoint using the 'BreakpointId'
-- datatype, which indexes tick-level breakpoint information.
--
-- 'ModBreaks' and 'BreakpointId's are not to be confused with
-- 'InternalModBreaks' and 'InternalBreakId's. The latter are constructed
-- during bytecode generation and can be found in 'GHC.ByteCode.Breakpoints'.
--
-- See Note [ModBreaks vs InternalModBreaks] and Note [Breakpoint identifiers]
module GHC.HsToCore.Breakpoints
  ( -- * ModBreaks
    mkModBreaks, ModBreaks(..), modBreaks_locs

    -- ** Re-exports BreakpointId
  , BreakpointId(..), BreakTickIndex
  ) where

import GHC.Prelude
import Data.Array

import GHC.HsToCore.Ticks (Tick (..))
import GHC.HsToCore.Breakpoints.Types
import GHC.Data.SizedSeq
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable
import Data.List (intersperse)
import GHC.Utils.Binary (BinSrcSpan(BinSrcSpan))
-- | Initialize memory for breakpoint data that is shared between the bytecode
-- generator and the interpreter.
--
-- Since GHCi and the RTS need to interact with breakpoint data and the bytecode
-- generator needs to encode this information for each expression, the data is
-- allocated remotely in GHCi's address space and passed to the codegen as
-- foreign pointers.
mkModBreaks :: Bool {-^ Whether the interpreter is profiled and thus if we should include store a CCS array -}
            -> Module -> SizedSeq Tick -> ModBreaks
mkModBreaks interpreterProfiled modl extendedMixEntries
  = let count = fromIntegral $ sizeSS extendedMixEntries
        entries = ssElts extendedMixEntries
        locsTicks  = listArray (0,count-1) [ tick_loc  t | t <- entries ]
        varsTicks  = listArray (0,count-1) [ tick_ids  t | t <- entries ]
        declsTicks = listArray (0,count-1) [ tick_path t | t <- entries ]
        ccs
          | interpreterProfiled =
              listArray
                (0, count - 1)
                [ ( concat $ intersperse "." $ tick_path t,
                    renderWithContext defaultSDocContext $ ppr $ tick_loc t
                  )
                | t <- entries
                ]
          | otherwise = listArray (0, -1) []
     in ModBreaks
      { modBreaks_locs_  = fmap BinSrcSpan locsTicks
      , modBreaks_vars   = varsTicks
      , modBreaks_decls  = declsTicks
      , modBreaks_ccs    = ccs
      , modBreaks_module = modl
      }

{-
Note [Field modBreaks_decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A value of eg ["foo", "bar", "baz"] in a `modBreaks_decls` field means:
The breakpoint is in the function called "baz" that is declared in a `let`
or `where` clause of a declaration called "bar", which itself is declared
in a `let` or `where` clause of the top-level function called "foo".
-}
