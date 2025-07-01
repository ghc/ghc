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
-- See Note [Breakpoint identifiers]
module GHC.HsToCore.Breakpoints
  ( -- * ModBreaks
    mkModBreaks, ModBreaks(..)

    -- ** Re-exports BreakpointId
  , BreakpointId(..), BreakTickIndex
  ) where

import GHC.Prelude
import Data.Array

import GHC.HsToCore.Ticks (Tick (..))
import GHC.Data.SizedSeq
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.Name (OccName)
import GHC.Types.Tickish (BreakTickIndex, BreakpointId(..))
import GHC.Unit.Module (Module)
import GHC.Utils.Outputable
import Data.List (intersperse)

import GHCi.BreakArray (BreakArray)
import GHCi.RemoteTypes (ForeignRef)

-- TODO: Break this cycle
import {-# SOURCE #-} GHC.Runtime.Interpreter.Types (Interp, interpreterProfiled)
import {-# SOURCE #-} qualified GHC.Runtime.Interpreter as GHCi (newBreakArray)
import Data.Array.Base (numElements)

--------------------------------------------------------------------------------
-- ModBreaks
--------------------------------------------------------------------------------

-- | All the information about the source-relevant breakpoints for a module
--
-- This information is constructed once during desugaring (with `mkModBreaks`)
-- from breakpoint ticks and fixed/unchanged from there on forward. It could be
-- exported as an abstract datatype because it should never be updated after
-- construction, only queried.
--
-- The arrays can be indexed using the int in the corresponding 'BreakpointId'
-- (i.e. the 'BreakpointId' whose 'Module' matches the 'Module' corresponding
-- to these 'ModBreaks') with the accessors 'modBreaks_locs', 'modBreaks_vars',
-- and 'modBreaks_decls'.
data ModBreaks
   = ModBreaks
   { modBreaks_flags  :: ForeignRef BreakArray
        -- ^ The array of flags, one per breakpoint,
        -- indicating which breakpoints are enabled.
   , modBreaks_locs   :: !(Array BreakTickIndex SrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars   :: !(Array BreakTickIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls  :: !(Array BreakTickIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
        -- See Note [Field modBreaks_decls]
   , modBreaks_ccs    :: !(Array BreakTickIndex (String, String))
        -- ^ Array pointing to cost centre info for each breakpoint;
        -- actual 'CostCentre' allocation is done at link-time.
   , modBreaks_module :: !Module
        -- ^ The module to which this ModBreaks is associated.
        -- We also cache this here for internal sanity checks.
   }

-- | Initialize memory for breakpoint data that is shared between the bytecode
-- generator and the interpreter.
--
-- Since GHCi and the RTS need to interact with breakpoint data and the bytecode
-- generator needs to encode this information for each expression, the data is
-- allocated remotely in GHCi's address space and passed to the codegen as
-- foreign pointers.
mkModBreaks :: Interp -> Module -> SizedSeq Tick -> IO ModBreaks
mkModBreaks interp mod extendedMixEntries
  = do
    let count = fromIntegral $ sizeSS extendedMixEntries
        entries = ssElts extendedMixEntries
    let
           locsTicks  = listArray (0,count-1) [ tick_loc  t | t <- entries ]
           varsTicks  = listArray (0,count-1) [ tick_ids  t | t <- entries ]
           declsTicks = listArray (0,count-1) [ tick_path t | t <- entries ]
           ccs
             | interpreterProfiled interp =
                 listArray
                   (0, count - 1)
                   [ ( concat $ intersperse "." $ tick_path t,
                       renderWithContext defaultSDocContext $ ppr $ tick_loc t
                     )
                   | t <- entries
                   ]
             | otherwise = listArray (0, -1) []
    hydrateModBreaks interp $
      ModBreaks
        { modBreaks_flags = undefined,
          modBreaks_locs = locsTicks,
          modBreaks_vars = varsTicks,
          modBreaks_decls = declsTicks,
          modBreaks_ccs = ccs,
          modBreaks_module = mod
        }

hydrateModBreaks :: Interp -> ModBreaks -> IO ModBreaks
hydrateModBreaks interp ModBreaks {..} = do
  let count = numElements modBreaks_locs
  modBreaks_flags <- GHCi.newBreakArray interp count
  pure ModBreaks {..}

{-
Note [Field modBreaks_decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A value of eg ["foo", "bar", "baz"] in a `modBreaks_decls` field means:
The breakpoint is in the function called "baz" that is declared in a `let`
or `where` clause of a declaration called "bar", which itself is declared
in a `let` or `where` clause of the top-level function called "foo".
-}
