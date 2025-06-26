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
    mkModBreaks, ModBreaks(modBreaks_locs, modBreaks_vars, modBreaks_decls)

    -- ** Queries
  , getBreakLoc, getBreakVars, getBreakDecls

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
import GHC.Utils.Panic

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
   { modBreaks_locs :: !(Array BreakTickIndex SrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars :: !(Array BreakTickIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls :: !(Array BreakTickIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
        -- See Note [Field modBreaks_decls]
   , modBreaks_module :: !Module
        -- ^ The module to which this ModBreaks is associated.
        -- We cache this here for internal sanity checks (don't export it!).
   }

-- | Initialize memory for breakpoint data that is shared between the bytecode
-- generator and the interpreter.
--
-- Since GHCi and the RTS need to interact with breakpoint data and the bytecode
-- generator needs to encode this information for each expression, the data is
-- allocated remotely in GHCi's address space and passed to the codegen as
-- foreign pointers.
mkModBreaks :: Module -> SizedSeq Tick -> ModBreaks
mkModBreaks modl extendedMixEntries
  = let count = fromIntegral $ sizeSS extendedMixEntries
        entries = ssElts extendedMixEntries
        locsTicks  = listArray (0,count-1) [ tick_loc  t | t <- entries ]
        varsTicks  = listArray (0,count-1) [ tick_ids  t | t <- entries ]
        declsTicks = listArray (0,count-1) [ tick_path t | t <- entries ]
     in ModBreaks
      { modBreaks_locs   = locsTicks
      , modBreaks_vars   = varsTicks
      , modBreaks_decls  = declsTicks
      , modBreaks_module = modl
      }

-- | Get the source span for this breakpoint
getBreakLoc  :: BreakpointId -> ModBreaks -> SrcSpan
getBreakLoc (BreakpointId bid_mod ix) mbs =
  assert_modules_match bid_mod (modBreaks_module mbs) $ modBreaks_locs mbs ! ix

-- | Get the vars for this breakpoint
getBreakVars  :: BreakpointId -> ModBreaks -> [OccName]
getBreakVars (BreakpointId bid_mod ix) mbs =
  assert_modules_match bid_mod (modBreaks_module mbs) $ modBreaks_vars mbs ! ix

-- | Get the decls for this breakpoint
getBreakDecls :: BreakpointId -> ModBreaks -> [String]
getBreakDecls (BreakpointId bid_mod ix) mbs =
  assert_modules_match bid_mod (modBreaks_module mbs) $ modBreaks_decls mbs ! ix

-- | Assert that the module in the 'BreakpointId' and in 'ModBreaks' match.
assert_modules_match :: Module -> Module -> a -> a
assert_modules_match bid_mod mbs_mod =
  assertPpr (bid_mod == mbs_mod)
    (text "Tried to query the ModBreaks of module" <+> ppr mbs_mod
        <+> text "with a BreakpointId for module" <+> ppr bid_mod)

{-
Note [Field modBreaks_decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A value of eg ["foo", "bar", "baz"] in a `modBreaks_decls` field means:
The breakpoint is in the function called "baz" that is declared in a `let`
or `where` clause of a declaration called "bar", which itself is declared
in a `let` or `where` clause of the top-level function called "foo".
-}

