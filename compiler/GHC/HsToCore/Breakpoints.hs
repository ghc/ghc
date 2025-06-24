{-# LANGUAGE RecordWildCards #-}

module GHC.HsToCore.Breakpoints
  ( mkModBreaks,
    hydrateModBreaks
  ) where

import GHC.Prelude

import qualified GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Interpreter
import GHC.ByteCode.Types
import GHC.Unit

import GHC.HsToCore.Ticks (Tick (..))

import GHC.Data.SizedSeq
import GHC.Utils.Outputable as Outputable

import Data.List (intersperse)
import Data.Array
import Data.Array.Base (numElements)
import qualified Data.IntMap as IntMap

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
          modBreaks_breakInfo = IntMap.empty,
          modBreaks_module = mod
        }

hydrateModBreaks :: Interp -> ModBreaks -> IO ModBreaks
hydrateModBreaks interp ModBreaks {..} = do
  let count = numElements modBreaks_locs
  modBreaks_flags <- GHCi.newBreakArray interp count
  pure ModBreaks {..}
