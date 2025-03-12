-- | GHC API debugger module for finding and setting breakpoints.
--
-- This module is user facing and is at least used by `GHCi` and `ghc-debugger`
-- to find and set breakpoints.
module GHC.Runtime.Debugger.Breakpoints where

import GHC.Prelude
import GHC.Types.SrcLoc
import qualified GHC.Data.Strict as Strict
import qualified Data.Semigroup as S
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import GHC
import Data.Array

--------------------------------------------------------------------------------
-- Finding breakpoints
--------------------------------------------------------------------------------

-- | Find a breakpoint given a Module's 'TickArray' and the line number.
--
-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int {-^ Line number -} -> TickArray -> Maybe (BreakIndex, RealSrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmostLargestRealSrcSpan `on` snd)  comp)   `mplus`
    listToMaybe (sortBy (compare `on` snd) incomp) `mplus`
    listToMaybe (sortBy (flip compare `on` snd) ticks)
  where
        ticks = arr ! line

        starts_here = [ (ix,pan) | (ix, pan) <- ticks,
                        GHC.srcSpanStartLine pan == line ]

        (comp, incomp) = partition ends_here starts_here
            where ends_here (_,pan) = GHC.srcSpanEndLine pan == line

-- | The aim of this function is to find the breakpoints for all the RHSs of
-- the equations corresponding to a binding. So we find all breakpoints
-- for
--   (a) this binder only (it maybe a top-level or a nested declaration)
--   (b) that do not have an enclosing breakpoint
findBreakForBind :: String {-^ Name of bind to break at -} -> GHC.ModBreaks -> [(BreakIndex, RealSrcSpan)]
findBreakForBind str_name modbreaks = filter (not . enclosed) ticks
  where
    ticks = [ (index, span)
            | (index, decls) <- assocs (GHC.modBreaks_decls modbreaks),
              str_name == intercalate "." decls,
              RealSrcSpan span _ <- [GHC.modBreaks_locs modbreaks ! index] ]
    enclosed (_,sp0) = any subspan ticks
      where subspan (_,sp) = sp /= sp0 &&
                         realSrcSpanStart sp <= realSrcSpanStart sp0 &&
                         realSrcSpanEnd sp0 <= realSrcSpanEnd sp

-- | Find a breakpoint in the 'TickArray' of a module, given a line number and a column coordinate.
findBreakByCoord :: (Int, Int) -> TickArray -> Maybe (BreakIndex, RealSrcSpan)
findBreakByCoord (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (flip compare `on` snd) contains ++
                 sortBy (compare `on` snd) after_here)
  where
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(_,pan) <- ticks, RealSrcSpan pan Strict.Nothing `spans` (line,col) ]

        after_here = [ tick | tick@(_,pan) <- ticks,
                              GHC.srcSpanStartLine pan == line,
                              GHC.srcSpanStartCol pan >= col ]

leftmostLargestRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
leftmostLargestRealSrcSpan = on compare realSrcSpanStart S.<> on (flip compare) realSrcSpanEnd

--------------------------------------------------------------------------------
-- Mapping line numbers to ticks
--------------------------------------------------------------------------------

-- | Maps line numbers to the breakpoint ticks existing at that line for a module.
type TickArray = Array Int [(GHC.BreakIndex,RealSrcSpan)]

-- | Construct the 'TickArray' for the given module.
makeModuleLineMap :: GhcMonad m => Module -> m (Maybe TickArray)
makeModuleLineMap m = do
  mi <- GHC.getModuleInfo m
  return $
    mkTickArray . assocs . GHC.modBreaks_locs . GHC.modInfoModBreaks <$> mi
  where
    mkTickArray :: [(BreakIndex, SrcSpan)] -> TickArray
    mkTickArray ticks
      = accumArray (flip (:)) [] (1, max_line)
            [ (line, (nm,pan)) | (nm,RealSrcSpan pan _) <- ticks, line <- srcSpanLines pan ]
        where
            max_line = foldr max 0 [ GHC.srcSpanEndLine sp | (_, RealSrcSpan sp _) <- ticks ]
            srcSpanLines pan = [ GHC.srcSpanStartLine pan ..  GHC.srcSpanEndLine pan ]



