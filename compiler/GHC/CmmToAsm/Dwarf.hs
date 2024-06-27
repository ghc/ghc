module GHC.CmmToAsm.Dwarf (
  dwarfGen
  ) where

import GHC.Prelude

import GHC.Cmm.CLabel
import GHC.Cmm.Expr
import GHC.Data.FastString
import GHC.Settings.Config ( cProjectName, cProjectVersion )
import GHC.Types.Tickish   ( CmmTickish, GenTickish(..) )
import GHC.Cmm.DebugBlock
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique
import GHC.Types.Unique.DSM

import GHC.CmmToAsm.Dwarf.Constants
import GHC.CmmToAsm.Dwarf.Types
import GHC.CmmToAsm.Config

import Control.Arrow    ( first )
import Control.Monad    ( mfilter )
import Data.Maybe
import Data.List        ( sortBy )
import Data.Ord         ( comparing )
import qualified Data.Map as Map
import System.FilePath

import qualified GHC.Cmm.Dataflow.Label as H

-- | Generate DWARF/debug information
dwarfGen :: IsDoc doc => String -> NCGConfig -> ModLocation -> DUniqSupply -> [DebugBlock] -> (doc, DUniqSupply)
dwarfGen _        _      _      us []     = (empty, us)
dwarfGen compPath config modLoc us blocks =
  let platform = ncgPlatform config

      -- Convert debug data structures to DWARF info records
      procs = debugSplitProcs blocks
      stripBlocks dbg
        | ncgDwarfStripBlockInfo config = dbg { dblBlocks = [] }
        | otherwise                     = dbg
      lowLabel = dblCLabel $ head procs
      highLabel = mkAsmTempProcEndLabel $ dblCLabel $ last procs
      dwarfUnit = DwarfCompileUnit
        { dwChildren = map (procToDwarf config) (map stripBlocks procs)
        , dwName = fromMaybe "" (ml_hs_file modLoc)
        , dwCompDir = addTrailingPathSeparator compPath
        , dwProducer = cProjectName ++ " " ++ cProjectVersion
        , dwLowLabel = lowLabel
        , dwHighLabel = highLabel
        }

      -- Check whether we have any source code information, so we do not
      -- end up writing a pointer to an empty .debug_line section
      -- (dsymutil on Mac Os gets confused by this).
      haveSrcIn blk = isJust (dblSourceTick blk) && isJust (dblPosition blk)
                      || any haveSrcIn (dblBlocks blk)
      haveSrc = any haveSrcIn procs

  -- .debug_abbrev section: Declare the format we're using
      abbrevSct = pprAbbrevDecls platform haveSrc

  -- .debug_info section: Information records on procedures and blocks
      -- unique to identify start and end compilation unit .debug_inf
      (unitU, us') = takeUniqueFromDSupply us
      infoSct = vcat [ line (dwarfInfoLabel <> colon)
                     , dwarfInfoSection platform
                     , compileUnitHeader platform unitU
                     , pprDwarfInfo platform haveSrc dwarfUnit
                     , compileUnitFooter platform unitU
                     ]

  -- .debug_line section: Generated mainly by the assembler, but we
  -- need to label it
      lineSct = dwarfLineSection platform $$
                line (dwarfLineLabel <> colon)

  -- .debug_frame section: Information about the layout of the GHC stack
      (framesU, us'') = takeUniqueFromDSupply us'
      frameSct = dwarfFrameSection platform $$
                 line (dwarfFrameLabel <> colon) $$
                 pprDwarfFrame platform (debugFrame platform framesU procs)

  -- .aranges section: Information about the bounds of compilation units
      aranges' | ncgSplitSections config = map mkDwarfARange procs
               | otherwise               = [DwarfARange lowLabel highLabel]
      aranges = dwarfARangesSection platform $$ pprDwarfARanges platform aranges' unitU

  in (infoSct $$ abbrevSct $$ lineSct $$ frameSct $$ aranges, us'')
{-# SPECIALIZE dwarfGen :: String -> NCGConfig -> ModLocation -> DUniqSupply -> [DebugBlock] -> (SDoc, DUniqSupply) #-}
{-# SPECIALIZE dwarfGen :: String -> NCGConfig -> ModLocation -> DUniqSupply -> [DebugBlock] -> (HDoc, DUniqSupply) #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Build an address range entry for one proc.
-- With split sections, each proc needs its own entry, since they may get
-- scattered in the final binary. Without split sections, we could make a
-- single arange based on the first/last proc.
mkDwarfARange :: DebugBlock -> DwarfARange
mkDwarfARange proc = DwarfARange lbl end
  where
    lbl = dblCLabel proc
    end = mkAsmTempProcEndLabel lbl

-- | Header for a compilation unit, establishing global format
-- parameters
compileUnitHeader :: IsDoc doc => Platform -> Unique -> doc
compileUnitHeader platform unitU =
  let cuLabel = mkAsmTempLabel unitU  -- sits right before initialLength field
      length = pprAsmLabel platform (mkAsmTempEndLabel cuLabel) <> char '-' <> pprAsmLabel platform cuLabel
               <> text "-4"       -- length of initialLength field
  in vcat [ line (pprAsmLabel platform cuLabel <> colon)
          , line (text "\t.long " <> length)  -- compilation unit size
          , pprHalf 3                          -- DWARF version
          , sectionOffset platform dwarfAbbrevLabel dwarfAbbrevLabel
                                               -- abbrevs offset
          , line (text "\t.byte " <> int (platformWordSizeInBytes platform)) -- word size
          ]

-- | Compilation unit footer, mainly establishing size of debug sections
compileUnitFooter :: IsDoc doc => Platform -> Unique -> doc
compileUnitFooter platform unitU =
  let cuEndLabel = mkAsmTempEndLabel $ mkAsmTempLabel unitU
  in line (pprAsmLabel platform cuEndLabel <> colon)

-- | Splits the blocks by procedures. In the result all nested blocks
-- will come from the same procedure as the top-level block. See
-- Note [Splitting DebugBlocks] for details.
debugSplitProcs :: [DebugBlock] -> [DebugBlock]
debugSplitProcs b = concat $ H.mapElems $ mergeMaps $ map (split Nothing) b
  where mergeMaps = foldr (H.mapUnionWithKey (const (++))) H.mapEmpty
        split :: Maybe DebugBlock -> DebugBlock -> H.LabelMap [DebugBlock]
        split parent blk = H.mapInsert prc [blk'] nested
          where prc = dblProcedure blk
                blk' = blk { dblBlocks = own_blks
                           , dblParent = parent
                           }
                own_blks = fromMaybe [] $ H.mapLookup prc nested
                nested = mergeMaps $ map (split parent') $ dblBlocks blk
                -- Figure out who should be the parent of nested blocks.
                -- If @blk@ is optimized out then it isn't a good choice
                -- and we just use its parent.
                parent'
                  | Nothing <- dblPosition blk = parent
                  | otherwise                  = Just blk

{-
Note [Splitting DebugBlocks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DWARF requires that we break up the nested DebugBlocks produced from
the C-- AST. For instance, we begin with tick trees containing nested procs.
For example,

    proc A [tick1, tick2]
      block B [tick3]
        proc C [tick4]

when producing DWARF we need to procs (which are represented in DWARF as
TAG_subprogram DIEs) to be top-level DIEs. debugSplitProcs is responsible for
this transform, pulling out the nested procs into top-level procs.

However, in doing this we need to be careful to preserve the parentage of the
nested procs. This is the reason DebugBlocks carry the dblParent field, allowing
us to reorganize the above tree as,

    proc A [tick1, tick2]
      block B [tick3]
    proc C [tick4] parent=B

Here we have annotated the new proc C with an attribute giving its original
parent, B.
-}

-- | Generate DWARF info for a procedure debug block
procToDwarf :: NCGConfig -> DebugBlock -> DwarfInfo
procToDwarf config prc
  = DwarfSubprogram { dwChildren = map (blockToDwarf config) (dblBlocks prc)
                    , dwName     = case dblSourceTick prc of
                         Just s@SourceNote{} -> case sourceName s of
                            LexicalFastString s -> unpackFS s
                         _otherwise -> show (dblLabel prc)
                    , dwLabel    = dblCLabel prc
                    , dwParent   = fmap mkAsmTempDieLabel
                                   $ mfilter goodParent
                                   $ fmap dblCLabel (dblParent prc)
                    }
  where
  goodParent a | a == dblCLabel prc = False
               -- Omit parent if it would be self-referential
  goodParent a | not (externallyVisibleCLabel a)
               , ncgDwarfStripBlockInfo config = False
               -- If we strip block information, don't refer to blocks.
               -- Fixes #14894.
  goodParent _ = True

-- | Generate DWARF info for a block
blockToDwarf :: NCGConfig -> DebugBlock -> DwarfInfo
blockToDwarf config blk
  = DwarfBlock { dwChildren = map (blockToDwarf config) (dblBlocks blk) ++ srcNotes
               , dwLabel    = dblCLabel blk
               , dwMarker   = marker
               }
  where
    srcNotes
      | ncgDwarfSourceNotes config = concatMap tickToDwarf (dblTicks blk)
      | otherwise                  = []

    marker
      | Just _ <- dblPosition blk = Just $ mkAsmTempLabel $ dblLabel blk
      | otherwise                 = Nothing   -- block was optimized out

tickToDwarf :: CmmTickish -> [DwarfInfo]
tickToDwarf  (SourceNote ss _) = [DwarfSrcNote ss]
tickToDwarf _ = []

-- | Generates the data for the debug frame section, which encodes the
-- desired stack unwind behaviour for the debugger
debugFrame :: Platform -> Unique -> [DebugBlock] -> DwarfFrame
debugFrame p u procs
  = DwarfFrame { dwCieLabel = mkAsmTempLabel u
               , dwCieInit  = initUws
               , dwCieProcs = map (procToFrame initUws) procs
               }
  where
    initUws :: UnwindTable
    initUws = Map.fromList [(Sp, Just (UwReg (GlobalRegUse Sp $ bWord p) 0))]

-- | Generates unwind information for a procedure debug block
procToFrame :: UnwindTable -> DebugBlock -> DwarfFrameProc
procToFrame initUws blk
  = DwarfFrameProc { dwFdeProc    = dblCLabel blk
                   , dwFdeHasInfo = dblHasInfoTbl blk
                   , dwFdeBlocks  = map (uncurry blockToFrame)
                                        (setHasInfo blockUws)
                   }
  where blockUws :: [(DebugBlock, [UnwindPoint])]
        blockUws = map snd $ sortBy (comparing fst) $ flatten blk

        flatten :: DebugBlock
                -> [(Int, (DebugBlock, [UnwindPoint]))]
        flatten b@DebugBlock{ dblPosition=pos, dblUnwind=uws, dblBlocks=blocks }
          | Just p <- pos  = (p, (b, uws')):nested
          | otherwise      = nested -- block was optimized out
          where uws'   = addDefaultUnwindings initUws uws
                nested = concatMap flatten blocks

        -- If the current procedure has an info table, then we also say that
        -- its first block has one to ensure that it gets the necessary -1
        -- offset applied to its start address.
        -- See Note [Info Offset] in "GHC.CmmToAsm.Dwarf.Types".
        setHasInfo :: [(DebugBlock, [UnwindPoint])]
                   -> [(DebugBlock, [UnwindPoint])]
        setHasInfo [] = []
        setHasInfo (c0:cs) = first setIt c0 : cs
          where
            setIt child =
              child { dblHasInfoTbl = dblHasInfoTbl child
                                      || dblHasInfoTbl blk }

blockToFrame :: DebugBlock -> [UnwindPoint] -> DwarfFrameBlock
blockToFrame blk uws
  = DwarfFrameBlock { dwFdeBlkHasInfo = dblHasInfoTbl blk
                    , dwFdeUnwind     = uws
                    }

addDefaultUnwindings :: UnwindTable -> [UnwindPoint] -> [UnwindPoint]
addDefaultUnwindings tbl pts =
    [ UnwindPoint lbl (tbl' `mappend` tbl)
      -- mappend is left-biased
    | UnwindPoint lbl tbl' <- pts
    ]
