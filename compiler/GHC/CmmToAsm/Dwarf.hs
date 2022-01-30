module GHC.CmmToAsm.Dwarf (
  dwarfGen
  ) where

import GHC.Prelude

import GHC.Cmm.CLabel
import GHC.Cmm.Expr        ( GlobalReg(..) )
import GHC.Settings.Config ( cProjectName, cProjectVersion )
import GHC.Types.Tickish   ( CmmTickish, GenTickish(..) )
import GHC.Cmm.DebugBlock
import GHC.Unit.Module
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique
import GHC.Types.Unique.Supply

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
import System.Directory ( getCurrentDirectory )

import qualified GHC.Cmm.Dataflow.Label as H
import qualified GHC.Cmm.Dataflow.Collections as H

-- | Generate DWARF/debug information
dwarfGen :: NCGConfig -> ModLocation -> UniqSupply -> [DebugBlock]
            -> IO (SDoc, UniqSupply)
dwarfGen _      _      us []     = return (empty, us)
dwarfGen config modLoc us blocks = do
  let platform = ncgPlatform config

  -- Convert debug data structures to DWARF info records
  let procs = debugSplitProcs blocks
      stripBlocks dbg
        | ncgDwarfStripBlockInfo config = dbg { dblBlocks = [] }
        | otherwise                     = dbg
  compPath <- getCurrentDirectory
  let lowLabel = dblCLabel $ head procs
      highLabel = mkAsmTempProcEndLabel $ dblCLabel $ last procs
      dwarfUnit = DwarfCompileUnit
        { dwChildren = map (procToDwarf config) (map stripBlocks procs)
        , dwName = fromMaybe "" (ml_hs_file modLoc)
        , dwCompDir = addTrailingPathSeparator compPath
        , dwProducer = cProjectName ++ " " ++ cProjectVersion
        , dwLowLabel = pdoc platform lowLabel
        , dwHighLabel = pdoc platform highLabel
        , dwLineLabel = dwarfLineLabel
        }

  -- Check whether we have any source code information, so we do not
  -- end up writing a pointer to an empty .debug_line section
  -- (dsymutil on Mac Os gets confused by this).
  let haveSrcIn blk = isJust (dblSourceTick blk) && isJust (dblPosition blk)
                      || any haveSrcIn (dblBlocks blk)
      haveSrc = any haveSrcIn procs

  -- .debug_abbrev section: Declare the format we're using
  let abbrevSct = pprAbbrevDecls platform haveSrc

  -- .debug_info section: Information records on procedures and blocks
  let -- unique to identify start and end compilation unit .debug_inf
      (unitU, us') = takeUniqFromSupply us
      infoSct = vcat [ dwarfInfoLabel <> colon
                     , dwarfInfoSection platform
                     , compileUnitHeader platform unitU
                     , pprDwarfInfo platform haveSrc dwarfUnit
                     , compileUnitFooter platform unitU
                     ]

  -- .debug_line section: Generated mainly by the assembler, but we
  -- need to label it
  let lineSct = dwarfLineSection platform $$
                dwarfLineLabel <> colon

  -- .debug_frame section: Information about the layout of the GHC stack
  let (framesU, us'') = takeUniqFromSupply us'
      frameSct = dwarfFrameSection platform $$
                 dwarfFrameLabel <> colon $$
                 pprDwarfFrame platform (debugFrame framesU procs)

  -- .aranges section: Information about the bounds of compilation units
  let aranges' | ncgSplitSections config = map mkDwarfARange procs
               | otherwise               = [DwarfARange lowLabel highLabel]
  let aranges = dwarfARangesSection platform $$ pprDwarfARanges platform aranges' unitU

  return (infoSct $$ abbrevSct $$ lineSct $$ frameSct $$ aranges, us'')

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
compileUnitHeader :: Platform -> Unique -> SDoc
compileUnitHeader platform unitU =
  let cuLabel = mkAsmTempLabel unitU  -- sits right before initialLength field
      length = pdoc platform (mkAsmTempEndLabel cuLabel) <> char '-' <> pdoc platform cuLabel
               <> text "-4"       -- length of initialLength field
  in vcat [ pdoc platform cuLabel <> colon
          , text "\t.long " <> length  -- compilation unit size
          , pprHalf 3                          -- DWARF version
          , sectionOffset platform dwarfAbbrevLabel dwarfAbbrevLabel
                                               -- abbrevs offset
          , text "\t.byte " <> ppr (platformWordSizeInBytes platform) -- word size
          ]

-- | Compilation unit footer, mainly establishing size of debug sections
compileUnitFooter :: Platform -> Unique -> SDoc
compileUnitFooter platform unitU =
  let cuEndLabel = mkAsmTempEndLabel $ mkAsmTempLabel unitU
  in pdoc platform cuEndLabel <> colon

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
                         Just s@SourceNote{} -> sourceName s
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
debugFrame :: Unique -> [DebugBlock] -> DwarfFrame
debugFrame u procs
  = DwarfFrame { dwCieLabel = mkAsmTempLabel u
               , dwCieInit  = initUws
               , dwCieProcs = map (procToFrame initUws) procs
               }
  where
    initUws :: UnwindTable
    initUws = Map.fromList [(Sp, Just (UwReg Sp 0))]

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

        -- | If the current procedure has an info table, then we also say that
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
