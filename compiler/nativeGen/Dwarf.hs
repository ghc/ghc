module Dwarf (
  dwarfGen
  ) where

import CLabel
import CmmExpr         ( GlobalReg(..) )
import Config          ( cProjectName, cProjectVersion )
import CoreSyn         ( Tickish(..) )
import Debug
import DynFlags
import FastString
import Module
import Outputable
import Platform
import Unique
import UniqSupply

import Dwarf.Constants
import Dwarf.Types

import Data.Maybe
import Data.List        ( sortBy )
import Data.Ord         ( comparing )
import qualified Data.Map as Map
import System.FilePath
import System.Directory ( getCurrentDirectory )

import qualified Compiler.Hoopl as H

-- | Generate DWARF/debug information
dwarfGen :: DynFlags -> ModLocation -> UniqSupply -> [DebugBlock]
            -> IO (SDoc, UniqSupply)
dwarfGen df modLoc us blocks = do

  -- Convert debug data structures to DWARF info records
  -- We strip out block information, as it is not currently useful for
  -- anything. In future we might want to only do this for -g1.
  let procs = debugSplitProcs blocks
      stripBlocks dbg = dbg { dblBlocks = [] }
  compPath <- getCurrentDirectory
  let dwarfUnit = DwarfCompileUnit
        { dwChildren = map (procToDwarf df) (map stripBlocks procs)
        , dwName = fromMaybe "" (ml_hs_file modLoc)
        , dwCompDir = addTrailingPathSeparator compPath
        , dwProducer = cProjectName ++ " " ++ cProjectVersion
        , dwLineLabel = dwarfLineLabel
        }

  -- Check whether we have any source code information, so we do not
  -- end up writing a pointer to an empty .debug_line section
  -- (dsymutil on Mac Os gets confused by this).
  let haveSrcIn blk = isJust (dblSourceTick blk) && isJust (dblPosition blk)
                      || any haveSrcIn (dblBlocks blk)
      haveSrc = any haveSrcIn procs

  -- .debug_abbrev section: Declare the format we're using
  let abbrevSct = pprAbbrevDecls haveSrc

  -- .debug_info section: Information records on procedures and blocks
  let (unitU, us') = takeUniqFromSupply us
      infoSct = vcat [ dwarfInfoSection
                     , compileUnitHeader unitU
                     , pprDwarfInfo haveSrc dwarfUnit
                     , compileUnitFooter unitU
                     ]

  -- .debug_line section: Generated mainly by the assembler, but we
  -- need to label it
  let lineSct = dwarfLineSection $$
                ptext dwarfLineLabel <> colon

  -- .debug_frame section: Information about the layout of the GHC stack
  let (framesU, us'') = takeUniqFromSupply us'
      frameSct = dwarfFrameSection $$
                 ptext dwarfFrameLabel <> colon $$
                 pprDwarfFrame (debugFrame framesU procs)

  return (infoSct $$ abbrevSct $$ lineSct $$ frameSct, us'')

-- | Header for a compilation unit, establishing global format
-- parameters
compileUnitHeader :: Unique -> SDoc
compileUnitHeader unitU = sdocWithPlatform $ \plat ->
  let cuLabel = mkAsmTempLabel unitU
      length = ppr (mkAsmTempEndLabel cuLabel) <> char '-' <> ppr cuLabel
  in vcat [ ptext (sLit "\t.long ") <> length  -- compilation unit size
          , ppr cuLabel <> colon
          , ptext (sLit "\t.word 3")           -- DWARF version
          , sectionOffset dwarfAbbrevLabel dwarfAbbrevLabel
                                               -- abbrevs offset
          , ptext (sLit "\t.byte ") <> ppr (platformWordSize plat) -- word size
          ]

-- | Compilation unit footer, mainly establishing size of debug sections
compileUnitFooter :: Unique -> SDoc
compileUnitFooter unitU =
  let cuEndLabel = mkAsmTempEndLabel $ mkAsmTempLabel unitU
  in ppr cuEndLabel <> colon

-- | Splits the blocks by procedures. In the result all nested blocks
-- will come from the same procedure as the top-level block.
debugSplitProcs :: [DebugBlock] -> [DebugBlock]
debugSplitProcs b = concat $ H.mapElems $ mergeMaps $ map split b
  where mergeMaps = foldr (H.mapUnionWithKey (const (++))) H.mapEmpty
        split :: DebugBlock -> H.LabelMap [DebugBlock]
        split blk = H.mapInsert prc [blk {dblBlocks = own_blks}] nested
          where prc = dblProcedure blk
                own_blks = fromMaybe [] $ H.mapLookup prc nested
                nested = mergeMaps $ map split $ dblBlocks blk
        -- Note that we are rebuilding the tree here, so tick scopes
        -- might change. We could fix that - but we actually only care
        -- about dblSourceTick in the result, so this is okay.

-- | Generate DWARF info for a procedure debug block
procToDwarf :: DynFlags -> DebugBlock -> DwarfInfo
procToDwarf df prc
  = DwarfSubprogram { dwChildren = foldr blockToDwarf [] $ dblBlocks prc
                    , dwName     = case dblSourceTick prc of
                         Just s@SourceNote{} -> sourceName s
                         _otherwise -> showSDocDump df $ ppr $ dblLabel prc
                    , dwLabel    = dblCLabel prc
                    }

-- | Generate DWARF info for a block
blockToDwarf :: DebugBlock -> [DwarfInfo] -> [DwarfInfo]
blockToDwarf blk dws
  | isJust (dblPosition blk) = dw : dws
  | otherwise                = nested ++ dws -- block was optimized out, flatten
  where nested = foldr blockToDwarf [] $ dblBlocks blk
        dw = DwarfBlock { dwChildren = nested
                        , dwLabel    = dblCLabel blk
                        , dwMarker   = mkAsmTempLabel (dblLabel blk)
                        }

-- | Generates the data for the debug frame section, which encodes the
-- desired stack unwind behaviour for the debugger
debugFrame :: Unique -> [DebugBlock] -> DwarfFrame
debugFrame u procs
  = DwarfFrame { dwCieLabel = mkAsmTempLabel u
               , dwCieInit  = initUws
               , dwCieProcs = map (procToFrame initUws) procs
               }
  where initUws = Map.fromList [(Sp, UwReg Sp 0)]

-- | Generates unwind information for a procedure debug block
procToFrame :: UnwindTable -> DebugBlock -> DwarfFrameProc
procToFrame initUws blk
  = DwarfFrameProc { dwFdeProc    = dblCLabel blk
                   , dwFdeHasInfo = dblHasInfoTbl blk
                   , dwFdeBlocks  = map (uncurry blockToFrame) blockUws
                   }
  where blockUws :: [(DebugBlock, UnwindTable)]
        blockUws = map snd $ sortBy (comparing fst) $ flatten initUws blk
        flatten uws0 b@DebugBlock{ dblPosition=pos, dblUnwind=uws,
                                   dblBlocks=blocks }
          | Just p <- pos  = (p, (b, uws')):nested
          | otherwise      = nested -- block was optimized out
          where uws'   = uws `Map.union` uws0
                nested = concatMap (flatten uws') blocks

blockToFrame :: DebugBlock -> UnwindTable -> DwarfFrameBlock
blockToFrame blk uws
  = DwarfFrameBlock { dwFdeBlock      = mkAsmTempLabel $ dblLabel blk
                    , dwFdeBlkHasInfo = dblHasInfoTbl blk
                    , dwFdeUnwind     = uws
                    }
