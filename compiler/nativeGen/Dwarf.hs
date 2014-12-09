module Dwarf (
  dwarfGen
  ) where

import CLabel
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
import System.FilePath
import System.Directory ( getCurrentDirectory )

import qualified Compiler.Hoopl as H

-- | Generate DWARF/debug information
dwarfGen :: DynFlags -> ModLocation -> UniqSupply -> [DebugBlock]
            -> IO (SDoc, UniqSupply)
dwarfGen df modLoc us blocks = do

  -- Convert debug data structures to DWARF info records
  let procs = debugSplitProcs blocks
  compPath <- getCurrentDirectory
  let dwarfUnit = DwarfCompileUnit
        { dwChildren = map (procToDwarf df) procs
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

  return (infoSct $$ abbrevSct $$ lineSct, us')

-- | Header for a compilation unit, establishing global format
-- parameters
compileUnitHeader :: Unique -> SDoc
compileUnitHeader unitU = sdocWithPlatform $ \plat ->
  let cuLabel = mkAsmTempLabel unitU
      length = ppr (mkAsmTempEndLabel cuLabel) <> char '-' <> ppr cuLabel
  in vcat [ ptext (sLit "\t.long ") <> length  -- compilation unit size
          , ppr cuLabel <> colon
          , ptext (sLit "\t.word 3")           -- DWARF version
          , pprDwWord (ptext dwarfAbbrevLabel <> char '-' <>
                       ptext dwarfAbbrevLabel) -- pointer to our abbrevs
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
