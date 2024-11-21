{-
(c) Galois, 2006
(c) University of Glasgow, 2007
-}

module GHC.HsToCore.Coverage
  ( writeMixEntries
  , hpcInitCode
  ) where

import GHC.Prelude as Prelude

import GHC.Unit

import GHC.HsToCore.Ticks

import GHC.Platform

import GHC.Data.FastString
import GHC.Data.SizedSeq

import GHC.Cmm.CLabel

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Types.ForeignStubs
import GHC.Types.HpcInfo
import GHC.Types.SrcLoc

import Control.Monad
import Data.Time
import System.Directory

import Trace.Hpc.Mix
import Trace.Hpc.Util

import qualified Data.ByteString as BS

writeMixEntries
  :: FilePath -> Module -> SizedSeq Tick -> FilePath -> IO Int
writeMixEntries hpc_dir mod extendedMixEntries filename
  = do
        let count = fromIntegral $ sizeSS extendedMixEntries
            entries = ssElts extendedMixEntries

            mod_name = moduleNameString (moduleName mod)

            hpc_mod_dir
              | moduleUnit mod == mainUnit  = hpc_dir
              | otherwise = hpc_dir ++ "/" ++ unitString (moduleUnit mod)

            tabStop = 8 -- <tab> counts as a normal char in GHC's
                        -- location ranges.

        createDirectoryIfMissing True hpc_mod_dir
        modTime <- getModificationUTCTime filename
        let entries' = [ (hpcPos, tick_label t)
                       | t <- entries, hpcPos <- [mkHpcPos $ tick_loc t] ]
        when (entries' `lengthIsNot` count) $
          panic "the number of .mix entries are inconsistent"
        let hashNo = mixHash filename modTime tabStop entries'
        mixCreate hpc_mod_dir mod_name
                       $ Mix filename modTime (toHash hashNo) tabStop entries'
        return hashNo

mkHpcPos :: SrcSpan -> HpcPos
mkHpcPos pos@(RealSrcSpan s _)
   | isGoodSrcSpan' pos = toHpcPos (srcSpanStartLine s,
                                    srcSpanStartCol s,
                                    srcSpanEndLine s,
                                    srcSpanEndCol s - 1)
                              -- the end column of a SrcSpan is one
                              -- greater than the last column of the
                              -- span (see SrcLoc), whereas HPC
                              -- expects to the column range to be
                              -- inclusive, hence we subtract one above.
mkHpcPos _ = panic "bad source span; expected such spans to be filtered out"

-- For the hash value, we hash everything: the file name,
--  the timestamp of the original source file, the tab stop,
--  and the mix entries. We cheat, and hash the show'd string.
-- This hash only has to be hashed at Mix creation time,
-- and is for sanity checking only.
mixHash :: FilePath -> UTCTime -> Int -> [MixEntry] -> Int
mixHash file tm tabstop entries = fromIntegral $ hashString
        (show $ Mix file tm 0 tabstop entries)

{-
************************************************************************
*                                                                      *
*              initialisation
*                                                                      *
************************************************************************
-}

{- | Create HPC initialization C code for a module

Each module compiled with -fhpc declares an initialisation function of
the form `hpc_init_<module>()`, which is emitted into the _stub.c file
and annotated with __attribute__((constructor)) so that it gets
executed at startup time.

The function's purpose is to call hs_hpc_module to register this
module with the RTS, and it looks something like this:

> static void hpc_init_Main(void) __attribute__((constructor));
> static void hpc_init_Main(void)
> {
>   extern StgWord64 _hpc_tickboxes_Main_hpc[];
>   hs_hpc_module("Main",8,1150288664,_hpc_tickboxes_Main_hpc);
> }
-}
hpcInitCode :: Platform -> Module -> HpcInfo -> CStub
hpcInitCode _ _ (NoHpcInfo {}) = mempty
hpcInitCode platform this_mod (HpcInfo tickCount hashNo)
 = initializerCStub platform fn_name decls body
  where
    fn_name = mkInitializerStubLabel this_mod (fsLit "hpc")
    decls = text "StgWord64 " <> tickboxes <> brackets (int tickCount) <> semi
    body = text "hs_hpc_module" <>
              parens (hcat (punctuate comma [
                  doubleQuotes full_name_str,
                  int tickCount, -- really StgWord32
                  int hashNo,    -- really StgWord32
                  tickboxes
                ])) <> semi

    tickboxes = pprCLabel platform (mkHpcTicksLabel $ this_mod)

    module_name  = hcat (map (text.charToC) $ BS.unpack $
                         bytesFS (moduleNameFS (moduleName this_mod)))
    package_name = hcat (map (text.charToC) $ BS.unpack $
                         bytesFS (unitFS  (moduleUnit this_mod)))
    full_name_str
       | moduleUnit this_mod == mainUnit
       = module_name
       | otherwise
       = package_name <> char '/' <> module_name
