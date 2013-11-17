-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
--
-- This script processes the assembly produced by LLVM, rearranging the code
-- so that an info table appears before its corresponding function.
--

module LlvmMangler ( llvmFixupAsm ) where

import DynFlags ( DynFlags )
import ErrUtils ( showPass )
import LlvmCodeGen.Ppr ( infoSection )

import Control.Exception
import Control.Monad ( when )
import qualified Data.ByteString.Char8 as B
import Data.Char
import System.IO

import Data.List ( sortBy )
import Data.Function ( on )

#if x86_64_TARGET_ARCH
#define REWRITE_AVX
#endif

-- Magic Strings
secStmt, infoSec, newLine, textStmt, dataStmt, syntaxUnified :: B.ByteString
secStmt       = B.pack "\t.section\t"
infoSec       = B.pack infoSection
newLine       = B.pack "\n"
textStmt      = B.pack "\t.text"
dataStmt      = B.pack "\t.data"
syntaxUnified = B.pack "\t.syntax unified"

infoLen :: Int
infoLen = B.length infoSec

-- Search Predicates
isType :: B.ByteString -> Bool
isType = B.isPrefixOf (B.pack "\t.type")

-- section of a file in the form of (header line, contents)
type Section = (B.ByteString, B.ByteString)

-- | Read in assembly file and process
llvmFixupAsm :: DynFlags -> FilePath -> FilePath -> IO ()
llvmFixupAsm dflags f1 f2 = {-# SCC "llvm_mangler" #-} do
    showPass dflags "LLVM Mangler"
    r <- openBinaryFile f1 ReadMode
    w <- openBinaryFile f2 WriteMode
    ss <- readSections r w
    hClose r
    let fixed = (map rewriteAVX . fixTables) ss
    mapM_ (writeSection w) fixed
    hClose w
    return ()

-- | Splits the file contents into its sections
readSections :: Handle -> Handle -> IO [Section]
readSections r w = go B.empty [] []
  where
    go hdr ss ls = do
      e_l <- (try (B.hGetLine r))::IO (Either IOError B.ByteString)

      -- Note that ".type" directives at the end of a section refer to
      -- the first directive of the *next* section, therefore we take
      -- it over to that section.
      let (tys, ls') = span isType ls
          cts = B.intercalate newLine $ reverse ls'

      -- Decide whether to directly output the section or append it
      -- to the list for resorting.
      let finishSection
            | infoSec `B.isInfixOf` hdr =
                cts `seq` return $ (hdr, cts):ss
            | otherwise =
                writeSection w (hdr, cts) >> return ss

      case e_l of
        Right l | l == syntaxUnified 
                  -> finishSection >>= \ss' -> writeSection w (l, B.empty)
                                   >> go B.empty ss' tys
                | any (`B.isPrefixOf` l) [secStmt, textStmt, dataStmt]
                  -> finishSection >>= \ss' -> go l ss' tys
                | otherwise
                  -> go hdr ss (l:ls)
        Left _    -> finishSection >>= \ss' -> return (reverse ss')

-- | Writes sections back
writeSection :: Handle -> Section -> IO ()
writeSection w (hdr, cts) = do
  when (not $ B.null hdr) $
    B.hPutStrLn w hdr
  B.hPutStrLn w cts

#if REWRITE_AVX
rewriteAVX :: Section -> Section
rewriteAVX = rewriteVmovaps . rewriteVmovdqa

rewriteVmovdqa :: Section -> Section
rewriteVmovdqa = rewriteInstructions vmovdqa vmovdqu
  where
    vmovdqa, vmovdqu :: B.ByteString
    vmovdqa = B.pack "vmovdqa"
    vmovdqu = B.pack "vmovdqu"

rewriteVmovap :: Section -> Section
rewriteVmovap = rewriteInstructions vmovap vmovup
  where
    vmovap, vmovup :: B.ByteString
    vmovap = B.pack "vmovap"
    vmovup = B.pack "vmovup"

rewriteInstructions :: B.ByteString -> B.ByteString -> Section -> Section
rewriteInstructions matchBS replaceBS (hdr, cts) =
    (hdr, loop cts)
  where
    loop :: B.ByteString -> B.ByteString
    loop cts =
        case B.breakSubstring cts matchBS of
          (hd,tl) | B.null tl -> hd
                  | otherwise -> hd `B.append` replaceBS `B.append`
                                 loop (B.drop (B.length matchBS) tl)
#else /* !REWRITE_AVX */
rewriteAVX :: Section -> Section
rewriteAVX = id
#endif /* !REWRITE_SSE */

-- | Reorder and convert sections so info tables end up next to the
-- code. Also does stack fixups.
fixTables :: [Section] -> [Section]
fixTables ss = map strip sorted
  where
    -- Resort sections: We only assign a non-zero number to all
    -- sections having the "STRIP ME" marker. As sortBy is stable,
    -- this will cause all these sections to be appended to the end of
    -- the file in the order given by the indexes.
    extractIx hdr
      | B.null a  = 0
      | otherwise = 1 + readInt (B.takeWhile isDigit $ B.drop infoLen a)
      where (_,a) = B.breakSubstring infoSec hdr

    indexed = zip (map (extractIx . fst) ss) ss

    sorted = map snd $ sortBy (compare `on` fst) indexed

    -- Turn all the "STRIP ME" sections into normal text sections, as
    -- they are in the right place now.
    strip (hdr, cts)
      | infoSec `B.isInfixOf` hdr = (textStmt, cts)
      | otherwise                 = (hdr, cts)

-- | Read an int or error
readInt :: B.ByteString -> Int
readInt str | B.all isDigit str = (read . B.unpack) str
            | otherwise = error $ "LLvmMangler Cannot read " ++ show str
                                ++ " as it's not an Int"

