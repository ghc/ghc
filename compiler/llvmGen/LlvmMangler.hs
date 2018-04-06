-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
--
-- This script processes the assembly produced by LLVM, rewriting all symbols
-- of type @function to @object. This keeps them from going through the PLT,
-- which would be bad due to tables-next-to-code. On x86_64,
-- it also rewrites AVX instructions that require alignment to their
-- unaligned counterparts, since the stack is only 16-byte aligned but these
-- instructions require 32-byte alignment.
--

module LlvmMangler ( llvmFixupAsm ) where

import GhcPrelude

import DynFlags ( DynFlags, targetPlatform )
import Platform ( platformArch, Arch(..) )
import ErrUtils ( withTiming )
import Outputable ( text )

import Control.Exception
import qualified Data.ByteString.Char8 as B
import System.IO

-- | Read in assembly file and process
llvmFixupAsm :: DynFlags -> FilePath -> FilePath -> IO ()
llvmFixupAsm dflags f1 f2 = {-# SCC "llvm_mangler" #-}
    withTiming (pure dflags) (text "LLVM Mangler") id $
    withBinaryFile f1 ReadMode $ \r -> withBinaryFile f2 WriteMode $ \w -> do
        go r w
        hClose r
        hClose w
        return ()
  where
    go :: Handle -> Handle -> IO ()
    go r w = do
      e_l <- try $ B.hGetLine r ::IO (Either IOError B.ByteString)
      let writeline a = B.hPutStrLn w (rewriteLine dflags rewrites a) >> go r w
      case e_l of
        Right l -> writeline l
        Left _  -> return ()

-- | These are the rewrites that the mangler will perform
rewrites :: [Rewrite]
rewrites = [rewriteSymType, rewriteAVX]

type Rewrite = DynFlags -> B.ByteString -> Maybe B.ByteString

-- | Rewrite a line of assembly source with the given rewrites,
-- taking the first rewrite that applies.
rewriteLine :: DynFlags -> [Rewrite] -> B.ByteString -> B.ByteString
rewriteLine dflags rewrites l
  -- We disable .subsections_via_symbols on darwin and ios, as the llvm code
  -- gen uses prefix data for the info table.  This however does not prevent
  -- llvm from generating .subsections_via_symbols, which in turn with
  -- -dead_strip, strips the info tables, and therefore breaks ghc.
  | isSubsectionsViaSymbols l =
    (B.pack "## no .subsection_via_symbols for ghc. We need our info tables!")
  | otherwise =
    case firstJust $ map (\rewrite -> rewrite dflags rest) rewrites of
      Nothing        -> l
      Just rewritten -> B.concat $ [symbol, B.pack "\t", rewritten]
  where
    isSubsectionsViaSymbols = B.isPrefixOf (B.pack ".subsections_via_symbols")

    (symbol, rest) = splitLine l

    firstJust :: [Maybe a] -> Maybe a
    firstJust (Just x:_) = Just x
    firstJust []         = Nothing
    firstJust (_:rest)   = firstJust rest

-- | This rewrites @.type@ annotations of function symbols to @%object@.
-- This is done as the linker can relocate @%functions@ through the
-- Procedure Linking Table (PLT). This is bad since we expect that the
-- info table will appear directly before the symbol's location. In the
-- case that the PLT is used, this will be not an info table but instead
-- some random PLT garbage.
rewriteSymType :: Rewrite
rewriteSymType _ l
  | isType l  = Just $ rewrite '@' $ rewrite '%' l
  | otherwise = Nothing
  where
    isType = B.isPrefixOf (B.pack ".type")

    rewrite :: Char -> B.ByteString -> B.ByteString
    rewrite prefix = replaceOnce funcType objType
      where
        funcType = prefix `B.cons` B.pack "function"
        objType  = prefix `B.cons` B.pack "object"

-- | This rewrites aligned AVX instructions to their unaligned counterparts on
-- x86-64. This is necessary because the stack is not adequately aligned for
-- aligned AVX spills, so LLVM would emit code that adjusts the stack pointer
-- and disable tail call optimization. Both would be catastrophic here so GHC
-- tells LLVM that the stack is 32-byte aligned (even though it isn't) and then
-- rewrites the instructions in the mangler.
rewriteAVX :: Rewrite
rewriteAVX dflags s
  | not isX86_64 = Nothing
  | isVmovdqa s  = Just $ replaceOnce (B.pack "vmovdqa") (B.pack "vmovdqu") s
  | isVmovap s   = Just $ replaceOnce (B.pack "vmovap") (B.pack "vmovup") s
  | otherwise    = Nothing
  where
    isX86_64 = platformArch (targetPlatform dflags) == ArchX86_64
    isVmovdqa = B.isPrefixOf (B.pack "vmovdqa")
    isVmovap = B.isPrefixOf (B.pack "vmovap")

-- | @replaceOnce match replace bs@ replaces the first occurrence of the
-- substring @match@ in @bs@ with @replace@.
replaceOnce :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceOnce matchBS replaceOnceBS = loop
  where
    loop :: B.ByteString -> B.ByteString
    loop cts =
        case B.breakSubstring matchBS cts of
          (hd,tl) | B.null tl -> hd
                  | otherwise -> hd `B.append` replaceOnceBS `B.append`
                                 B.drop (B.length matchBS) tl

-- | This function splits a line of assembly code into the label and the
-- rest of the code.
splitLine :: B.ByteString -> (B.ByteString, B.ByteString)
splitLine l = (symbol, B.dropWhile isSpace rest)
  where
    isSpace ' ' = True
    isSpace '\t' = True
    isSpace _ = False
    (symbol, rest) = B.span (not . isSpace) l
