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

module LlvmMangler ( llvmFixupAsm, ManglerInfo, ManglerStr ) where

import DynFlags ( DynFlags, targetPlatform )
import Platform ( platformArch, Arch(..), platformOS, OS(..) )
import ErrUtils ( withTiming )
import Outputable ( text, panic )

import Control.Exception
import qualified Data.ByteString.Char8 as B
import System.IO
import Hoopl.Label
import Hoopl.Collections
import Hoopl.Unique ( intToUnique )
import Data.Maybe ( fromMaybe )

-- note [mangler string func]
-- A ManglerStr takes the name of the label it will be attached to,
-- and returns the data that should appear before that label.
-- We do this because some of the static data is of the form 
--
--      .quad  SRT_LAB-CUR_LABEL
--
-- and we do not have a name for CUR_LABEL until LLVM generates
-- the assembly code for the function. The name for SRT_LAB 
-- is only in the LLVM monad, so we need two stages to generate
-- that line

type ManglerStr = [B.ByteString -> B.ByteString]
type ManglerInfo = Maybe (LabelMap ManglerStr)

-- to manage the simple state machine for adjacent labels
data State
    = Default
    | FirstLabel
    | OtherLabel

-- | Read in assembly file and process
llvmFixupAsm :: DynFlags -> LabelMap ManglerStr -> FilePath -> FilePath -> IO ()
llvmFixupAsm dflags gcInfo f1 f2 = {-# SCC "llvm_mangler" #-}
    withTiming (pure dflags) (text "LLVM Mangler") id $
    withBinaryFile f1 ReadMode $ \r -> withBinaryFile f2 WriteMode $ \w -> do
        go r w Default
        hClose r
        hClose w
        return ()
  where
    doRewrite = rewriteLine dflags (labRewrites gcInfo) rewrites
    
    go :: Handle -> Handle -> State -> IO ()
    go r w s = do
      e_l <- try $ B.hGetLine r ::IO (Either IOError B.ByteString)
      let writeline a s = let (newL, newS) = doRewrite a s in
                            B.hPutStrLn w newL >> go r w newS
      case e_l of
        Right l -> writeline l s
        Left _  -> return ()

-- | These are the non-label rewrites that the mangler will perform
rewrites :: [Rewrite]
rewrites = [rewriteSymType, rewriteAVX]

-- | These are the label-based rewrites that the mangler will perform
labRewrites :: LabelMap ManglerStr -> [LabRewrite]
labRewrites info = [addInfoTable info]

type Rewrite = DynFlags -> B.ByteString -> Maybe B.ByteString
type LabRewrite = State -> Rewrite

-- | This rewrite looks for return points of a llvm.cpscall and adds GC info
-- above that label.
addInfoTable :: LabelMap ManglerStr -> LabRewrite
addInfoTable info FirstLabel dflags line = do
        retPt <- stripPrefix labPrefix line
        (i, _) <- B.readInt retPt
        statics <- mapLookup (toKey i) info
        fullName <- stripSuffix colon line
        return $ B.concat $ (map (\f -> f fullName) statics) ++ [line]
    where
        
        -- At a minimum, Mac and Linux assembly output from LLVM use different prefixes
        -- basic block labels. It may be the case that one of the prefixes is very common,
        -- but I have not looked into it (kavon)
        labPrefix = case platformOS (targetPlatform dflags) of
                        OSDarwin -> B.pack "L" 
                        OSLinux  -> B.pack ".L"
                        otherwise -> panic "Please update LLVM Mangler for this OS."
                        
        colon = B.pack ":"
        toKey = uniqueToLbl . intToUnique
        
        -- TODO(kavon): on Travis CI, it seems the bytestring package is out of date, and
        -- we're missing B.stripSuffix and B.stripPrefix. I've reimplemented them here.
        -- please remove these when that issue is resolved.
        stripPrefix pfx line 
            | B.isPrefixOf pfx line 
                = Just $ B.drop (B.length pfx) line
            | otherwise = Nothing
            
        stripSuffix sfx line
            | B.isSuffixOf sfx line
                = Just $ B.take ((B.length line) - (B.length sfx)) line
            | otherwise = Nothing
        
addInfoTable _ _ _ _ = Nothing
        
-- | Rewrite a line of assembly source with the given rewrites,
-- taking the first rewrite that applies for each kind of rewrite (label and non-label).
rewriteLine :: DynFlags -> [LabRewrite] -> [Rewrite] 
                        -> B.ByteString -> State -> (B.ByteString, State)
rewriteLine dflags labRewrites rewrites l state = withState $
    case (maybNewSym, maybNewRest) of
        (Nothing, Nothing) -> l -- avoid concat
        (newS, newR)       -> cat (fromMaybe symbol newS) (fromMaybe rest newR)
  where
    
    -- the transition function of the state machine
    withState l = (l, curState)
    curState = case (isOnlyLabel split, state) of 
                    (True, Default)    -> FirstLabel
                    (True, FirstLabel) -> OtherLabel
                    (False, _)         -> Default
                    _                  -> state
      
    cat sym rst = B.concat $ [sym, B.pack "\t", rst]
    
    addState state rws = map (\rw -> rw state) rws
    findRwOf txt rws = firstJust $ map (\rw -> rw dflags txt) rws
    
    (split @ (symbol, rest)) = splitLine l
    -- check for new label part
    maybNewSym = findRwOf symbol $ addState curState labRewrites
    -- check for new non-label part
    maybNewRest = findRwOf rest rewrites

    firstJust :: [Maybe a] -> Maybe a
    firstJust (jx@(Just _):_) = jx
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
    
isOnlyLabel :: (B.ByteString, B.ByteString) -> Bool
isOnlyLabel (symbol, rest) = 
    (B.null rest || B.head rest == '#') && not (B.null symbol)
