{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | This module provides support for CPP, interpreter directives and line
-- pragmas.
module Preprocess
   (
     stripLinePragmas
   , getCppTokensAsComments
   , getPreprocessedSrcDirect
   , readFileGhc

   , CppOptions(..)
   , defaultCppOptions
   ) where

import qualified GHC            as GHC hiding (parseModule)

import qualified Control.Monad.IO.Class as GHC
import qualified GHC.Data.FastString   as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.DynFlags   as GHC
import qualified GHC.Driver.Env        as GHC
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Phases     as GHC
import qualified GHC.Driver.Pipeline   as GHC
import qualified GHC.Parser.Lexer      as GHC
import qualified GHC.Parser.PreProcess.State as GHC
import qualified GHC.Settings          as GHC
import qualified GHC.Types.Error       as GHC
import qualified GHC.Types.SourceError as GHC
import qualified GHC.Types.SourceFile  as GHC
import qualified GHC.Types.SrcLoc      as GHC
import qualified GHC.Utils.Error       as GHC
import qualified GHC.Utils.Fingerprint as GHC
import qualified GHC.Utils.Outputable  as GHC
import qualified GHC.Utils.Panic.Plain as GHC
import GHC.Types.SrcLoc (mkSrcSpan, mkSrcLoc)
import GHC.Data.FastString (mkFastString)

import Data.List (isPrefixOf, partition)
import Data.Maybe
import Types
import Utils
import qualified Data.Set as Set
import qualified GHC.Data.Strict as Strict


import Debug.Trace
import qualified GHC.LanguageExtensions as LangExt
--

-- ---------------------------------------------------------------------

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }

defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

-- ---------------------------------------------------------------------
-- | Remove GHC style line pragmas (@{-# LINE .. #-}@) and convert them into comments.
stripLinePragmas :: String -> (String, [GHC.LEpaComment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe GHC.LEpaComment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe GHC.LEpaComment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
           mSrcLoc = mkSrcLoc (mkFastString "LINE")
           ss     = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (size+1))
       in (res, Just $ mkLEpaComment pragma (GHC.spanAsAnchor ss) (GHC.realSrcSpan ss))
  -- Deal with shebang/cpp directives too
  |  "#!" `isPrefixOf` s =
    let mSrcLoc = mkSrcLoc (mkFastString "SHEBANG")
        ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (length s))
    in
    ("",Just $ mkLEpaComment s (GHC.spanAsAnchor ss) (GHC.realSrcSpan ss))
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)

-- ---------------------------------------------------------------------

-- | Replacement for original 'getRichTokenStream' which will return
-- the tokens for a file processed by CPP.
-- See bug <http://ghc.haskell.org/trac/ghc/ticket/8265>
getCppTokensAsComments :: GHC.GhcMonad m
                       => CppOptions  -- ^ Preprocessor Options
                       -> FilePath    -- ^ Path to source file
                       -> m [GHC.LEpaComment]
getCppTokensAsComments cppOptions sourceFile = do
  source <- GHC.liftIO $ GHC.hGetStringBuffer sourceFile
  let startLoc = GHC.mkRealSrcLoc (GHC.mkFastString sourceFile) 1 1
  (_txt,strSrcBuf,flags2') <- getPreprocessedSrcDirectPrim cppOptions sourceFile

  let flags2g  = GHC.xopt_set flags2' LangExt.GhcCpp
  let flags2 = GHC.initParserOpts flags2'
  let flags2'' = GHC.initParserOpts flags2g
  -- let flags2'' = flags2 { GHC.pExtsBitmap = GHC.xset GHC.GhcCppBit (GHC.pExtsBitmap flags2)}
  -- hash-ifdef tokens
  -- directiveToks <- GHC.liftIO $ getPreprocessorAsComments sourceFile
  -- Tokens without hash-ifdef
  nonDirectiveToks <- tokeniseOriginalSrc startLoc flags2 source
  case GHC.lexTokenStream () (GHC.enableGhcCpp flags2) source startLoc of
        GHC.POk _ ts ->
               do
                  let
                      isCppTok (GHC.L _ (GHC.ITcpp _ _ _)) = True
                      isCppTok _                           = False
                      toks = GHC.addSourceToTokens startLoc source
                                (trace ("bitmap:" ++ show (GHC.pExtsBitmap flags2)) ts)
                      (directiveToks, toks') = partition (\(t,_) -> isCppTok t) toks
                      -- (directiveToks, toks') = partition (\(t,_) -> isCppTok t)
                      --                 (trace ("toks:" ++ show toks) toks)
                      cppCommentToks = getCppTokens directiveToks nonDirectiveToks toks'
                  return $ filter goodComment
                         $  map (GHC.commentToAnnotation . toRealLocated . fst) cppCommentToks
        GHC.PFailed pst -> parseError pst


goodComment :: GHC.LEpaComment -> Bool
goodComment c = isGoodComment (tokComment c)
  where
    isGoodComment :: [Comment] -> Bool
    isGoodComment []                 = False
    isGoodComment [Comment "" _ _ _] = False
    isGoodComment _                  = True

toRealLocated :: GHC.Located a -> GHC.PsLocated a
toRealLocated (GHC.L (GHC.RealSrcSpan s (Strict.Just b)) x) = GHC.L (GHC.PsSpan s b) x
toRealLocated (GHC.L l _)                     = GHC.panic $ "toRealLocated:" ++ show l

-- ---------------------------------------------------------------------

-- | Combine the three sets of tokens to produce a single set that
-- represents the code compiled, and will regenerate the original
-- source file.
-- [@directiveToks@] are the tokens corresponding to preprocessor
--                   directives, converted to comments
-- [@origSrcToks@] are the tokenised source of the original code, with
--                 the preprocessor directives stripped out so that
--                 the lexer  does not complain
-- [@postCppToks@] are the tokens that the compiler saw originally
-- NOTE: this scheme will only work for cpp in -nomacro mode
getCppTokens ::
     [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
getCppTokens directiveToks' origSrcToks postCppToks = toks
  where
    directiveToks = trace ("directiveToks: " ++ show directiveToks') directiveToks'
    locFn (GHC.L l1 _,_) (GHC.L l2 _,_) = compare (rs l1) (rs l2)
    m1Toks = mergeBy locFn postCppToks directiveToks

    -- We must now find the set of tokens that are in origSrcToks, but
    -- not in m1Toks

    -- GHC.Token does not have Ord, can't use a set directly
    origSpans = map (\(GHC.L l _,_) -> rs l) origSrcToks
    m1Spans = map (\(GHC.L l _,_) -> rs l) m1Toks
    missingSpans = Set.fromList origSpans Set.\\ Set.fromList m1Spans

    missingToks = filter (\(GHC.L l _,_) -> Set.member (rs l) missingSpans) origSrcToks

    missingAsComments = map mkCommentTok missingToks
      where
        mkCommentTok :: (GHC.Located GHC.Token,String) -> (GHC.Located GHC.Token,String)
        mkCommentTok (GHC.L l _,s) = (GHC.L l (GHC.ITlineComment s (makeBufSpan l)),s)

    toks = mergeBy locFn directiveToks missingAsComments

-- ---------------------------------------------------------------------

tokeniseOriginalSrc ::
  GHC.GhcMonad m
  => GHC.RealSrcLoc -> GHC.ParserOpts -> GHC.StringBuffer
  -> m [(GHC.Located GHC.Token, String)]
tokeniseOriginalSrc startLoc flags buf = do
  let src = stripPreprocessorDirectives buf
  case GHC.lexTokenStream () flags src startLoc of
    GHC.POk _ ts -> return $ GHC.addSourceToTokens startLoc src ts
    GHC.PFailed pst -> parseError pst

-- ---------------------------------------------------------------------

-- | Strip out the CPP directives so that the balance of the source
-- can tokenised.
stripPreprocessorDirectives :: GHC.StringBuffer -> GHC.StringBuffer
stripPreprocessorDirectives buf = buf'
  where
    srcByLine = lines $ sbufToString buf
    noDirectivesLines = map (\line -> case line of '#' : _ -> ""; _ -> line) srcByLine
    buf' = GHC.stringToStringBuffer $ unlines noDirectivesLines

-- ---------------------------------------------------------------------

sbufToString :: GHC.StringBuffer -> String
sbufToString sb@(GHC.StringBuffer _buf len _cur) = GHC.lexemeToString sb len

-- ---------------------------------------------------------------------
getPreprocessedSrcDirect :: (GHC.GhcMonad m)
                         => CppOptions
                         -> FilePath
                         -> m (String, GHC.DynFlags)
getPreprocessedSrcDirect cppOptions src =
    (\(s,_,d) -> (s,d)) <$> getPreprocessedSrcDirectPrim cppOptions src

getPreprocessedSrcDirectPrim :: (GHC.GhcMonad m)
                              => CppOptions
                              -> FilePath
                              -> m (String, GHC.StringBuffer, GHC.DynFlags)
getPreprocessedSrcDirectPrim cppOptions src_fn = do
  hsc_env <- GHC.getSession
  let dfs = GHC.hsc_dflags hsc_env
      new_env = hsc_env { GHC.hsc_dflags = injectCppOptions cppOptions dfs }
  r <- GHC.liftIO $ GHC.preprocess new_env src_fn Nothing (Just (GHC.Cpp GHC.HsSrcFile))
  case r of
    Left err -> error $ showErrorMessages err
    Right (dflags', hspp_fn) -> do
      buf <- GHC.liftIO $ GHC.hGetStringBuffer hspp_fn
      txt <- GHC.liftIO $ readFileGhc hspp_fn
      return (txt, buf, dflags')

showErrorMessages :: GHC.Messages GHC.DriverMessage -> String
showErrorMessages msgs =
  GHC.renderWithContext GHC.defaultSDocContext
    $ GHC.vcat
    $ GHC.pprMsgEnvelopeBagWithLocDefault
    $ GHC.getMessages
    $ msgs

injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
injectCppOptions CppOptions{..} dflags = folded_opt
  where
    mkDefine = ("-D" ++)
    mkIncludeDir = ("-I" ++)
    mkInclude = ("-include" ++)
    file_flags = map mkDefine cppDefine ++ map mkIncludeDir cppInclude ++ map mkInclude cppFile
    addFs = [addOptP, addOptJSP, addOptCmmP]
    folded_opt = foldr ($) dflags (addFs <*> file_flags)

addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP   f = alterToolSettings $ \s -> s
          { GHC.toolSettings_opt_P   = f : GHC.toolSettings_opt_P s
          , GHC.toolSettings_opt_P_fingerprint = GHC.fingerprintStrings (f : GHC.toolSettings_opt_P s)
          }
addOptJSP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptJSP f = alterToolSettings $ \s -> s
          { GHC.toolSettings_opt_JSP   = f : GHC.toolSettings_opt_JSP s
          , GHC.toolSettings_opt_JSP_fingerprint = GHC.fingerprintStrings (f : GHC.toolSettings_opt_JSP s)
          }
addOptCmmP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptCmmP f = alterToolSettings $ \s -> s
          { GHC.toolSettings_opt_CmmP = f : GHC.toolSettings_opt_CmmP s
          , GHC.toolSettings_opt_CmmP_fingerprint = GHC.fingerprintStrings (f : GHC.toolSettings_opt_CmmP s)
          }
alterToolSettings :: (GHC.ToolSettings -> GHC.ToolSettings) -> GHC.DynFlags -> GHC.DynFlags
alterToolSettings f dynFlags = dynFlags { GHC.toolSettings = f (GHC.toolSettings dynFlags) }

-- ---------------------------------------------------------------------

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments' :: FilePath -> IO [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments' srcFile = do
  fcontents <- readFileGhc srcFile
  let directives = filter (\(_lineNum,line) -> case line of '#' : _ -> True; _ -> False)
                    $ zip [1..] (lines fcontents)

  let mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line (makeBufSpan l)),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

  let toks = map mkTok directives
  return toks

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments :: FilePath -> IO [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments srcFile = do
  fcontents <- readFileGhc srcFile
  let directives = filter (\(_lineNum,line) -> case line of '#' : _ -> True; _ -> False)
                    $ zip [1..] (lines fcontents)

  let mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line (makeBufSpan l)),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

  let toks = map mkTok directives
  return toks

-- TODO: possibly use the one from GHC.Parser.PreProcess, depending on
-- which lexer it ends up using. Or have specific versions of lexAll
-- for the two lexers.
lexAll :: GHC.PState GHC.PpState -> (GHC.PState GHC.PpState, [GHC.Located GHC.Token])
lexAll state = case GHC.unP (GHC.lexer True return) state of
-- lexAll state = case unP (lexerDbg True return) state of
-- lexAll state = case unP (Lexer.lexerDbg True return) state of
    GHC.POk s t@(GHC.L _ GHC.ITeof) -> (s, [t])
    -- POk state' t -> (ss, t : rest)
    GHC.POk state' t -> (ss, trace ("lexAll:" ++ show t) t : rest)
      where
        (ss, rest) = lexAll state'
    GHC.PFailed _pst -> GHC.panic $ "GHC.Parser.PreProcess.lexAll failed"

makeBufSpan :: GHC.SrcSpan -> GHC.PsSpan
makeBufSpan (GHC.RealSrcSpan s (Strict.Just bs)) = GHC.PsSpan s bs
makeBufSpan ss = pspan
  where
    bl = GHC.BufPos 0
    pspan = GHC.PsSpan (GHC.realSrcSpan ss) (GHC.BufSpan bl bl)

-- ---------------------------------------------------------------------

parseError :: (GHC.MonadIO m) => GHC.PState p -> m b
parseError pst = GHC.throwErrors (fmap GHC.GhcPsMessage (GHC.getPsErrorMessages pst))

-- ---------------------------------------------------------------------

readFileGhc :: FilePath -> IO String
readFileGhc file = do
  buf@(GHC.StringBuffer _ len _) <- GHC.hGetStringBuffer file
  return (GHC.lexemeToString buf len)

-- ---------------------------------------------------------------------

-- Copied over from MissingH, the dependency cause travis to fail

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys
