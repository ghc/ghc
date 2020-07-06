{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified GHC.Data.Bag          as GHC
import qualified GHC.Data.FastString   as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config     as GHC
import qualified GHC.Driver.Env        as GHC
import qualified GHC.Driver.Phases     as GHC
import qualified GHC.Driver.Pipeline   as GHC
import qualified GHC.Fingerprint.Type  as GHC
import qualified GHC.Parser.Errors.Ppr as GHC
import qualified GHC.Parser.Lexer      as GHC
import qualified GHC.Parser.Lexer      as GHC
import qualified GHC.Settings          as GHC
import qualified GHC.Types.SourceError as GHC
import qualified GHC.Types.SourceFile  as GHC
import qualified GHC.Types.SrcLoc      as GHC
import qualified GHC.Utils.Error       as GHC
import qualified GHC.Utils.Fingerprint as GHC
import GHC.Types.SrcLoc (mkSrcSpan, mkSrcLoc)
import GHC.Data.FastString (mkFastString)

import Data.List hiding (find)
import Data.Maybe
import Types
import Utils
import qualified Data.Set as Set


-- import Debug.Trace
--
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- ---------------------------------------------------------------------

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }

defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

-- ---------------------------------------------------------------------
-- | Remove GHC style line pragams (@{-# LINE .. #-}@) and convert them into comments.
stripLinePragmas :: String -> (String, [Comment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe Comment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe Comment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
           mSrcLoc = mkSrcLoc (mkFastString "LINE")
           ss     = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (size+1))
       in (res, Just $ mkComment pragma (GHC.spanAsAnchor ss))
  -- Deal with shebang/cpp directives too
  -- x |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  |  "#!" `isPrefixOf` s =
    let mSrcLoc = mkSrcLoc (mkFastString "SHEBANG")
        ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (length s))
    in
    ("",Just $ mkComment s (GHC.spanAsAnchor ss))
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
                       -> m [Comment]
getCppTokensAsComments cppOptions sourceFile = do
  source <- GHC.liftIO $ GHC.hGetStringBuffer sourceFile
  let startLoc = GHC.mkRealSrcLoc (GHC.mkFastString sourceFile) 1 1
  (_txt,strSrcBuf,flags2') <- getPreprocessedSrcDirectPrim cppOptions sourceFile
  let flags2 = GHC.initParserOpts flags2'
  -- #ifdef tokens
  directiveToks <- GHC.liftIO $ getPreprocessorAsComments sourceFile
  -- Tokens without #ifdef
  nonDirectiveToks <- tokeniseOriginalSrc startLoc flags2 source
  case GHC.lexTokenStream flags2 strSrcBuf startLoc of
        GHC.POk _ ts ->
               do
                  let toks = GHC.addSourceToTokens startLoc source ts
                      cppCommentToks = getCppTokens directiveToks nonDirectiveToks toks
                  return $ filter goodComment
                         $  map (tokComment . GHC.commentToAnnotation . toRealLocated . fst) cppCommentToks
        GHC.PFailed pst -> parseError pst

goodComment :: Comment -> Bool
goodComment (Comment "" _ _) = False
goodComment _              = True


toRealLocated :: GHC.Located a -> GHC.RealLocated a
toRealLocated (GHC.L (GHC.RealSrcSpan s _) x) = GHC.L s              x
toRealLocated (GHC.L _ x)                     = GHC.L badRealSrcSpan x

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
getCppTokens directiveToks origSrcToks postCppToks = toks
  where
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
        mkCommentTok (GHC.L l _,s) = (GHC.L l (GHC.ITlineComment s),s)

    toks = mergeBy locFn directiveToks missingAsComments

-- ---------------------------------------------------------------------

tokeniseOriginalSrc ::
  GHC.GhcMonad m
  => GHC.RealSrcLoc -> GHC.ParserOpts -> GHC.StringBuffer
  -> m [(GHC.Located GHC.Token, String)]
tokeniseOriginalSrc startLoc flags buf = do
  let src = stripPreprocessorDirectives buf
  case GHC.lexTokenStream flags src startLoc of
    GHC.POk _ ts -> return $ GHC.addSourceToTokens startLoc src ts
    GHC.PFailed pst -> parseError pst

-- ---------------------------------------------------------------------

-- | Strip out the CPP directives so that the balance of the source
-- can tokenised.
stripPreprocessorDirectives :: GHC.StringBuffer -> GHC.StringBuffer
stripPreprocessorDirectives buf = buf'
  where
    srcByLine = lines $ sbufToString buf
    noDirectivesLines = map (\line -> if line /= [] && head line == '#' then "" else line) srcByLine
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
  -- (dflags', hspp_fn) <-
  r <- GHC.liftIO $ GHC.preprocess new_env src_fn Nothing (Just (GHC.Cpp GHC.HsSrcFile))
  case r of
    Left err -> error $ showErrorMessages err
    Right (dflags', hspp_fn) -> do
      buf <- GHC.liftIO $ GHC.hGetStringBuffer hspp_fn
      txt <- GHC.liftIO $ readFileGhc hspp_fn
      return (txt, buf, dflags')

showErrorMessages :: GHC.ErrorMessages -> String
showErrorMessages msgs = intercalate "\n" $ map show $ GHC.bagToList msgs

injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
injectCppOptions CppOptions{..} dflags =
  foldr addOptP dflags (map mkDefine cppDefine ++ map mkIncludeDir cppInclude ++ map mkInclude cppFile)
  where
    mkDefine = ("-D" ++)
    mkIncludeDir = ("-I" ++)
    mkInclude = ("-include" ++)


addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP   f = alterToolSettings $ \s -> s
          { GHC.toolSettings_opt_P   = f : GHC.toolSettings_opt_P s
          , GHC.toolSettings_opt_P_fingerprint = fingerprintStrings (f : GHC.toolSettings_opt_P s)
          }
alterToolSettings :: (GHC.ToolSettings -> GHC.ToolSettings) -> GHC.DynFlags -> GHC.DynFlags
alterToolSettings f dynFlags = dynFlags { GHC.toolSettings = f (GHC.toolSettings dynFlags) }

fingerprintStrings :: [String] -> GHC.Fingerprint
fingerprintStrings ss = GHC.fingerprintFingerprints $ map GHC.fingerprintString ss

-- ---------------------------------------------------------------------

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments :: FilePath -> IO [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments srcFile = do
  fcontents <- readFileGhc srcFile
  let directives = filter (\(_lineNum,line) -> line /= [] && head line == '#')
                    $ zip [1..] (lines fcontents)

  let mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

  let toks = map mkTok directives
  return toks

-- ---------------------------------------------------------------------

parseError :: (GHC.MonadIO m) => GHC.PState -> m b
parseError pst = do
     let
       -- (warns,errs) = GHC.getMessages pst dflags
     -- throw $ GHC.mkSrcErr (GHC.unitBag $ GHC.mkPlainErrMsg dflags sspan err)
     GHC.throwErrors (fmap GHC.pprError (GHC.getErrorMessages pst))

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

