{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Data
import GHC.Types.SrcLoc
import GHC hiding (moduleName)
import GHC.Hs.Dump
import GHC.Driver.Session
import GHC.Utils.Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath
import System.IO

usage :: String
usage = unlines
    [ "usage: check-ppr (libdir) (file)"
    , ""
    , "where libdir is the GHC library directory (e.g. the output of"
    , "ghc --print-libdir) and file is the file to parse."
    ]

main :: IO()
main = do
  args <- getArgs
  case args of
   [libdir,fileName] -> testOneFile libdir fileName
   _ -> putStrLn usage

-- | N.B. It's important that we write our output as binary lest Windows will
-- change our LF line endings to CRLF, which will show up in the AST when we
-- re-parse.
writeBinFile :: FilePath -> String -> IO()
writeBinFile fpath x = withBinaryFile fpath WriteMode (\h -> hSetEncoding h utf8 >> hPutStr h x)

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       p <- parseOneFile libdir fileName
       let
         origAst = showPprUnsafe
                     $ showAstData BlankSrcSpan BlankEpAnnotations
                     $ eraseLayoutInfo (pm_parsed_source p)
         pped    = pragmas ++ "\n" ++ pp (pm_parsed_source p)
         pragmas = getPragmas (pm_parsed_source p)

         newFile = dropExtension fileName <.> "ppr" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"

       writeBinFile astFile origAst
       writeBinFile newFile pped

       p' <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = showPprUnsafe
                         $ showAstData BlankSrcSpan BlankEpAnnotations
                         $ eraseLayoutInfo (pm_parsed_source p')
       writeBinFile newAstFile newAstStr

       if origAst == newAstStr
         then do
           -- putStrLn "ASTs matched"
           exitSuccess
         else do
           putStrLn "ppr AST Match Failed"
           putStrLn "\n===================================\nOrig\n\n"
           putStrLn origAst
           putStrLn "\n===================================\nNew\n\n"
           putStrLn newAstStr
           exitFailure


parseOneFile :: FilePath -> FilePath -> IO ParsedModule
parseOneFile libdir fileName = do
       let modByFile m =
             case ml_hs_file $ ms_location m of
               Nothing -> False
               Just fn -> fn == fileName
       runGhc (Just libdir) $ do
         dflags <- getSessionDynFlags
         let dflags2 = dflags `gopt_set` Opt_KeepRawTokenStream
         _ <- setSessionDynFlags dflags2
         addTarget Target { targetId = TargetFile fileName Nothing
                          , targetAllowObjCode = True
                          , targetContents = Nothing }
         _ <- load LoadAllTargets
         graph <- getModuleGraph
         let
           modSum = case filter modByFile (mgModSummaries graph) of
                     [x] -> x
                     xs -> error $ "Can't find module, got:"
                              ++ show (map (ml_hs_file . ms_location) xs)
         parseModule modSum

getPragmas :: Located HsModule -> String
getPragmas (L _ (HsModule { hsmodAnn = anns'})) = pragmaStr
  where
    tokComment (L _ (EpaComment (EpaBlockComment s) _)) = s
    tokComment (L _ (EpaComment (EpaLineComment  s) _)) = s
    tokComment _ = ""

    cmp (L l1 _) (L l2 _) = compare (anchor l1) (anchor l2)
    comments' = map tokComment $ sortBy cmp $ priorComments $ epAnnComments anns'
    pragmas = filter (\c -> isPrefixOf "{-#" c ) comments'
    pragmaStr = intercalate "\n" pragmas

pp :: (Outputable a) => a -> String
pp a = showPprUnsafe a

eraseLayoutInfo :: ParsedSource -> ParsedSource
eraseLayoutInfo = everywhere go
  where
    go :: forall a. Typeable a => a -> a
    go x =
      case eqT @a @LayoutInfo of
        Nothing -> x
        Just Refl -> NoLayoutInfo

-- ---------------------------------------------------------------------

-- Copied from syb for the test

everywhere :: (forall a. Data a => a -> a)
           -> (forall a. Data a => a -> a)
everywhere f = go
  where
    go :: forall a. Data a => a -> a
    go = f . gmapT go
