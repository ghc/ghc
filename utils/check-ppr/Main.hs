{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Data
import SrcLoc
import GHC hiding (moduleName)
import GHC.Hs.Dump
import GHC.Driver.Session
import Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath

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

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       p <- parseOneFile libdir fileName
       let
         origAst = showSDoc unsafeGlobalDynFlags
                     $ showAstData BlankSrcSpan
                     $ eraseLayoutInfo (pm_parsed_source p)
         pped    = pragmas ++ "\n" ++ pp (pm_parsed_source p)
         anns    = pm_annotations p
         pragmas = getPragmas anns

         newFile = dropExtension fileName <.> "ppr" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"

       writeFile astFile origAst
       writeFile newFile pped

       p' <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = showSDoc unsafeGlobalDynFlags
                         $ showAstData BlankSrcSpan
                         $ eraseLayoutInfo (pm_parsed_source p')
       writeFile newAstFile newAstStr

       if origAst == newAstStr
         then do
           -- putStrLn "ASTs matched"
           exitSuccess
         else do
           putStrLn "AST Match Failed"
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

getPragmas :: ApiAnns -> String
getPragmas anns = pragmaStr
  where
    tokComment (L _ (AnnBlockComment s)) = s
    tokComment (L _ (AnnLineComment  s)) = s
    tokComment _ = ""

    comments = map tokComment $ sortRealLocated $ apiAnnRogueComments anns
    pragmas = filter (\c -> isPrefixOf "{-#" c ) comments
    pragmaStr = intercalate "\n" pragmas

pp :: (Outputable a) => a -> String
pp a = showPpr unsafeGlobalDynFlags a

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
