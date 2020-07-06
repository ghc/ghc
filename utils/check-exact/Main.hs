{-# LANGUAGE ScopedTypeVariables #-}

-- import Data.List
-- import GHC.Types.SrcLoc
import GHC hiding (moduleName)
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs.Dump
-- import GHC.Hs.Exact hiding (ExactPrint())
import GHC.Utils.Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath
import ExactPrint
-- exactPrint = undefined
-- showPprUnsafe = undefined

-- ---------------------------------------------------------------------

tt :: IO ()
tt = testOneFile "/home/alanz/mysrc/git.haskell.org/ghc/_build/stage1/lib"
  -- "Test.hs"
 -- "../../testsuite/tests/printer/Ppr001.hs"
 -- "../../testsuite/tests/printer/Ppr002.hs"
 -- "../../testsuite/tests/printer/Ppr003.hs"
 -- "../../testsuite/tests/printer/Ppr004.hs"
 -- "../../testsuite/tests/printer/Ppr005.hs"
 -- "../../testsuite/tests/qualifieddo/should_compile/qdocompile001.hs"
 -- "../../testsuite/tests/printer/Ppr006.hs"
 -- "../../testsuite/tests/printer/Ppr007.hs"
 -- "../../testsuite/tests/printer/Ppr008.hs"
 -- "../../testsuite/tests/hiefile/should_compile/hie008.hs"
 -- "../../testsuite/tests/printer/Ppr009.hs"
 -- "../../testsuite/tests/printer/Ppr011.hs"
 -- "../../testsuite/tests/printer/Ppr012.hs"
 -- "../../testsuite/tests/printer/Ppr013.hs"
 -- "../../testsuite/tests/printer/Ppr014.hs"
 -- "../../testsuite/tests/printer/Ppr015.hs"
 -- "../../testsuite/tests/printer/Ppr016.hs"
 -- "../../testsuite/tests/printer/Ppr017.hs"
 -- "../../testsuite/tests/printer/Ppr018.hs"
 -- "../../testsuite/tests/printer/Ppr019.hs"
 -- "../../testsuite/tests/printer/Ppr020.hs"
 -- "../../testsuite/tests/printer/Ppr021.hs"
 -- "../../testsuite/tests/printer/Ppr022.hs"
 -- "../../testsuite/tests/printer/Ppr023.hs"
 -- "../../testsuite/tests/printer/Ppr024.hs"
 -- "../../testsuite/tests/printer/Ppr025.hs"
 -- "../../testsuite/tests/printer/Ppr026.hs"
 -- "../../testsuite/tests/printer/Ppr027.hs"
 -- "../../testsuite/tests/printer/Ppr028.hs"
 -- "../../testsuite/tests/printer/Ppr029.hs"
 -- "../../testsuite/tests/printer/Ppr030.hs"
 -- "../../testsuite/tests/printer/Ppr031.hs"
 -- "../../testsuite/tests/printer/Ppr032.hs"
 -- "../../testsuite/tests/printer/Ppr033.hs"
 -- "../../testsuite/tests/printer/Ppr034.hs"
 -- "../../testsuite/tests/printer/Ppr035.hs"
 -- "../../testsuite/tests/printer/Ppr036.hs"
 -- "../../testsuite/tests/printer/Ppr037.hs"
 -- "../../testsuite/tests/printer/Ppr038.hs"
 -- "../../testsuite/tests/printer/Ppr039.hs"
 -- "../../testsuite/tests/printer/Ppr040.hs"
 -- "../../testsuite/tests/printer/Ppr041.hs"
 -- "../../testsuite/tests/printer/Ppr042.hs"
 -- "../../testsuite/tests/printer/Ppr043.hs"
 -- "../../testsuite/tests/printer/Ppr044.hs"
 -- "../../testsuite/tests/printer/Ppr045.hs"
 -- "../../testsuite/tests/printer/Ppr046.hs"
 -- Not tested, the GENERATED pragma is getting removed "../../testsuite/tests/printer/Ppr047.hs"
 -- "../../testsuite/tests/printer/Ppr048.hs"
 -- "../../testsuite/tests/printer/Ppr049.hs"
 -- "../../testsuite/tests/printer/T13050p.hs"
 -- "../../testsuite/tests/printer/T13199.hs"
 -- "../../testsuite/tests/printer/T13550.hs"
 -- "../../testsuite/tests/printer/T13942.hs"
 -- "../../testsuite/tests/printer/T14289b.hs"
 -- "../../testsuite/tests/printer/T14289c.hs"
 -- "../../testsuite/tests/printer/T14289.hs"
 -- "../../testsuite/tests/printer/T14306.hs"
 -- "../../testsuite/tests/printer/T14343b.hs"
 -- "../../testsuite/tests/printer/T14343.hs"
 -- "../../testsuite/tests/printer/T15761.hs"
 -- "../../testsuite/tests/printer/T18052a.hs"
 -- "../../testsuite/tests/printer/T18247a.hs"
 -- "../../testsuite/tests/printer/Ppr050.hs"
 -- "../../testsuite/tests/printer/Ppr051.hs"
 -- "../../testsuite/tests/printer/Ppr052.hs"
 -- "../../testsuite/tests/typecheck/should_fail/T17566c.hs"
 -- "../../testsuite/tests/hiefile/should_compile/Constructors.hs"
 -- "../../testsuite/tests/printer/StarBinderAnns.hs"
 -- "../../testsuite/tests/typecheck/should_fail/StrictBinds.hs"
 -- "../../testsuite/tests/printer/Test10276.hs"
 -- "../../testsuite/tests/printer/Test10278.hs"
 -- "../../testsuite/tests/printer/Test12417.hs"
 -- "../../testsuite/tests/parser/should_compile/T14189.hs"
 -- "../../testsuite/tests/printer/Test16212.hs"
 -- "../../testsuite/tests/printer/Test10312.hs"
 -- "../../testsuite/tests/printer/Test10354.hs"
 -- "../../testsuite/tests/printer/Test10357.hs"
 -- "../../testsuite/tests/printer/Test10399.hs"
 -- "../../testsuite/tests/printer/Test11018.hs"
 -- "../../testsuite/tests/printer/Test11332.hs"
 -- "../../testsuite/tests/printer/Test16230.hs"
 -- "../../testsuite/tests/printer/AnnotationLet.hs"
 -- "../../testsuite/tests/printer/AnnotationTuple.hs"
  -- "../../testsuite/tests/ghc-api/annotations/CommentsTest.hs"
 -- "../../testsuite/tests/hiefile/should_compile/Scopes.hs"
 -- "../../testsuite/tests/printer/Ppr053.hs"
 -- "../../testsuite/tests/printer/Ppr054.hs"
 "../../testsuite/tests/printer/Ppr055.hs"


-- exact = ppr

-- ---------------------------------------------------------------------

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
       -- putStrLn $ "\n\ngot p"
       let
         origAst = showSDocUnsafe
                     $ showAstData BlankSrcSpanFile NoBlankApiAnnotations
                                                         (pm_parsed_source p)
         anns'   = pm_annotations p
         -- pped    = pragmas ++ "\n" ++ (exactPrint $ pm_parsed_source p)
         pped    = exactPrint (pm_parsed_source p) anns'
         -- pragmas = getPragmas anns'

         newFile = dropExtension fileName <.> "ppr" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"

       -- putStrLn $ "\n\nabout to writeFile"
       writeFile astFile origAst
       -- putStrLn $ "\n\nabout to pp"
       writeFile newFile pped

       -- putStrLn $ "anns':" ++ showPprUnsafe (apiAnnRogueComments anns')

       p' <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = showSDocUnsafe
                         $ showAstData BlankSrcSpanFile NoBlankApiAnnotations
                                                         (pm_parsed_source p')
       writeFile newAstFile newAstStr

       -- putStrLn $ "\n\nanns':" ++ showPprUnsafe (apiAnnRogueComments anns')

       if origAst == newAstStr
         then do
           -- putStrLn "ASTs matched"
           exitSuccess
         else do
           putStrLn "AST Match Failed"
           -- putStrLn "\n===================================\nOrig\n\n"
           -- putStrLn origAst
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

-- getPragmas :: ApiAnns -> String
-- getPragmas anns' = pragmaStr
--   where
--     tokComment (L _ (AnnBlockComment s)) = s
--     tokComment (L _ (AnnLineComment  s)) = s
--     tokComment _ = ""

--     comments' = map tokComment $ sortRealLocated $ apiAnnRogueComments anns'
--     pragmas = filter (\c -> isPrefixOf "{-#" c ) comments'
--     pragmaStr = intercalate "\n" pragmas

-- pp :: (Outputable a) => a -> String
-- pp a = showPpr unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------
