{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- import Data.List
import Data.Data
import Data.Typeable
-- import GHC.Types.SrcLoc
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC hiding (moduleName)
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs.Dump
-- import GHC.Types.SourceText
-- import GHC.Hs.Exact hiding (ExactPrint())
-- import GHC.Utils.Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath

import Types
import Utils
import ExactPrint
-- exactPrint = undefined
-- showPprUnsafe = undefined

-- ---------------------------------------------------------------------

tt :: IO ()
-- tt = testOneFile "/home/alanz/mysrc/git.haskell.org/ghc/_build/stage1/lib"
tt = testOneFile "/home/alanz/mysrc/git.haskell.org/worktree/exactprint/_build/stage1/lib"
 -- "cases/RenameCase1.hs" changeRenameCase1
 -- "cases/LayoutLet2.hs" changeLayoutLet2
 -- "cases/LayoutLet3.hs" changeLayoutLet3
 -- "cases/LayoutLet4.hs" changeLayoutLet3
 "cases/Rename1.hs" changeRename1

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
   [libdir,fileName] -> testOneFile libdir fileName noChange
   _ -> putStrLn usage

testOneFile :: FilePath -> String -> Changer -> IO ()
testOneFile libdir fileName changer = do
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

         newFile        = dropExtension fileName <.> "ppr"     <.> takeExtension fileName
         newFileChanged = dropExtension fileName <.> "changed" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"

       -- pped' <- exactprintWithChange changeRenameCase1 (pm_parsed_source p) anns'
       pped' <- exactprintWithChange changer (pm_parsed_source p) anns'
       -- putStrLn $ "\n\nabout to writeFile"
       writeFile astFile origAst
       -- putStrLn $ "\n\nabout to pp"
       writeFile newFile        pped
       writeFile newFileChanged pped'

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

exactprintWithChange :: Changer -> ParsedSource -> ApiAnns -> IO String
exactprintWithChange f p anns = do
  (anns',p') <- f anns p
  return $ exactPrint p' anns'


type Changer = (ApiAnns -> ParsedSource -> IO (ApiAnns,ParsedSource))

noChange :: Changer
noChange ans parsed = return (ans,parsed)

changeRenameCase1 :: Changer
changeRenameCase1 ans parsed = return (ans,rename "bazLonger" [((3,15),(3,18))] parsed)

changeLayoutLet2 :: Changer
changeLayoutLet2 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLayoutLet3 :: Changer
changeLayoutLet3 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeRename1 :: Changer
changeRename1 ans parsed = return (ans,rename "bar2" [((3,1),(3,4))] parsed)

rename :: (Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans' a
  = everywhere (mkT replaceRdr) a
  where
    newName = mkRdrUnqual (mkVarOcc newNameStr)

    cond :: SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans'

    replaceRdr :: LocatedN RdrName -> LocatedN RdrName
    replaceRdr (L ln _)
        | cond (locA ln) = L ln newName
    replaceRdr x = x

-- ---------------------------------------------------------------------
-- From SYB

-- | Apply transformation on each level of a tree.
--
-- Just like 'everything', this is stolen from SYB package.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)

-- | Create generic transformation.
--
-- Another function stolen from SYB package.
mkT :: (Typeable a, Typeable b) => (b -> b) -> (a -> a)
mkT f = case cast f of
    Just f' -> f'
    Nothing -> id
