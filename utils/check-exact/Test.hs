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
import GHC.Data.Bag
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
 -- "cases/Rename1.hs" changeRename1
 -- "cases/Rename2.hs" changeRename2
 -- "cases/LayoutIn1.hs" changeLayoutIn1
 -- "cases/LayoutIn3.hs" changeLayoutIn3
 -- "cases/LayoutIn3a.hs" changeLayoutIn3
 -- "cases/LayoutIn3b.hs" changeLayoutIn3
 -- "cases/LayoutIn4.hs" changeLayoutIn4
 -- "cases/LocToName.hs" changeLocToName
 -- "cases/LetIn1.hs" changeLetIn1
 -- "cases/WhereIn4.hs" changeWhereIn4
 "cases/AddDecl.hs" changeAddDecl



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
         origAst = ppAst (pm_parsed_source p)
         anns'   = pm_annotations p
         -- pped    = pragmas ++ "\n" ++ (exactPrint $ pm_parsed_source p)
         pped    = exactPrint (pm_parsed_source p) anns'
         -- pragmas = getPragmas anns'

         newFile        = dropExtension fileName <.> "ppr"     <.> takeExtension fileName
         newFileChanged = dropExtension fileName <.> "changed" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"
         changedAstFile = fileName <.> "ast.changed"

       -- pped' <- exactprintWithChange changeRenameCase1 (pm_parsed_source p) anns'
       (pped', ast') <- exactprintWithChange changer (pm_parsed_source p) anns'
       -- putStrLn $ "\n\nabout to writeFile"
       writeFile changedAstFile (ppAst ast')
       writeFile astFile origAst
       -- putStrLn $ "\n\nabout to pp"
       writeFile newFile        pped
       writeFile newFileChanged pped'

       -- putStrLn $ "anns':" ++ showPprUnsafe (apiAnnRogueComments anns')

       p' <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = ppAst (pm_parsed_source p')
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

ppAst ast = showSDocUnsafe $ showAstData BlankSrcSpanFile NoBlankApiAnnotations ast

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

exactprintWithChange :: Changer -> ParsedSource -> ApiAnns -> IO (String, ParsedSource)
exactprintWithChange f p anns = do
  (anns',p') <- f anns p
  return (exactPrint p' anns', p')


type Changer = (ApiAnns -> ParsedSource -> IO (ApiAnns,ParsedSource))

noChange :: Changer
noChange ans parsed = return (ans,parsed)

changeRenameCase1 :: Changer
changeRenameCase1 ans parsed = return (ans,rename "bazLonger" [((3,15),(3,18))] parsed)

changeLayoutLet2 :: Changer
changeLayoutLet2 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLayoutLet3 :: Changer
changeLayoutLet3 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeLayoutIn1 :: Changer
changeLayoutIn1 ans parsed = return (ans,rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed)

changeLayoutIn3 :: Changer
changeLayoutIn3 ans parsed = return (ans,rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed)

changeLayoutIn4 :: Changer
changeLayoutIn4 ans parsed = return (ans,rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed)

changeLocToName :: Changer
changeLocToName ans parsed = return (ans,rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed)


changeRename1 :: Changer
changeRename1 ans parsed = return (ans,rename "bar2" [((3,1),(3,4))] parsed)

changeRename2 :: Changer
changeRename2 ans parsed = return (ans,rename "joe" [((2,1),(2,5))] parsed)

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

changeWhereIn4 :: Changer
changeWhereIn4 ans parsed
  = return (ans,everywhere (mkT replace) parsed)
  where
    replace :: LocatedN RdrName -> LocatedN RdrName
    replace (L ln _n)
      | ss2range (locA ln) == ((12,16),(12,17)) = L ln (mkRdrUnqual (mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Changer
changeLetIn1 ans parsed
  = return (ans,everywhere (mkT replace) parsed)
  where
    replace :: HsExpr GhcPs -> HsExpr GhcPs
    replace (HsLet an localDecls expr)
      =
         let (HsValBinds x (ValBinds xv bagDecls sigs)) = localDecls
             decls@(l1:l2:ls) = reverse $ bagToList bagDecls
             l2' = remove l2 l1
             bagDecls' = listToBag $ reverse (l2':ls)
         in (HsLet an (HsValBinds x (ValBinds xv bagDecls' sigs)) expr)

    replace x = x
-- ---------------------------------------------------------------------

-- | Add a declaration to AddDecl
changeAddDecl :: Changer
changeAddDecl ans top = do
  Right (declAnns, decl) <- withDynFlags (\df -> parseDecl df "<interactive>" "nn = n2")
  -- putStrLn $ "changeDecl:(declAnns,decl)=" ++ showGhc (declAnns,decl)
  let declAnns' = setPrecedingLines decl 2 0 declAnns
  -- putStrLn $ "changeDecl:(declAnns',decl)=" ++ showGhc (declAnns',decl)

  let (p',(ans',_),_) = runTransform ans doAddDecl
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top
      replaceTopLevelDecls :: ParsedSource -> Transform (ParsedSource)
      replaceTopLevelDecls m = insertAtStart m decl
  return (mergeAnns declAnns' ans',p')

-- ---------------------------------------------------------------------
-- Next section to be moved to the appropriate library

remove :: (Monoid t) => LocatedAn t a -> LocatedAn u b -> LocatedAn t a
remove (L (SrcSpanAnn ApiAnnNotUsed l) v) lb
  = L (SrcSpanAnn (ApiAnn (DeletedAnchor (realSrcSpan l) (locatedAnAnchor lb)) mempty []) l) v
remove (L (SrcSpanAnn (ApiAnn a an cs) l) v) lb
  = L (SrcSpanAnn (ApiAnn (DeletedAnchor (anchor a) (locatedAnAnchor lb)) an cs) l) v

locatedAnAnchor :: LocatedAn a t -> RealSrcSpan
locatedAnAnchor (L (SrcSpanAnn ApiAnnNotUsed l) _) = realSrcSpan l
locatedAnAnchor (L (SrcSpanAnn (ApiAnn a _ _) _) _) = anchor a

-- End of section to be moved to the appropriate library
-- ---------------------------------------------------------------------


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
