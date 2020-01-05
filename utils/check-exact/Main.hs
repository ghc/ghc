{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import GHC.Types.SrcLoc
import GHC hiding (moduleName)
import GHC.Driver.Session
import GHC.Hs.Dump
-- import GHC.Hs.Exact hiding (ExactPrint())
import GHC.Utils.Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath
import ExactPrint
-- exactPrint = undefined

-- ---------------------------------------------------------------------

tt :: IO ()
tt = testOneFile "/home/alanz/mysrc/git.haskell.org/ghc/_build/bindist/ghc-8.11.0.20200524-x86_64-unknown-linux/lib"
  "Test.hs"

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
       let
         origAst = showSDoc unsafeGlobalDynFlags
                     $ showAstData BlankSrcSpanFile NoBlankApiAnnotations
                                                         (pm_parsed_source p)
         anns'   = pm_annotations p
         -- pped    = pragmas ++ "\n" ++ (exactPrint $ pm_parsed_source p)
         pped    = exactPrint (pm_parsed_source p) anns'
         pragmas = getPragmas anns'

         newFile = dropExtension fileName <.> "ppr" <.> takeExtension fileName
         astFile = fileName <.> "ast"
         newAstFile = fileName <.> "ast.new"

       writeFile astFile origAst
       writeFile newFile pped

       p' <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = showSDoc unsafeGlobalDynFlags
                         $ showAstData BlankSrcSpanFile NoBlankApiAnnotations
                                                         (pm_parsed_source p')
       writeFile newAstFile newAstStr

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

getPragmas :: ApiAnns -> String
getPragmas anns' = pragmaStr
  where
    tokComment (L _ (AnnBlockComment s)) = s
    tokComment (L _ (AnnLineComment  s)) = s
    tokComment _ = ""

    comments' = map tokComment $ sortRealLocated $ apiAnnRogueComments anns'
    pragmas = filter (\c -> isPrefixOf "{-#" c ) comments'
    pragmaStr = intercalate "\n" pragmas

pp :: (Outputable a) => a -> String
pp a = showPpr unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------

-- instance ExactPrint (LocatedA RdrName) where
--   exact (L l n) = ppr n

{-
Code in ghc-exactprint

isSymRdr :: GHC.RdrName -> Bool
isSymRdr n = GHC.isSymOcc (GHC.rdrNameOcc n) || rdrName2String n == "."

instance Annotate GHC.RdrName where
  markAST l n = do
    let
      str = rdrName2String n
      isSym = isSymRdr n
      doNormalRdrName = do
        let str' = case str of
              -- TODO: unicode support?
                        "forall" -> if spanLength l == 1 then "∀" else str
                        _ -> str

        let
          markParen :: GHC.AnnKeywordId -> Annotated ()
          markParen pa = do
            if isSym
              then ifInContext (Set.fromList [PrefixOp,PrefixOpDollar])
                                       (mark         pa) -- '('
                                       (markOptional pa)
              else markOptional pa

        markOptional GHC.AnnSimpleQuote
        markParen GHC.AnnOpenP
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 0
        cnt  <- countAnns GHC.AnnVal
        case cnt of
          0 -> markExternal l GHC.AnnVal str'
          1 -> markWithString GHC.AnnVal str'
          _ -> traceM $ "Printing RdrName, more than 1 AnnVal:" ++ showGhc (l,n)
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 1
        markParen GHC.AnnCloseP

    case n of
      GHC.Unqual _ -> doNormalRdrName
      GHC.Qual _ _ -> doNormalRdrName
      GHC.Orig _ _ -> if str == "~"
                        then doNormalRdrName
                        -- then error $ "GHC.orig:(isSym,canParen)=" ++ show (isSym,canParen)
                        else markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> error $ "GHC.orig:str=[" ++ str ++ "]"
      GHC.Exact n'  -> do
       case str of
         -- Special handling for Exact RdrNames, which are built-in Names
         "[]" -> do
           mark GHC.AnnOpenS  -- '['
           mark GHC.AnnCloseS -- ']'
         "()" -> do
           mark GHC.AnnOpenP  -- '('
           mark GHC.AnnCloseP -- ')'
         ('(':'#':_) -> do
           markWithString GHC.AnnOpen  "(#" -- '(#'
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           markWithString GHC.AnnClose  "#)"-- '#)'
         "[::]" -> do
           markWithString GHC.AnnOpen  "[:" -- '[:'
           markWithString GHC.AnnClose ":]" -- ':]'
         "->" -> do
           mark GHC.AnnOpenP -- '('
           mark GHC.AnnRarrow
           mark GHC.AnnCloseP -- ')'
         -- "~#"  -> do
         --   mark GHC.AnnOpenP -- '('
         --   mark GHC.AnnTildehsh
         --   mark GHC.AnnCloseP
         "~"  -> do
           doNormalRdrName
         "*"  -> do
           markExternal l GHC.AnnVal str
         "★"  -> do -- Note: unicode star
           markExternal l GHC.AnnVal str
         ":"  -> do
           -- Note: The OccName for ":" has the following attributes (via occAttributes)
           -- (d, Data DataSym Sym Val )
           -- consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon
           doNormalRdrName
           -- trace ("RdrName.checking :" ++ (occAttributes $ GHC.occName n)) doNormalRdrName
         ('(':',':_) -> do
           mark GHC.AnnOpenP
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           mark GHC.AnnCloseP -- ')'
         _ -> do
            let isSym' = isSymRdr  (GHC.nameRdrName n')
            when isSym' $ mark GHC.AnnOpenP -- '('
            markWithString GHC.AnnVal str
            when isSym $ mark GHC.AnnCloseP -- ')'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in RdrName")


-}
