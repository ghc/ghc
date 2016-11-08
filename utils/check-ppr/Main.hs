{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import Data.Data hiding (Fixity)
import Data.List
import Bag
import FastString
import NameSet
import SrcLoc
import HsSyn
import OccName hiding (occName)
import GHC hiding (moduleName)
import Var
import DynFlags
import Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath

import qualified Data.ByteString as B
import qualified Data.Map        as Map

main::IO()
main = do
  args <- getArgs
  case args of
   [libdir,fileName] -> testOneFile libdir fileName
   _ -> putStrLn "invoke with the libdir and a file to parse."

testOneFile :: FilePath -> String -> IO ()
testOneFile libdir fileName = do
       p <- parseOneFile libdir fileName
       let
         origAst = showAstData 0 (pm_parsed_source p)
         pped    = pragmas ++ "\n" ++ pp (pm_parsed_source p)
         anns    = pm_annotations p
         pragmas = getPragmas anns

         newFile = dropExtension fileName <.> "ppr" <.> takeExtension fileName
         astFile = fileName <.> "ast"

       writeFile astFile origAst
       writeFile newFile pped

       p' <- parseOneFile libdir newFile

       let newAstStr = showAstData 0 (pm_parsed_source p')

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
           modSum = case filter modByFile graph of
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

    comments = case Map.lookup noSrcSpan (snd anns) of
      Nothing -> []
      Just cl -> map tokComment $ sortLocated cl
    pragmas = filter (\c -> isPrefixOf "{-#" c ) comments
    pragmaStr = intercalate "\n" pragmas

pp :: (Outputable a) => a -> String
pp a = showPpr unsafeGlobalDynFlags a


-- | Show a GHC AST with SrcSpan's blanked out, to avoid comparing locations,
-- only structure
showAstData :: Data a => Int -> a -> String
showAstData n =
  generic
          `ext1Q` list
          `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` bytestring
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` overLit
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` fixity
          `ext2Q` located
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (unwords (gmapQ (showAstData (n+1)) t)) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent i = "\n" ++ replicate i ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: FastString -> String
        bytestring = show :: B.ByteString -> String
        list l     = indent n ++ "["
                              ++ intercalate "," (map (showAstData (n+1)) l) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDocDebug_ . ppr :: Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . ppr :: ModuleName -> String

        srcSpan :: SrcSpan -> String
        srcSpan _ss = "{ "++ "ss" ++"}"

        var        = ("{Var: "++) . (++"}") . showSDocDebug_ . ppr :: Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . ppr :: DataCon -> String

        overLit :: HsOverLit RdrName -> String
        overLit    = ("{HsOverLit:"++) . (++"}") . showSDoc_ . ppr

        bagRdrName:: Bag (Located (HsBind RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . bagToList
        bagName   :: Bag (Located (HsBind Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . bagToList
        bagVar    :: Bag (Located (HsBind Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . bagToList

        nameSet = ("{NameSet: "++) . (++"}") . list . nameSetElemsStable

        fixity = ("{Fixity: "++) . (++"}") . showSDoc_ . ppr :: Fixity -> String

        located :: (Data b,Data loc) => GenLocated loc b -> String
        located (L ss a) =
          indent n ++ "("
            ++ case cast ss of
                    Just (s :: SrcSpan) ->
                      srcSpan s
                    Nothing -> "nnnnnnnn"
                  ++ showAstData (n+1) a
                  ++ ")"

showSDoc_ :: SDoc -> String
showSDoc_ = showSDoc unsafeGlobalDynFlags

showSDocDebug_ :: SDoc -> String
showSDocDebug_ = showSDocDebug unsafeGlobalDynFlags

-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))

-- | Flexible type extension
ext1 :: (Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)



-- | Flexible type extension
ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)



