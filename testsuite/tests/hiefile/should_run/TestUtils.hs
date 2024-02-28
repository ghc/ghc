module TestUtils
  ( explainEv
  , readTestHie
  , render
  , text
  , SDoc
  , DynFlags
  , module GHC.Iface.Ext.Types
  , module GHC.Iface.Ext.Utils
  ) where

import System.Environment
import Data.List (sort)
import Data.Tree
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name
import GHC.Utils.Outputable                 ( Outputable, renderWithContext, ppr, defaultUserStyle, SDoc )
import qualified GHC.Utils.Outputable as O
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils

import GHC.Driver.Session
import GHC.SysTools

makeNc :: IO NameCache
makeNc = initNameCache 'z' []

dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings

readTestHie :: FilePath -> IO (DynFlags, HieFile)
readTestHie fp = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  hfr <- readHieFile nc fp
  pure (df, hie_file_result hfr)

render :: Outputable a => DynFlags -> a -> String
render df = renderWithContext (initSDocContext df defaultUserStyle) . ppr

text :: String -> SDoc
text = O.text -- SDoc-only version

explainEv :: DynFlags -> HieFile -> RefMap Int -> (Int,Int) -> IO ()
explainEv df hf refmap point = do
  putStrLn $ replicate 26 '='
  putStrLn $ "At point " ++ show point ++ ", we found:"
  putStrLn $ replicate 26 '='
  putStr $ drawForest ptrees
  where
    trees = sort $ getEvidenceTreesAtPoint hf refmap point

    ptrees = fmap (pprint . fmap expandType) <$> trees

    expandType = text . renderHieType df .
      flip recoverFullType (hie_types hf)

    pretty = unlines . (++["└"]) . ("┌":) . map ("│ "++) . lines

    pprint = pretty . render df
