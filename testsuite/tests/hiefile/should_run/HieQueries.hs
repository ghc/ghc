{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name
import Data.Tree
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import Data.Maybe (fromJust)
import GHC.Driver.Session
import GHC.SysTools
import GHC.Utils.Outputable                 ( Outputable, renderWithStyle, ppr, defaultUserStyle, initSDocContext, text)
import qualified Data.Map as M
import Data.Foldable

class C a where
  f :: a -> Char

instance C Char where
  f x = x

instance C a => C [a] where
  f x = 'a'

foo :: C a => a -> Char
foo x = f [x]
--      ^ this is the point
point :: (Int,Int)
point = (31,9)

bar :: Show x => x -> String
bar x = show [(1,x,A)]
--      ^ this is the point'
point' :: (Int,Int)
point' = (37,9)

data A = A deriving Show

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings (LlvmConfig [] [])

main = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  hfr <- readHieFile (NCU (\f -> pure $ snd $ f nc)) "HieQueries.hie"
  let hf = hie_file_result hfr
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
  explainEv df hf refmap point
  explainEv df hf refmap point'
  return ()

explainEv :: DynFlags -> HieFile -> RefMap Int -> (Int,Int) -> IO ()
explainEv df hf refmap point = do
  putStrLn $ replicate 26 '='
  putStrLn $ "At point " ++ show point ++ ", we found:"
  putStrLn $ replicate 26 '='
  putStr $ drawForest ptrees
  where
    trees = getEvidenceTreesAtPoint hf refmap point

    ptrees = fmap (pprint . fmap expandType) <$> trees

    expandType = text . renderHieType df .
      flip recoverFullType (hie_types hf)

    pretty = unlines . (++["└"]) . ("┌":) . map ("│ "++) . lines

    pprint = pretty . renderWithStyle (initSDocContext df sty) . ppr
    sty = defaultUserStyle
