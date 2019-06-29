{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils

import GHC.Driver.Session
import GHC.SysTools

import qualified Data.Map as M
import Data.Foldable

foo :: Maybe Char -> Char
foo Nothing = 'a'
--  1^
foo (Just c) | c == 'a' =  c
-- 2^    3^
foo x = 'b'
-- 4^

p1,p2,p3,p4 :: (Int,Int)
p1 = (22,6)
p2 = (24,5)
p3 = (24,11)
p4 = (26,5)

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings (LlvmConfig [] [])

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  hfr <- readHieFile (NCU (\f -> pure $ snd $ f nc)) "PatTypes.hie"
  let hf = hie_file_result hfr
  forM_ [p1,p2,p3,p4] $ \point -> do
    putStr $ "At " ++ show point ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
