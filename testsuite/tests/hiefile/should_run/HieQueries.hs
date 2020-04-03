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
import Maybes
import GHC.Driver.Session
import SysTools
import Outputable                 ( Outputable, renderWithStyle, ppr, defaultUserStyle, initSDocContext )
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
  (hfr, nc') <- readHieFile nc "HieQueries.hie"
  let hf = hie_file_result hfr
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
  explainEv df hf refmap point
  explainEv df hf refmap point'
  return ()

type RefMap = M.Map Identifier [(Span, IdentifierDetails Int)]

explainEv :: DynFlags -> HieFile -> RefMap -> (Int,Int) -> IO ()
explainEv df hf refmap point = do
  let (var,dets) = expectJust "couldn't find evidence var" $
        findEvidence $ nodeInfo $
          expectJust "point not found" $ selectPoint hf point
      Just typ = identType dets
  putStr $ "At " ++ pprint point ++ ", found evidence of type: "
  putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
  let describeEvidenceVar :: Name -> IO ()
      describeEvidenceVar = describeEvidenceVar' 0
      describeEvidenceVar' :: Int -> Name -> IO ()
      describeEvidenceVar' lvl var =
        case M.lookup (Right var) refmap of
          Nothing -> error "evidence var not found in map"
          Just xs -> case find (any isEvidenceBind . identInfo . snd) xs of
            Nothing -> error "couldn't find evidence bind"
            Just (sp,dets) -> do
              let Just typ = identType dets
              print [(replicate (69-(lvl*2)) '-')]
              forM_ (identInfo dets) $ \det -> case det of
                EvidenceVarBind EvSigBind _ msrc -> do
                  print [ "Evidence from", pprint sp, "of type:"
                        , renderHieType df $ recoverFullType typ (hie_types hf)]
                  print ["Bound by a signature for the value at:",pprint msrc]
                EvidenceVarBind (EvLetBind deps) _ _ -> do
                  print [ "Evidence from", pprint sp, "of type:"
                        , renderHieType df $ recoverFullType typ (hie_types hf)]
                  print ["Bound by a let, depending on:"]
                  mapM_ (describeEvidenceVar' $ lvl+1) (getEvBindDeps deps)
                EvidenceVarBind EvExternalBind _ _ -> do
                  print [ "Evidence of type:"
                        , renderHieType df $ recoverFullType typ (hie_types hf)]
                  print ["Bound by an instance at", pprint (nameSrcSpan var)]
                  print ["From the module:", pprint (nameModule var)]
                _ -> return ()
        where
          print xs = putStrLn $ concat (replicate lvl "  ") ++ ('|':unwords xs)
  describeEvidenceVar var
  putStrLn $ replicate 70 '='
  where
    pprint :: Outputable a => a -> String
    pprint = renderWithStyle (initSDocContext df sty) . ppr
      where sty = defaultUserStyle df


