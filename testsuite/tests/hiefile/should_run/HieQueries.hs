{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment

import NameCache
import SrcLoc
import UniqSupply
import Name

import HieBin
import HieTypes
import HieUtils

import DynFlags
import SysTools

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

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])

selectPoint :: HieFile -> (Int,Int) -> HieAST Int
selectPoint hf (sl,sc) = case M.toList (getAsts $ hie_asts hf) of
    [(fs,ast)] ->
      case selectSmallestContaining (sp fs) ast of
        Nothing -> error "point not found"
        Just ast' -> ast'
    _ -> error "map should only contain a single AST"
 where
   sloc fs = mkRealSrcLoc fs sl sc
   sp fs = mkRealSrcSpan (sloc fs) (sloc fs)

findEvidence :: NodeInfo a -> (Name, IdentifierDetails a)
findEvidence ni =
    case find go xs of
      Just (Right n,x) -> (n,x)
      _ -> error "couldn't find evidence var"
 where
   xs = M.toList $ nodeIdentifiers ni
   go (n,dets) = any isEvidenceContext (identInfo dets)

main = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  (hfr, nc') <- readHieFile nc "HieQueries.hie"
  let hf = hie_file_result hfr
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
  let (var,dets) = findEvidence $ nodeInfo $ selectPoint hf point
      Just typ = identType dets
  putStr $ "At " ++ show point ++ ", found evidence of type: "
  putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
  putStrLn ""
  let describeEvidenceVar :: Name -> IO ()
      describeEvidenceVar var =
        case M.lookup (Right var) refmap of
          Nothing -> error "evidence var not found in map"
          Just xs -> case find (any isEvidenceBind . identInfo . snd) xs of
            Nothing -> error "couldn't find evidence bind"
            Just (sp,dets) -> do
              let Just typ = identType dets
              forM_ (identInfo dets) $ \det -> case det of
                EvidenceVarBind EvSigBind _ _ -> do
                  putStr $ "Evidence from " ++ show sp ++ " of type: "
                  putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
                  putStrLn "Is bound by a signature\n"
                EvidenceVarBind (EvLetBind deps) _ _ -> do
                  putStr $ "Evidence from " ++ show sp ++ " of type: "
                  putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
                  putStrLn "Is bound by a let, depending on:\n"
                  mapM_ describeEvidenceVar (getEvBindDeps deps)
                EvidenceVarBind EvExternalBind _ _ -> do
                  putStr $ "Evidence of type: "
                  putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
                  putStrLn $ "bound by an instance at " ++ show (nameSrcSpan var)
                  putStrLn ""
                _ -> return ()
  describeEvidenceVar var
  return ()
