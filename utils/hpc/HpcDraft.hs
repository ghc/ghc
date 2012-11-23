module HpcDraft (draft_plugin) where

import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util

import HpcFlags

import qualified Data.Set as Set
import qualified Data.Map as Map
import HpcUtils
import Data.Tree

------------------------------------------------------------------------------
draft_options :: FlagOptSeq
draft_options
        = excludeOpt
        . includeOpt
        . srcDirOpt
        . hpcDirOpt
        . resetHpcDirsOpt
        . outputOpt

draft_plugin :: Plugin
draft_plugin = Plugin { name = "draft"
                       , usage = "[OPTION] .. <TIX_FILE>"
                       , options = draft_options
                       , summary = "Generate draft overlay that provides 100% coverage"
                       , implementation = draft_main
                       , init_flags = default_flags
                       , final_flags = default_final_flags
                       }

------------------------------------------------------------------------------

draft_main :: Flags -> [String] -> IO ()
draft_main _        []              = error "draft_main: unhandled case: []"
draft_main hpcflags (progName:mods) = do
  let hpcflags1 = hpcflags
                { includeMods = Set.fromList mods
                                   `Set.union`
                                includeMods hpcflags }
  let prog = getTixFileName $ progName
  tix <- readTix prog
  case tix of
    Just (Tix tickCounts) -> do
        outs <- sequence
                      [ makeDraft hpcflags1 tixModule
                      | tixModule@(TixModule m _ _ _) <- tickCounts
                      , allowModule hpcflags1 m
                      ]
        case outputFile hpcflags1 of
         "-" -> putStrLn (unlines outs)
         out -> writeFile out (unlines outs)
    Nothing -> hpcError draft_plugin $ "unable to find tix file for:" ++ progName


makeDraft :: Flags -> TixModule -> IO String
makeDraft hpcflags tix = do
  let modu = tixModuleName tix
      tixs = tixModuleTixs tix

  (Mix filepath _ _ _ entries) <- readMixWithFlags hpcflags (Right tix)

  let forest = createMixEntryDom
              [ (srcspan,(box,v > 0))
              | ((srcspan,box),v) <- zip entries tixs
              ]

--  let show' (span,stuff) = show (span,stuff,grabHpcPos hsMap span)
--  putStrLn $ drawForest $ map (fmap show) $ forest

  let non_ticked = findNotTickedFromList forest

  hs  <- readFileFromPath (hpcError draft_plugin) filepath (srcDirs hpcflags)

  let hsMap :: Map.Map Int String
      hsMap = Map.fromList (zip [1..] $ lines hs)

  let quoteString = show

  let firstLine pos = case fromHpcPos pos of
                        (ln,_,_,_) -> ln


  let showPleaseTick :: Int -> PleaseTick -> String
      showPleaseTick d (TickFun str pos) =
                     spaces d ++ "tick function \"" ++ last str ++ "\" "
                              ++ "on line " ++ show (firstLine pos) ++ ";"
      showPleaseTick d (TickExp pos) =
                     spaces d ++ "tick "
                              ++ if '\n' `elem` txt
                                 then "at position " ++ show pos ++ ";"
                                 else quoteString txt ++ " "  ++ "on line " ++ show (firstLine pos) ++ ";"

          where
                  txt = grabHpcPos hsMap pos

      showPleaseTick d (TickInside [str] _ pleases) =
                     spaces d ++ "inside \"" ++ str ++ "\" {\n" ++
                     showPleaseTicks (d + 2) pleases ++
                     spaces d ++ "}"

      showPleaseTick _ (TickInside _ _ _)
          = error "showPleaseTick: Unhandled case TickInside"

      showPleaseTicks d pleases = unlines (map (showPleaseTick d) pleases)

      spaces d = take d (repeat ' ')

  return $ "module " ++ show (fixPackageSuffix modu) ++ " {\n" ++
         showPleaseTicks 2 non_ticked ++ "}"

fixPackageSuffix :: String -> String
fixPackageSuffix modu = case span (/= '/') modu of
                        (before,'/':after) -> before ++ ":" ++ after
                        _                  -> modu

data PleaseTick
   = TickFun [String] HpcPos
   | TickExp HpcPos
   | TickInside [String] HpcPos [PleaseTick]
    deriving Show

mkTickInside :: [String] -> HpcPos -> [PleaseTick]
             -> [PleaseTick] -> [PleaseTick]
mkTickInside _ _ []        = id
mkTickInside nm pos inside = (TickInside nm pos inside :)

findNotTickedFromTree :: MixEntryDom [(BoxLabel,Bool)] -> [PleaseTick]
findNotTickedFromTree (Node (pos,(ExpBox {},False):_) _) = [TickExp pos]
findNotTickedFromTree (Node (pos,(TopLevelBox nm,False):_) _)
  = [ TickFun nm pos ]
findNotTickedFromTree (Node (pos,(LocalBox nm,False):_) _)
  = [ TickFun nm pos ]
findNotTickedFromTree (Node (pos,(TopLevelBox nm,True):_) children)
  = mkTickInside nm pos (findNotTickedFromList children) []
findNotTickedFromTree (Node (pos,_:others) children) =
                      findNotTickedFromTree (Node (pos,others) children)
findNotTickedFromTree (Node (_, []) children) = findNotTickedFromList children

findNotTickedFromList :: [MixEntryDom [(BoxLabel,Bool)]] -> [PleaseTick]
findNotTickedFromList = concatMap findNotTickedFromTree

