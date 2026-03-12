module Main where

import GHC
import GHC.Driver.Monad
import GHC.Driver.Session
import System.Environment
import GHC.Driver.Env.Types
import GHC.Profiling
import System.Mem
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main :: IO ()
main = do
    libdir:ghWayFlags <- getArgs
    runGhc (Just libdir) $ do
      initGhcM $ ["LinkableSpaceTest.hs", "-v0", "-package-db", "bytecode.package.conf", "-package-id", "testpkg-1.2.3.4-XXX", "-fprefer-byte-code" ] ++ ghWayFlags


initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    session <- getSession
    df1 <- getSessionDynFlags
    let cmdOpts = ["-fforce-recomp"] ++ xs
    (df2, leftovers, _) <- parseDynamicFlags (hsc_logger session) df1 (map noLoc cmdOpts)
    setSessionDynFlags df2
    ghcUnitId <- case lookup "Project Unit Id" (compilerInfo df2) of
                    Nothing -> fail "failed to find ghc's unit-id in the compiler info"
                    Just ghcUnitId -> pure ghcUnitId
    ts <- mapM (\s -> guessTarget s Nothing Nothing) $ map unLoc leftovers
    setTargets ts
    _ <- load LoadAllTargets
    liftIO $ do
      requestHeapCensus
      performGC
      putStrLn "### Heap Census"
      putStrLn "No UnlinkedBCO or DotGBC is alive after loading"
      recordCensus ghcUnitId
    return ()

data ConstrCensus
  = ConstrLinkable
  | ConstrUnlinkedBCO
  | ConstrDotGBC
  deriving (Show, Ord, Eq, Enum, Bounded)

printCensusData :: Map ConstrCensus Int -> IO ()
printCensusData dat = do
  mapM_ (putStrLn . showCensusData) [minBound .. maxBound]
  where
    showCensusData k =
      unwords
        [ show k ++ ":"
        , case Map.lookup k dat of
            Just v
              | v > 1000 -> "More than 1000 values"
              | v > 500 -> "More than 500 values"
              | v > 100 -> "More than 100 values"
              | v > 0 -> "More than 0 values"
              | otherwise -> error $ "Invalid value for " ++ show k ++ " " ++ show v
            Nothing ->
              "0"
        ]

recordCensus :: String -> IO ()
recordCensus ghcUnitId = do
  content <- lines <$> readFile "linkable-space.hp"
  let census = Maybe.mapMaybe (\ l ->
            isLinkable ghcUnitId l
        <|> isDotGBC ghcUnitId l
        <|> isUnlinkedBCO ghcUnitId l) content
  printCensusData $ Map.fromList census

isLinkable, isDotGBC, isUnlinkedBCO
  :: String -> String -> Maybe (ConstrCensus, Int)

isLinkable ghcUnitId l = do
  val <- getCensusValueFor (ghcUnitId <> ":GHC.Linker.Types.Linkable") l
  Just (ConstrLinkable, val)

isDotGBC ghcUnitId l = do
  val <- getCensusValueFor (ghcUnitId <> ":GHC.Linker.Types.DotGBC") l
  Just (ConstrDotGBC, val)

isUnlinkedBCO ghcUnitId l = do
  val <- getCensusValueFor (ghcUnitId <> ":GHC.ByteCode.Types.UnlinkedBCO") l
  Just (ConstrUnlinkedBCO, val)

getCensusValueFor :: String -> String -> Maybe Int
getCensusValueFor linePrefix l = do
  guard (List.isPrefixOf linePrefix l)
  case List.unsnoc (words l) of
    Nothing -> error "input is unexpectedly empty"
    Just (_, lst) -> Just $ read lst
