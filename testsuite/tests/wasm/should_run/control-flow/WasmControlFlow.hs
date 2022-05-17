{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (nub)
import Data.Maybe
import System.Environment ( getArgs )
import System.Exit

import GHC hiding (Stmt, Match)
import GHC.Cmm hiding (succ)
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dominators
import GHC.Cmm.Reducibility
import GHC.Cmm.Switch.Implement
import GHC.Driver.Session
import GHC.Platform
import GHC.Types.Unique.Supply
import GHC.Wasm.ControlFlow
import GHC.Wasm.ControlFlow.FromCmm

import qualified GHC.LanguageExtensions as LangExt

import ActionsAndObservations
import BitConsumer
import CmmPaths
import ControlTestMonad
import EntropyTransducer
import LoadCmmGroup
import RunCmm
import RunWasm

data TestMode = ReducibilityTest | SplittingTest | TranslationTest

testMode :: String -> TestMode
testMode s = case s of "-r" -> ReducibilityTest
                       "-s" -> SplittingTest
                       "-t" -> TranslationTest
                       _ -> error "mode should be -r or -t"

main :: IO ()
main = do
    libdir : modeString : files <- getArgs
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        raw_dflags <- getSessionDynFlags
        let dflags = raw_dflags `xopt_set` LangExt.MagicHash
                         `xopt_set` LangExt.StandaloneKindSignatures
                         `xopt_set` LangExt.UnliftedDatatypes
                         `xopt_set` LangExt.DataKinds
        setSessionDynFlags dflags
        groups <- mapM loadPath files
        liftIO $ do
          codes <- mapM (allTests $ targetPlatform dflags) (zip files groups)
          exitWith $ foldl combineExits exitZero codes
{-
            labeled_groups = [(path, group)
                                  | (path, groups) <- zip files groups, group <- groups]
            run :: (CmmGraph -> IO a) -> IO [(FilePath, a)]
            run f = concat <$>
                    mapM (fmap unroll . concatMapGraphs platform (const f)) labeled_groups
            runGrouped :: (CmmGraph -> IO a) -> IO [(FilePath, [a])]
            runGrouped f =
                mergeLike <$> mapM (concatMapGraphs platform (const f)) labeled_groups
            unroll :: (FilePath, [a]) -> [(FilePath, a)]
            unroll (path, as) = [(path, a) | a <- as]
            mergeLike :: [(FilePath, [a])] -> [(FilePath, [a])]
            mergeLike pairs =
                [(path, matching path) | path <- nub (map fst pairs)]
              where matching path = [a | (path', as) <- pairs, path == path', a <- as]
        liftIO $ case testMode modeString of
          ReducibilityTest -> do
            analyses <- runGrouped (return . reducibility . graphWithDominators)
            let dump (path, results) = do
                    putStr $ path ++ ": "
                    case (number (== Reducible), number (== Irreducible)) of
                      (0, 0) -> putStrLn $ "no code"
                      (0, n) -> putStrLn $ show n ++ " irreducible"
                      (n, 0) -> putStrLn $ show n ++ " reducible"
                      (r, i) -> putStrLn $ show r ++ " reducible, " ++ show i ++ " irreducible"
                   where number p = length $ filter p $ results
            mapM_ dump analyses
          SplittingTest -> do
            reductions <- run testNodeSplitting
            mutilations <- run (return . testGraphMutilation)
            results <- liftM2 (++) (mapM (analyze isIdentical) reductions)
                                   (mapM (analyze isDifferent) mutilations)
            exitWith $ foldl combineExits exitZero results
-}

allTests :: Platform -> (FilePath, [CmmGroup]) -> IO ExitCode
allTests platform (path, groups) =
  foldl combineExits exitZero <$>
  sequence [test platform (path, groups) | test <- tests]

tests :: [Platform -> (FilePath, [CmmGroup]) -> IO ExitCode]
tests = [reducibilityTest, splittingTest, translationTest]

reducibilityTest, splittingTest, translationTest :: Platform -> (FilePath, [CmmGroup]) -> IO ExitCode

reducibilityTest platform (path, groups) = do
  analyses <- runGrouped (return . reducibility . graphWithDominators) platform groups
  let dump results = do
          putStr $ path ++ ": "
          case (number (== Reducible), number (== Irreducible)) of
            (0, 0) -> putStrLn $ "no code"
            (1, 0) -> putStrLn $ "reducible"
            (0, 1) -> putStrLn $ "irreducible"
            (0, n) -> putStrLn $ show n ++ " irreducible"
            (n, 0) -> putStrLn $ show n ++ " reducible"
            (r, i) -> putStrLn $ show r ++ " reducible, " ++ show i ++ " irreducible"
         where number p = length $ filter p $ results
  dump analyses
  return exitZero

splittingTest platform (path, groups) = do
  reductions <- catMaybes <$> runGrouped testNodeSplitting platform groups
  mutilations <- runGrouped (return . testGraphMutilation) platform groups
  codes <- liftM2 (++) (mapM (analyze "node splitting" path isIdentical) reductions)
                         (mapM (analyze "mutilation" path isDifferent) mutilations)
  return $ foldl combineExits exitZero codes

translationTest platform (path, groups) = do
  txs <- runGrouped (testTranslation platform) platform groups
  codes <- mapM (analyze "WebAssembly translation" path isIdentical) txs
  return $ foldl combineExits exitZero codes




runGrouped :: (CmmGraph -> IO a) -> Platform -> [CmmGroup] -> IO [a]
runGrouped f platform groups = concat <$> mapM (concatMapGraphs platform (const f)) groups

concatMapGraphs :: Monad m
             => Platform
             -> (Platform -> CmmGraph -> m a)
             -> CmmGroup
             -> m [a]
concatMapGraphs platform f group =
    catMaybes <$> mapM (decl . cmmCfgOptsProc False) group
  where decl (CmmData {}) = return Nothing
        decl (CmmProc _h _entry _registers graph) =
            do a <- f platform graph
               return $ Just a

count :: [a] -> String -> String
count xs thing = case length xs of
                   1 -> "1 " ++ thing
                   n -> show n ++ " " ++ thing ++ "s"


data Outcome = Identical { npaths :: Int }
             | Different { different :: [(Trace, Trace)], nsame :: Int }
type Trace = [Event Stmt Expr]

isDifferent, isIdentical :: Outcome -> Bool

isDifferent (Different {}) = True
isDifferent _ = False

isIdentical (Identical {}) = True
isIdentical _ = False

testNodeSplitting :: CmmGraph -> IO (Maybe Outcome)
testNodeSplitting original_graph = do
  reducible_graph <- fmap gwd_graph $ runUniqSM $
                     asReducible $ graphWithDominators original_graph
  return $ case reducibility (graphWithDominators original_graph) of
    Reducible -> Nothing
    Irreducible ->
      Just $
      compareWithEntropy (runcfg original_graph) (runcfg reducible_graph) $
      cfgEntropy reducible_graph

testGraphMutilation :: CmmGraph -> Outcome
testGraphMutilation graph =
  compareWithEntropy (runcfg graph) (runcfg $ mutilate graph) $ cfgEntropy graph

testTranslation :: Platform -> CmmGraph -> IO Outcome
testTranslation platform big_switch_graph = do
  real_graph <- runUniqSM $ cmmImplementSwitchPlans platform big_switch_graph
  reducible_graph <- fmap gwd_graph $ runUniqSM $
                     asReducible $ graphWithDominators real_graph
  let wasm = structuredControl platform expr stmt reducible_graph
  return $ compareWithEntropy (runcfg real_graph) (runwasm wasm) $
           cfgEntropy reducible_graph


runcfg :: CmmGraph -> BitConsumer Stmt Expr ()
runcfg = evalGraph stmt expr

runwasm :: WasmControl Stmt Expr -> BitConsumer Stmt Expr ()
runwasm = evalWasm

runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'g'
  return (initUs_ us m)

type Entropy = [[Bool]]

----------------------------------------------------------------

mutilate :: CmmGraph -> CmmGraph
mutilate g =
    case filter (/= entry_label) $ successors entry_block of
      (lbl:_) -> CmmGraph lbl (g_graph g)
      [] -> error "cannot mutilate control-flow graph"
 where entry_label = g_entry g
       entry_block = mapFindWithDefault (error "no entry block") entry_label $ graphMap g

compareWithEntropy :: BitConsumer Stmt Expr ()
                   -> BitConsumer Stmt Expr ()
                   -> Entropy
                   -> Outcome
compareWithEntropy a b bit_streams =
   foldl add (Identical 0) $ map (compareRuns a b) bit_streams
  where add (Identical k) Match = Identical (succ k)
        add (Different ts k) Match = Different ts (succ k)
        add (Identical k) (NoMatch pair) = Different [pair] k
        add (Different ts k) (NoMatch pair) = Different (pair:ts) k

data SingleComparison = Match
                      | NoMatch (Trace, Trace)

compareRuns :: BitConsumer Stmt Expr ()
            -> BitConsumer Stmt Expr ()
            -> [Bool]
            -> SingleComparison
compareRuns a b bits =
    if and $ zipWith (==) aEvents bEvents then
        Match
    else
        NoMatch (aEvents, bEvents)
 where aEvents = pastEvents $ runWithBits a bits
       bEvents = pastEvents $ runWithBits b bits


cfgEntropy :: CmmGraph -> Entropy
cfgEntropy = map traceBits . cmmPaths

unimp :: String -> a
unimp s = error $ s ++ " not implemented"

----------------------------------------------------------------

combineExits :: ExitCode -> ExitCode -> ExitCode
exitZero :: ExitCode

exitZero = ExitSuccess
combineExits ExitSuccess e = e
combineExits e _ = e


analyze :: String -> FilePath -> (Outcome -> Bool) -> Outcome -> IO ExitCode
analyze what path isGood outcome = do
  putStrLn $ display $ path ++ ", " ++ what ++ ": " ++ case outcome of
    Identical n -> show n ++ " paths are identical"
    Different diffs nsame ->
       if nsame == 0 then
           "all " ++ show (length diffs) ++ " paths are different"
       else
           show (length diffs) ++ " of " ++ show (length diffs + nsame) ++ " paths are different"
  if isGood outcome then
      return ExitSuccess
  else
      return $ ExitFailure 1
 where display s = if isGood outcome then s ++ ", as expected"
                   else "(FAULT!) " ++ s
