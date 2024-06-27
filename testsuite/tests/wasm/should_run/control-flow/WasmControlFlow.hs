module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Environment ( getArgs )
import System.Exit

import GHC hiding (Stmt, Match)
import GHC.Cmm hiding (succ)
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dominators
import GHC.Cmm.Reducibility
import GHC.Cmm.Switch.Implement
import GHC.Driver.Session
import GHC.Platform
import GHC.Types.Unique.DSM
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

main :: IO ()
main = do
    libdir : _ : files <- getArgs
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

allTests :: Platform -> (FilePath, [CmmGroup]) -> IO ExitCode
allTests platform (path, groups) =
  foldl combineExits exitZero <$>
  sequence [test platform (path, groups) | test <- tests]

tests :: [Platform -> (FilePath, [CmmGroup]) -> IO ExitCode]
tests = [reducibilityTest, splittingTest, translationTest]

reducibilityTest, splittingTest, translationTest
   :: Platform -> (FilePath, [CmmGroup]) -> IO ExitCode




----------------------------------------------------------------

-- | Counts the number of reducible and irreducible CFGs in each group

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

----------------------------------------------------------------

-- Convert each input graph to a reducible graph via node splitting,
-- run control-flow--path tests to confirm they behave the same.
-- Run similar tests that compare each graph with a mutilated version,
-- to confirm that the tests do in fact detect when graphs are different.

splittingTest platform (path, groups) = do
  reductions <- catMaybes <$> runGrouped testNodeSplitting platform groups
  mutilations <- runGrouped (return . testGraphMutilation path) platform groups
  codes <- liftM2 (++) (mapM (analyze "node splitting" path isIdentical) reductions)
                       (mapM (analyze "mutilation" path isDifferent) mutilations)
  return $ foldl combineExits exitZero codes

testNodeSplitting :: CmmGraph -> IO (Maybe Outcome)
testNodeSplitting original_graph = do
  reducible_graph <- fmap gwd_graph $ runUniqDSM $
                     asReducible $ graphWithDominators original_graph
  return $ case reducibility (graphWithDominators original_graph) of
    Reducible -> Nothing
    Irreducible ->
      Just $
      compareWithEntropy (runcfg original_graph) (runcfg reducible_graph) $
      cfgEntropy reducible_graph

testGraphMutilation :: FilePath -> CmmGraph -> Outcome
testGraphMutilation path graph =
  compareWithEntropy (runcfg graph) (runcfg $ mutilate path graph) $ cfgEntropy graph

-- | Changes the graph's entry point to one of the entry point's successors.
-- Panics if the input graph has only one block.
mutilate :: FilePath -> CmmGraph -> CmmGraph
mutilate path g =
    case filter (/= entry_label) $ successors entry_block of
      (lbl:_) -> CmmGraph lbl (g_graph g)
      [] -> error $ "cannot mutilate control-flow graph in file " ++ path
 where entry_label = g_entry g
       entry_block = mapFindWithDefault (error "no entry block") entry_label $ graphMap g

----------------------------------------------------------------

-- Translate each input graph to WebAssembly, then run
-- control-flow--path tests to confirm the translation behaves the
-- same as the original.

translationTest platform (path, groups) = do
  txs <- runGrouped (testTranslation platform) platform groups
  codes <- mapM (analyze "WebAssembly translation" path isIdentical) txs
  return $ foldl combineExits exitZero codes

testTranslation :: Platform -> CmmGraph -> IO Outcome
testTranslation platform big_switch_graph = do
  real_graph <- runUniqDSM $ cmmImplementSwitchPlans platform big_switch_graph
  reducible_graph <- fmap gwd_graph $ runUniqDSM $
                     asReducible $ graphWithDominators real_graph
  let us = initDUniqSupply 'w' 0
  let (wasm, _) = runUniqueDSM us $ structuredControl platform ((pure .) . expr) ((pure .) . stmt) reducible_graph
  return $ compareWithEntropy (runcfg real_graph) (runwasm wasm) $
           cfgEntropy reducible_graph

----------------------------------------------------------------

-- Outcomes of comparisons

data Outcome = Identical { npaths :: Int }
             | Different { different :: [(Trace, Trace)], nsame :: Int }
type Trace = [Event Stmt Expr]

isDifferent, isIdentical :: Outcome -> Bool

isDifferent (Different {}) = True
isDifferent _ = False

isIdentical (Identical {}) = True
isIdentical _ = False

----------------------------------------------------------------

-- Comparisons of execution paths

type Entropy = [[Bool]]

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

----------------------------------------------------------------

-- Other test-running infrastructure

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

runcfg :: CmmGraph -> BitConsumer Stmt Expr ()
runcfg = evalGraph stmt expr

runwasm :: WasmControl Stmt Expr pre post -> BitConsumer Stmt Expr ()
runwasm = evalWasm

runUniqDSM :: UniqDSM a -> IO a
runUniqDSM m = do
  let us = initDUniqSupply 'g' 0
  return (fst $ runUniqueDSM us m)

----------------------------------------------------------------

-- ExitCode as monoid

combineExits :: ExitCode -> ExitCode -> ExitCode
exitZero :: ExitCode

exitZero = ExitSuccess
combineExits ExitSuccess e = e
combineExits e _ = e
