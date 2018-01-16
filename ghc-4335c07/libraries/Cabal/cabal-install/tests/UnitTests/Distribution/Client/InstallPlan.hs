{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}
module UnitTests.Distribution.Client.InstallPlan (tests) where

import           Distribution.Package
import           Distribution.Version
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.InstallPlan (GenericInstallPlan, IsUnit)
import qualified Distribution.Compat.Graph as Graph
import           Distribution.Compat.Graph (IsNode(..))
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.PackageFixedDeps
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Client.Types
import           Distribution.Client.JobControl

import Data.Graph
import Data.Array hiding (index)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IORef
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Random
import Test.QuickCheck

import Test.Tasty
import Test.Tasty.QuickCheck


tests :: [TestTree]
tests =
  [ testProperty "reverseTopologicalOrder" prop_reverseTopologicalOrder
  , testProperty "executionOrder"          prop_executionOrder
  , testProperty "execute serial"          prop_execute_serial
  , testProperty "execute parallel"        prop_execute_parallel
  , testProperty "execute/executionOrder"  prop_execute_vs_executionOrder
  ]

prop_reverseTopologicalOrder :: TestInstallPlan -> Bool
prop_reverseTopologicalOrder (TestInstallPlan plan graph toVertex _) =
    isReverseTopologicalOrder
      graph
      (map (toVertex . installedUnitId)
           (InstallPlan.reverseTopologicalOrder plan))

-- | @executionOrder@ is in reverse topological order
prop_executionOrder :: TestInstallPlan -> Bool
prop_executionOrder (TestInstallPlan plan graph toVertex _) =
    isReversePartialTopologicalOrder graph (map toVertex pkgids)
 && allConfiguredPackages plan == Set.fromList pkgids
  where
    pkgids = map installedUnitId (InstallPlan.executionOrder plan)

-- | @execute@ is in reverse topological order
prop_execute_serial :: TestInstallPlan -> Property
prop_execute_serial tplan@(TestInstallPlan plan graph toVertex _) =
    ioProperty $ do
      jobCtl <- newSerialJobControl
      pkgids <- executeTestInstallPlan jobCtl tplan (\_ -> return ())
      return $ isReversePartialTopologicalOrder graph (map toVertex pkgids)
            && allConfiguredPackages plan == Set.fromList pkgids

prop_execute_parallel :: Positive (Small Int) -> TestInstallPlan -> Property
prop_execute_parallel (Positive (Small maxJobLimit))
                      tplan@(TestInstallPlan plan graph toVertex _) =
    ioProperty $ do
      jobCtl <- newParallelJobControl maxJobLimit
      pkgids <- executeTestInstallPlan jobCtl tplan $ \_ -> do
                  delay <- randomRIO (0,1000)
                  threadDelay delay
      return $ isReversePartialTopologicalOrder graph (map toVertex pkgids)
            && allConfiguredPackages plan == Set.fromList pkgids

-- | return the packages that are visited by execute, in order.
executeTestInstallPlan :: JobControl IO (UnitId, Either () ())
                       -> TestInstallPlan
                       -> (TestPkg -> IO ())
                       -> IO [UnitId]
executeTestInstallPlan jobCtl (TestInstallPlan plan _ _ _) visit = do
    resultsRef <- newIORef []
    _ <- InstallPlan.execute jobCtl False (const ())
                             plan $ \(ReadyPackage pkg) -> do
           visit pkg
           atomicModifyIORef resultsRef $ \pkgs -> (installedUnitId pkg:pkgs, ())
           return (Right ())
    fmap reverse (readIORef resultsRef)

-- | @execute@ visits the packages in the same order as @executionOrder@
prop_execute_vs_executionOrder :: TestInstallPlan -> Property
prop_execute_vs_executionOrder tplan@(TestInstallPlan plan _ _ _) =
    ioProperty $ do
      jobCtl <- newSerialJobControl
      pkgids <- executeTestInstallPlan jobCtl tplan (\_ -> return ())
      let pkgids' = map installedUnitId (InstallPlan.executionOrder plan)
      return (pkgids == pkgids')


--------------------------
-- Property helper utils
--

-- | A graph topological ordering is a linear ordering of its vertices such
-- that for every directed edge uv from vertex u to vertex v, u comes before v
-- in the ordering.
--
-- A reverse topological ordering is the swapped: for every directed edge uv
-- from vertex u to vertex v, v comes before u in the ordering.
--
isReverseTopologicalOrder :: Graph -> [Vertex] -> Bool
isReverseTopologicalOrder g vs =
    and [ ixs ! u > ixs ! v
        | let ixs = array (bounds g) (zip vs [0::Int ..])
        , (u,v) <- edges g ]

isReversePartialTopologicalOrder :: Graph -> [Vertex] -> Bool
isReversePartialTopologicalOrder g vs =
    and [ case (ixs ! u, ixs ! v) of
            (Just ixu, Just ixv) -> ixu > ixv
            _                    -> True
        | let ixs = array (bounds g)
                          (zip (range (bounds g)) (repeat Nothing) ++ 
                           zip vs (map Just [0::Int ..]))
        , (u,v) <- edges g ]

allConfiguredPackages :: HasUnitId srcpkg
                      => GenericInstallPlan ipkg srcpkg -> Set UnitId
allConfiguredPackages plan =
    Set.fromList
      [ installedUnitId pkg
      | InstallPlan.Configured pkg <- InstallPlan.toList plan ]


--------------------
-- Test generators
--

data TestInstallPlan = TestInstallPlan
                         (GenericInstallPlan TestPkg TestPkg)
                         Graph
                         (UnitId -> Vertex)
                         (Vertex -> UnitId)

instance Show TestInstallPlan where
  show (TestInstallPlan plan _ _ _) = InstallPlan.showInstallPlan plan

data TestPkg = TestPkg PackageId UnitId [UnitId]
  deriving (Eq, Show)

instance IsNode TestPkg where
  type Key TestPkg = UnitId
  nodeKey (TestPkg _ ipkgid _) = ipkgid
  nodeNeighbors (TestPkg _ _ deps) = deps


instance Package TestPkg where
  packageId (TestPkg pkgid _ _) = pkgid

instance HasUnitId TestPkg where
  installedUnitId (TestPkg _ ipkgid _) = ipkgid

instance PackageFixedDeps TestPkg where
  depends (TestPkg _ _ deps) = CD.singleton CD.ComponentLib deps

instance PackageInstalled TestPkg where
  installedDepends (TestPkg _ _ deps) = deps

instance Arbitrary TestInstallPlan where
  arbitrary = arbitraryTestInstallPlan

arbitraryTestInstallPlan :: Gen TestInstallPlan
arbitraryTestInstallPlan = do
    graph <- arbitraryAcyclicGraph
               (choose (2,5))
               (choose (1,5))
               0.3

    plan  <- arbitraryInstallPlan mkTestPkg mkTestPkg 0.5 graph

    let toVertexMap   = Map.fromList [ (mkUnitIdV v, v) | v <- vertices graph ]
        fromVertexMap = Map.fromList [ (v, mkUnitIdV v) | v <- vertices graph ]
        toVertex      = (toVertexMap   Map.!)
        fromVertex    = (fromVertexMap Map.!)

    return (TestInstallPlan plan graph toVertex fromVertex)
  where
    mkTestPkg pkgv depvs =
        return (TestPkg pkgid ipkgid deps)
      where
        pkgid  = mkPkgId pkgv
        ipkgid = mkUnitIdV pkgv
        deps   = map mkUnitIdV depvs
    mkUnitIdV = mkUnitId . show
    mkPkgId v = PackageIdentifier (mkPackageName ("pkg" ++ show v))
                                  (mkVersion [1])


-- | Generate a random 'InstallPlan' following the structure of an existing
-- 'Graph'.
--
-- It takes generators for installed and source packages and the chance that
-- each package is installed (for those packages with no prerequisites).
--
arbitraryInstallPlan :: (IsUnit ipkg,
                         IsUnit srcpkg)
                     => (Vertex -> [Vertex] -> Gen ipkg)
                     -> (Vertex -> [Vertex] -> Gen srcpkg)
                     -> Float
                     -> Graph
                     -> Gen (InstallPlan.GenericInstallPlan ipkg srcpkg)
arbitraryInstallPlan mkIPkg mkSrcPkg ipkgProportion graph = do

    (ipkgvs, srcpkgvs) <-
      fmap ((\(ipkgs, srcpkgs) -> (map fst ipkgs, map fst srcpkgs))
            . partition snd) $
      sequence
        [ do isipkg <- if isRoot then pick ipkgProportion
                                 else return False
             return (v, isipkg)
        | (v,n) <- assocs (outdegree graph)
        , let isRoot = n == 0 ]

    ipkgs   <- sequence
                 [ mkIPkg pkgv depvs
                 | pkgv <- ipkgvs
                 , let depvs  = graph ! pkgv
                 ]
    srcpkgs <- sequence
                 [ mkSrcPkg pkgv depvs
                 | pkgv <- srcpkgvs
                 , let depvs  = graph ! pkgv
                 ]
    let index = Graph.fromDistinctList
                   (map InstallPlan.PreExisting ipkgs
                 ++ map InstallPlan.Configured  srcpkgs)
    return $ InstallPlan.new (IndependentGoals False) index


-- | Generate a random directed acyclic graph, based on the algorithm presented
-- here <http://stackoverflow.com/questions/12790337/generating-a-random-dag>
--
-- It generates a DAG based on ranks of nodes. Nodes in each rank can only
-- have edges to nodes in subsequent ranks.
--
-- The generator is paramterised by a generator for the number of ranks and
-- the number of nodes within each rank. It is also paramterised by the
-- chance that each node in each rank will have an edge from each node in
-- each previous rank. Thus a higher chance will produce a more densely
-- connected graph.
--
arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

pick :: Float -> Gen Bool
pick chance = do
    p <- choose (0,1)
    return (p < chance)


--------------------------------
-- Inspecting generated graphs
--

{-
-- Handy util for checking the generated graphs look sensible
writeDotFile :: FilePath -> Graph -> IO ()
writeDotFile file = writeFile file . renderDotGraph

renderDotGraph :: Graph -> String
renderDotGraph graph =
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs]
    ++ map renderNode (vertices graph)
    ++ map renderEdge (edges graph)
    ++ [footer]
  )
  where
    renderNode n = "\t" ++ show n ++ " [label=\"" ++ show n ++  "\"];"

    renderEdge (n, n') = "\t" ++ show n ++ " -> " ++ show n' ++ "[];"


header, footer, graphDefaultAtribs, nodeDefaultAtribs, edgeDefaultAtribs :: String

header = "digraph packages {"
footer = "}"

graphDefaultAtribs = "\tgraph [fontsize=14, fontcolor=black, color=black];"
nodeDefaultAtribs  = "\tnode [label=\"\\N\", width=\"0.75\", shape=ellipse];"
edgeDefaultAtribs  = "\tedge [fontsize=10];"
-}
