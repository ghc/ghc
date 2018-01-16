-- | Tests for detecting space leaks in the dependency solver.
module UnitTests.Distribution.Solver.Modular.MemoryUsage (tests) where

import Test.Tasty (TestTree)

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils

tests :: [TestTree]
tests = [
      runTest $ basicTest "basic space leak test"
    , runTest $ flagsTest "package with many flags"
    , runTest $ issue2899 "issue #2899"
    ]

-- | This test solves for n packages that each have two versions. There is no
-- solution, because the nth package depends on another package that doesn't fit
-- its version constraint. Backjumping is disabled, so the solver must explore a
-- search tree of size 2^n. It should fail if memory usage is proportional to
-- the size of the tree.
basicTest :: String -> SolverTest
basicTest name =
    disableBackjumping $ mkTest pkgs name ["target"] anySolverFailure
  where
    n :: Int
    n = 18

    pkgs :: ExampleDb
    pkgs = map Right $
           [ exAv "target" 1 [ExAny $ pkgName 1]]
        ++ [ exAv (pkgName i) v [ExRange (pkgName $ i + 1) 2 4]
           | i <- [1..n], v <- [2, 3]]
        ++ [exAv (pkgName $ n + 1) 1 []]

    pkgName :: Int -> ExamplePkgName
    pkgName x = "pkg-" ++ show x

-- | This test is similar to 'basicTest', except that it has one package with n
-- flags, flag-1 through flag-n. The solver assigns flags in order, so it
-- doesn't discover the unknown dependencies under flag-n until it has assigned
-- all of the flags. It has to explore the whole search tree.
flagsTest :: String -> SolverTest
flagsTest name =
    disableBackjumping $
    goalOrder orderedFlags $ mkTest pkgs name ["pkg"] anySolverFailure
  where
    n :: Int
    n = 16

    pkgs :: ExampleDb
    pkgs = [Right $ exAv "pkg" 1 $
                [exFlagged (flagName n) [ExAny "unknown1"] [ExAny "unknown2"]]

                -- The remaining flags have no effect:
             ++ [exFlagged (flagName i) [] [] | i <- [1..n - 1]]
           ]

    flagName :: Int -> ExampleFlagName
    flagName x = "flag-" ++ show x

    orderedFlags :: [ExampleVar]
    orderedFlags = [F None "pkg" (flagName i) | i <- [1..n]]

-- | Test for a space leak caused by sharing of search trees under packages with
-- link choices (issue #2899).
--
-- The goal order is fixed so that the solver chooses setup-dep and then
-- target-setup.setup-dep at the top of the search tree. target-setup.setup-dep
-- has two choices: link to setup-dep, and don't link to setup-dep. setup-dep
-- has a long chain of dependencies (pkg-1 through pkg-n). However, pkg-n
-- depends on pkg-n+1, which doesn't exist, so there is no solution. Since each
-- dependency has two versions, the solver must try 2^n combinations when
-- backjumping is disabled. These combinations create large search trees under
-- each of the two choices for target-setup.setup-dep. Although the choice to
-- not link is disallowed by the Single Instance Restriction, the solver doesn't
-- know that until it has explored (and evaluated) the whole tree under the
-- choice to link. If the two trees are shared, memory usage spikes.
issue2899 :: String -> SolverTest
issue2899 name =
    disableBackjumping $
    goalOrder goals $ mkTest pkgs name ["target"] anySolverFailure
  where
    n :: Int
    n = 16

    pkgs :: ExampleDb
    pkgs = map Right $
           [ exAv "target" 1 [ExAny "setup-dep"] `withSetupDeps` [ExAny "setup-dep"]
           , exAv "setup-dep" 1 [ExAny $ pkgName 1]]
        ++ [ exAv (pkgName i) v [ExAny $ pkgName (i + 1)]
           | i <- [1..n], v <- [1, 2]]

    pkgName :: Int -> ExamplePkgName
    pkgName x = "pkg-" ++ show x

    goals :: [ExampleVar]
    goals = [P None "setup-dep", P (Setup "target") "setup-dep"]
