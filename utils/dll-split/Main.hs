
{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do args <- getArgs
          case args of
              [depfile, startModule, reachableModules] ->
                  doit depfile
                       (Module startModule)
                       (Set.fromList $ map Module $ words reachableModules)
              _ -> error "dll-split: Bad args"

doit :: FilePath -> Module -> Set Module -> IO ()
doit depfile startModule expectedReachableMods
 = do xs <- readFile depfile
      let ys = catMaybes $ map mkEdge $ lines xs
          mapping = mkMap ys
          actualReachableMods = reachable mapping startModule
      unless (actualReachableMods == expectedReachableMods) $ do
          let extra     = actualReachableMods   Set.\\ expectedReachableMods
              redundant = expectedReachableMods Set.\\ actualReachableMods
              tellSet name set = unless (Set.null set) $
                                     let ms = map moduleName (Set.toList set)
                                     in putStrLn (name ++ ": " ++ unwords ms)
          putStrLn ("Reachable modules from " ++ moduleName startModule
                 ++ " out of date")
          putStrLn "Please fix it, or building DLLs on Widnows may break (#7780)"
          tellSet "Redundant modules" redundant
          tellSet "Extra modules"     extra
          exitFailure

newtype Module = Module String
    deriving (Eq, Ord)

moduleName :: Module -> String
moduleName (Module name) = name

-- Given:
-- compiler/stage2/build/X86/Regs.o : compiler/stage2/build/CodeGen/Platform.hi
-- Produce:
-- Just ("X86.Regs", "CodeGen.Platform")
mkEdge :: String -> Maybe (Module, Module)
mkEdge str = case words str of
             [from, ":", to]
              | Just from' <- getModule from
              , Just to'   <- getModule to ->
                 Just (from', to')
             _ ->
                 Nothing
    where getModule xs
              = case stripPrefix "compiler/stage2/build/" xs of
                Just xs' ->
                    let name = filePathToModuleName $ dropExtension xs'
                    in Just $ Module name
                Nothing  -> Nothing
          filePathToModuleName = map filePathToModuleNameChar
          filePathToModuleNameChar '/' = '.'
          filePathToModuleNameChar c   = c

mkMap :: [(Module, Module)] -> (Map Module (Set Module))
mkMap edges = let groupedEdges = groupBy ((==) `on` fst) $ sort edges
                  mkEdgeMap ys = (fst (head ys), Set.fromList (map snd ys))
              in Map.fromList $ map mkEdgeMap groupedEdges

reachable :: Map Module (Set Module) -> Module -> Set Module
reachable mapping startModule = f Set.empty startModule
    where f done m = if m `Set.member` done
                     then done
                     else foldl' f (m `Set.insert` done) (get m)
          get m = Set.toList (Map.findWithDefault Set.empty m mapping)

