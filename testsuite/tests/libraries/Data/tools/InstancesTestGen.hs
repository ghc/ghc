-- A very simple pre-processor to generate instances of test cases.

import System.Environment
import System.IO
import Distribution.Compat.FilePath

import qualified Data.Map as M
import Data.List
import Data.Char

descLine (':':_) = True
descLine _ = False

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

classify p x y = p x == p y

fields :: String -> [String]
fields = map trim . filter (not . descLine) . groupBy (classify (== ':')) . trim

toAlpha c | isAlphaNum c = c
          | otherwise = '_'

buildTest :: String -> String -> [String] -> [String] -> IO (String)
buildTest fname file keywords values = 
    do let targetFname = "test_" ++ (map toAlpha $ unwords values) ++ "_" ++ snd (splitFileName fname)
           (testName,"hs") = splitFileExt targetFname
           subst = M.fromList $ zip keywords values
           props = nub $ filter ("prop_" `isPrefixOf`) $ words file
           sourceWords = groupBy (classify isAlpha) file
           replace w = maybe w id (M.lookup w subst)
           target = concatMap replace sourceWords ++
                    "propNames = " ++ show props ++ "\n" ++
                    "propTests = [" ++ concat (intersperse "," $ map ("run "++) props) ++ "]\n" ++
                    "fileName = "++ show targetFname ++ "\n"
                    
       writeFile targetFname target
       return testName

processOneFile fname = 
    do
       file <- readFile fname
       let ls = lines file
           (keywords:substs) = map fields $ map tail $ filter descLine ls
       mapM (buildTest fname file keywords) substs
       
generateDriver test = "test('" ++ test ++ "', normal, multimod_compile_and_run_ignore_output, ['"
                      ++ test ++ "', '-ireference -itools -fglasgow-exts'])"

main = 
    do tests <- mapM processOneFile =<< getArgs
       writeFile "generated.T" $ unlines $ map generateDriver $ concat tests
       
