module DPH.Testsuite.Testcase (
  Test(..), mkTest, runTests
) where
import Test.QuickCheck
import System.Environment (getArgs)
import Data.Maybe (isJust)
import System.IO
import System.Exit


data Test
        = Test 
        { testName     :: String
        , testProperty :: Property }


mkTest :: Testable a => String -> a -> Test
mkTest name = Test name . property


runTests :: [Test] -> IO ()
runTests tests 
 = do   args <- getArgs
        mapM_ chk tests


chk (Test { testName = name, testProperty = prop }) 
 = do   putStr $ name ++ spaces (60 - length name) ++ "... "

        hFlush stdout
        res <- quickCheckWithResult customArgs prop

        case res of
          Success n _ _ 
           -> putStrLn $ "pass (" ++ show n ++ ")"

          GaveUp  n _ _
           -> do putStrLn $ "EXHAUSTED (" ++ show n ++ ")"
                 die

          Failure n _ _ _ _ _ s 
           -> do putStrLn $ "FAILED (" ++ show n ++ ")"
                 putStrLn $ indent s
                 die

          NoExpectedFailure n _ _
           -> do putStrLn $ "NO EXPECTED FAILURE (" ++ show n ++ ")"
                 die

        hFlush stdout

 where spaces n | n <= 0    = ""
                | otherwise = replicate n ' '

       -- do not print to stdout
       customArgs = stdArgs { chatty = False, maxSize = 100 }
 
       indent = unlines . map (spaces 4 ++) . lines 

die = exitWith (ExitFailure 1)

