module RunTests where

import Test.QuickCheck.Batch hiding (runTests)
import System.Exit
import System.Environment

import HeaderInfoTests as HI

runUnitTests :: Bool -> IO ()
runUnitTests debug = exitWith =<< performTests debug

performTests :: Bool -> IO ExitCode
performTests debug =
    do e1 <- exeTests "HeaderInfo" opts
                   [ run HI.prop_optionsIdentity
                   , run HI.prop_languageParse
                   , run HI.prop_languageError ]
       return (foldr1 cat [e1])
    where opts = TestOptions 100 10 debug
          cat (e@(ExitFailure _)) _ = e
          cat _ e = e

exeTests :: String -> TestOptions -> [TestOptions -> IO TestResult] -> IO ExitCode
exeTests name scale actions =
    do putStr (rjustify 25 name ++ " : ")
       tr 1 actions [] 0 False
    where
      rjustify n s = replicate (max 0 (n - length s)) ' ' ++ s
      tr n [] xs c e = do
                     putStr (rjustify (max 0 (35-n)) " (" ++ show c ++ ")\n")
                     mapM_ fa xs
                     if e
                        then return (ExitFailure 1)
                        else return ExitSuccess
      tr n (action:actions) others c e =
          do r <- action scale
             case r of
               (TestOk _ m _)
                   -> do { putStr "." ;
                           tr (n+1) actions others (c+m) e }
               (TestExausted s m ss)
                   -> do { putStr "?" ;
                           tr (n+1) actions others (c+m) e }
               (TestAborted e)
                   -> do { print e;
                           putStr "*" ;
                           tr (n+1) actions others c True }
               (TestFailed f num)
                   -> do { putStr "#" ;
                           tr (n+1) actions ((f,n,num):others) (c+num) True }
      fa :: ([String],Int,Int) -> IO ()
      fa (f,n,no) =
          do putStr "\n"
             putStr ("    ** test "
                     ++ show (n  :: Int)
                     ++ " of "
                     ++ name
                     ++ " failed with the binding(s)\n")
             sequence_ [putStr ("    **   " ++ v ++ "\n")
                        | v <- f ]
             putStr "\n"

