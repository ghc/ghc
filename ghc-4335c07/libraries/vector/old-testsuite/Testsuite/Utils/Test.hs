module Testsuite.Utils.Test (
  Test, ($?), ($$?), TestS(..), summarise, TestM, execTestM, liftIO, runTest
) where

import Test.QuickCheck
import Test.QuickCheck.Batch

import System.IO       ( hFlush, stdout )

data Test = Test String Property
          | Group String [Test]

($?) :: Testable a => String -> a -> Test
name $? test = Test name (property test)

($$?) :: String -> [Test] -> Test
($$?) = Group

data TestS = TestS {
               indent         :: Int
             , passCount      :: !Int
             , failCount      :: !Int
             , exhaustedCount :: !Int
             , abortedCount   :: !Int
             }

passed :: TestS -> TestS
passed t@(TestS {}) = t { passCount = passCount t + 1 }

failed :: TestS -> TestS
failed t@(TestS {}) = t { failCount = failCount t + 1 }

exhausted :: TestS -> TestS
exhausted t@(TestS {}) = t { exhaustedCount = exhaustedCount t + 1 }

aborted :: TestS -> TestS
aborted t@(TestS {}) = t { abortedCount = abortedCount t + 1 }

summarise :: TestS -> [String]
summarise s = concat [ [shows_n (passCount s) "passed"]
                     , shows_nz (failCount s) "failed"
                     , shows_nz (exhaustedCount s) "exhausted"
                     , shows_nz (abortedCount s) "aborted"
                     ]
  where
    shows_n n s = let t = show n
                      l = length t
                  in
                  replicate (10 - l) ' ' ++ t ++ " " ++ s

    shows_nz 0 s = []
    shows_nz n s = [shows_n n s]

newtype TestM a = TestM { runTestM :: TestS -> IO (a, TestS) }

instance Monad TestM where
  return x = TestM $ \s -> return (x,s)

  TestM f >>= g = TestM $ \s ->
                    do
                      (x,s') <- f s
                      runTestM (g x) s'

readTestM :: (TestS -> a) -> TestM a
readTestM f = TestM $ \s -> return (f s, s)

updTestM :: (TestS -> TestS) -> TestM ()
updTestM f = TestM $ \s -> return ((), f s)

execTestM :: TestM a -> IO (a, TestS)
execTestM (TestM f) = f $ TestS {
                                  indent         = 0
                                , passCount      = 0
                                , failCount      = 0
                                , exhaustedCount = 0
                                , abortedCount   = 0
                                }

liftIO :: IO a -> TestM a
liftIO p = TestM $ \s -> do
                           x <- p
                           return (x,s)

runTest :: Test -> TestM ()
runTest (Group name tests)
  = do
      ind <- readTestM indent
      liftIO . putStrLn $ replicate (ind * 2 + 2) '*' ++ " " ++ name
      updTestM $ \s -> s { indent = ind + 1 }
      mapM_ runTest tests
      updTestM $ \s -> s { indent = ind }
runTest (Test name prop)
  = do
      liftIO $ do putStr $ name ++ replicate (60 - length name) ' ' ++ "... "
                  hFlush stdout
      res <- liftIO $ run prop defOpt
      let (s, ss, upd) = result res
      liftIO $ do putStrLn s
                  hFlush stdout
                  mapM_ (putStrLn . ("    " ++)) ss
                  hFlush stdout
      updTestM upd

{-
      case res of
        TestOk _ n _ -> putStrLn $ "pass (" ++ show n ++ ")"
        TestExausted _ n _ -> putStrLn $ "EXHAUSTED (" ++ show n ++ ")"
        TestFailed    s n   ->
          do
            putStrLn $ "FAIL (" ++ show n ++ ")"
            mapM_ putStrLn $ map ("    " ++) s
        TestAborted e ->
          do
            putStrLn $ "ABORTED"
            putStrLn $ "    " ++ show e
-}

result :: TestResult -> (String, [String], TestS -> TestS)
result (TestOk _ _ _) = ("pass", [], passed)
result (TestExausted _ n _) = ("EXHAUSTED", [], exhausted)
result (TestFailed s n)     = ("FAIL", s, failed)
result (TestAborted e)      = ("ABORTED", [show e], aborted)

