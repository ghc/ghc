{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Test.Shuffle(main) where

import Control.Exception.Extra
import Control.Monad
import Data.Char
import Data.Either
import Data.IORef
import Data.List.Extra
import Data.Maybe
import Data.Typeable
import Development.Shake
import Development.Shake.Classes
import General.GetOpt
import qualified System.Directory.Extra as IO
import Test.Type


newtype Order = Order Int
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)

type instance RuleResult Order = Int


main :: IO () -> IO ()
main = testSimple $ do
    -- Command-line modes compose left-to-right, so none cancels an earlier
    -- shuffle request. Also exercise the optional argument grammar.
    shuffleFrom ["--shuffle=reverse", "--shuffle=none"] === Right ShuffleNone
    shuffleFrom ["--shuffle"] === Right (ShuffleRandom Nothing)
    shuffleFrom ["--shuffle=random"] === Right (ShuffleRandom Nothing)
    shuffleFrom ["--shuffle=123"] === Right (ShuffleRandom $ Just 123)
    assertBool (isLeft $ shuffleFrom ["--shuffle=-1"]) "negative shuffle seed should be rejected"

    -- Observe actual rule start order at -j1. Reverse is deliberately defined
    -- relative to Shake's existing scheduling order, while apply still returns
    -- values in the order requested by the caller.
    (baseline, values) <- runOrder ShuffleNone $ \_ _ -> pure ()
    (reversed, reversedValues) <- runOrder ShuffleReverse $ \_ _ -> pure ()
    reversed === reverse baseline
    values === [1..8]
    reversedValues === values

    (seeded1, seededValues1) <- runOrder (ShuffleRandom $ Just 12345) $ \_ _ -> pure ()
    (seeded2, seededValues2) <- runOrder (ShuffleRandom $ Just 12345) $ \_ _ -> pure ()
    seeded2 === seeded1
    assertBool (seeded1 /= baseline) "fixed seed should perturb the baseline order"
    seededValues1 === values
    seededValues2 === values

    -- An automatically selected seed is visible and replays the same order.
    messages <- newIORef []
    (randomOrder, _) <- runOrder (ShuffleRandom Nothing) $ \v msg ->
        when (v == Info) $ modifyIORef' messages (msg:)
    reportedSeeds <- mapMaybe readSeed <$> readIORef messages
    seed <- case reportedSeeds of
        [seed] -> pure seed
        xs -> error $ "expected one reported shuffle seed, got " ++ show xs
    (replayedOrder, _) <- runOrder (ShuffleRandom $ Just seed) $ \_ _ -> pure ()
    replayedOrder === randomOrder

    -- Exercise validation of dependencies loaded from the database, rather
    -- than only dependencies introduced by the currently running action.
    whenM_ (IO.doesDirectoryExist ".shuffle-incremental") $ IO.removePathForcibly ".shuffle-incremental"
    whenM_ (IO.doesFileExist "shuffle-incremental.out") $ IO.removeFile "shuffle-incremental.out"
    void $ runIncremental ShuffleNone
    storedBaseline <- runIncremental ShuffleNone
    storedReverse <- runIncremental ShuffleReverse
    sort storedBaseline === [1..8]
    storedReverse === reverse storedBaseline

    -- A missing sibling edge is hidden by the legacy order and exposed by the
    -- reverse order. Once declared, the same reverse build succeeds.
    legacy <- runSibling ShuffleNone False "legacy"
    assertBool (isRight legacy) "legacy sibling order should satisfy the test precondition"
    fuzzed <- runSibling ShuffleReverse False "fuzzed"
    assertBool (isLeft fuzzed) "reverse should expose the undeclared sibling dependency"
    declared <- runSibling ShuffleReverse True "declared"
    assertBool (isRight declared) "declared dependency should work under reverse"


shuffleFrom :: [String] -> Either String ShakeShuffle
shuffleFrom args = case getOpt shakeOptDescrs args of
    (flags, [], []) -> do
        pure $ shakeShuffle $ foldl' (flip ($)) shakeOptions flags
    (_, rest, errs) -> Left $ unlines $ errs ++ map ("unexpected argument: " ++) rest


readSeed :: String -> Maybe Int
readSeed msg = do
    rest <- stripPrefix "Shake shuffle seed: " msg
    case reads $ takeWhile isDigit rest of
        [(seed, "")] -> Just seed
        _ -> Nothing


runOrder :: ShakeShuffle -> (Verbosity -> String -> IO ()) -> IO ([Int], [Int])
runOrder mode output = do
    started <- newIORef []
    returned <- newIORef []
    shake shakeOptions
        {shakeFiles = "/dev/null"
        ,shakeThreads = 1
        ,shakeVerbosity = Info
        ,shakeOutput = output
        ,shakeShuffle = mode} $ do
        addOracle $ \(Order i) -> do
            liftIO $ modifyIORef' started (++ [i])
            pure i
        action $ do
            values <- askOracles $ map Order [1..8]
            liftIO $ writeIORef returned values
    (,) <$> readIORef started <*> readIORef returned


runIncremental :: ShakeShuffle -> IO [Int]
runIncremental mode = do
    started <- newIORef []
    shake shakeOptions
        {shakeFiles = ".shuffle-incremental"
        ,shakeThreads = 1
        ,shakeVerbosity = Silent
        ,shakeShuffle = mode} $ do
        addOracle $ \(Order i) -> do
            liftIO $ modifyIORef' started (++ [i])
            pure i
        want ["shuffle-incremental.out"]
        "shuffle-incremental.out" %> \out -> do
            values <- askOracles $ map Order [1..8]
            writeFile' out $ show values
    readIORef started


runSibling :: ShakeShuffle -> Bool -> String -> IO (Either SomeException ())
runSibling mode declared suffix = do
    let source = "shuffle-sibling-" ++ suffix ++ ".source"
        result = "shuffle-sibling-" ++ suffix ++ ".result"
        producer = "shuffle-sibling-" ++ suffix ++ "-producer"
        consumer = "shuffle-sibling-" ++ suffix ++ "-consumer"
        allTarget = "shuffle-sibling-" ++ suffix
    whenM_ (IO.doesFileExist source) $ IO.removeFile source
    whenM_ (IO.doesFileExist result) $ IO.removeFile result
    try_ $ shake shakeOptions
        {shakeFiles = "/dev/null"
        ,shakeThreads = 1
        ,shakeVerbosity = Silent
        ,shakeShuffle = mode} $ do
        want [allTarget]
        phony allTarget $ need [consumer, producer]
        phony producer $ liftIO $ writeFile source "built"
        phony consumer $ do
            when declared $ need [producer]
            src <- liftIO $ readFile source
            liftIO $ writeFile result src


whenM_ :: Monad m => m Bool -> m () -> m ()
whenM_ condition act = condition >>= flip when act
