{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

module Test.Random(main) where

import Development.Shake
import Numeric.Extra
import Test.Type
import Control.Exception.Extra
import Control.Monad
import Data.List
import Data.Maybe
import General.GetOpt
import System.Environment
import System.Exit
import System.Random
import General.Extra
import qualified System.IO.Extra as IO
import System.Time.Extra


inputRange = [1..10]

data Value = Single Int | Multiple [[Value]]
    deriving (Read,Show,Eq)

data Source = Input Int | Output Int | Bang
    deriving (Read,Show)

data Logic = Logic Int [[Source]]
           | Want [Int]
    deriving (Read,Show)

arg = [Option "" ["arg"] (ReqArg Right "") ""]

main = testBuildArgs test arg $ \args -> do
    let toFile (Input i) = "input-" ++ show i ++ ".txt"
        toFile (Output i) = "output-" ++ show i ++ ".txt"
        toFile Bang = error "BANG"

    let randomSleep = liftIO $ do
            i <- randomRIO (0, 25)
            sleep $ intToDouble i / 100

    forM_ (map read $ filter (isNothing . asDuration) args) $ \case
        Want xs -> want $ map (toFile . Output) xs
        Logic out srcs -> toFile (Output out) %> \out -> do
            res <- fmap (show . Multiple) $ forM srcs $ \src -> do
                randomSleep
                need $ map toFile src
                mapM (liftIO . fmap read . IO.readFile' . toFile) src
            randomSleep
            writeFileChanged out res


asDuration :: String -> Maybe Double
asDuration x
    | "s" `isSuffixOf` x, [(i,"")] <- reads $ init x = Just i
    | "m" `isSuffixOf` x, [(i,"")] <- reads $ init x = Just $ i * 60
    | otherwise = Nothing


test build = do
    limit <- do
        args <- getArgs
        let bound = listToMaybe $ reverse $ mapMaybe asDuration args
        time <- offsetTime
        pure $ when (isJust bound) $ do
            now <- time
            when (now > fromJust bound) exitSuccess

    forM_ [1..] $ \count -> do
        limit
        putStrLn $ "* PERFORMING RANDOM TEST " ++ show count
        build ["clean"]
        build [] -- to create the directory
        forM_ inputRange $ \i ->
            writeFile ("input-" ++ show i ++ ".txt") $ show $ Single i
        logic <- randomLogic
        runLogic [] logic
        chng <- filterM (const randomIO) inputRange
        forM_ chng $ \i ->
            writeFile ("input-" ++ show i ++ ".txt") $ show $ Single $ negate i
        runLogic chng logic
        forM_ inputRange $ \i ->
            writeFile ("input-" ++ show i ++ ".txt") $ show $ Single i
        logicBang <- addBang =<< addBang logic
        j <- randomRIO (1::Int,8)
        res <- try_ $ build $ "--exception" : ("-j" ++ show j) : map ((++) "--arg=" . show) (logicBang ++ [Want [i | Logic i _ <- logicBang]])
        case res of
            Left err
                | "BANG" `isInfixOf` show err -> pure () -- error I expected
                | otherwise -> error $ "UNEXPECTED ERROR: " ++ show err
            _ -> pure () -- occasionally we only put BANG in places with no dependenies that don't get rebuilt
        runLogic [] $ logic ++ [Want [i | Logic i _ <- logic]]
        where
            runLogic :: [Int] -> [Logic] -> IO ()
            runLogic negated xs = do
                let poss = [i | Logic i _ <- xs]
                i <- randomRIO (0, 7)
                wants <- replicateM i $ do
                    i <- randomRIO (0, 5)
                    replicateM i $ randomElem poss
                sleepFileTime
                j <- randomRIO (1::Int,8)
                build $ ("-j" ++ show j) : map ((++) "--arg=" . show) (xs ++ map Want wants)

                let value i =
                        let ys = headErr [ys | Logic j ys <- xs, j == i]
                        in Multiple $ flip map ys $ map $ \case
                            Input i -> Single $ if i `elem` negated then negate i else i
                            Output i -> value i
                            Bang -> error "BANG"
                forM_ (concat wants) $ \i -> do
                    let wanted = value i
                    got <- fmap read $ IO.readFile' $ "output-" ++ show i ++ ".txt"
                    when (wanted /= got) $
                        error $ "INCORRECT VALUE for " ++ show i


addBang :: [Logic] -> IO [Logic]
addBang xs = do
    i <- randomRIO (0, length xs - 1)
    let (before,now:after) = splitAt i xs
    now <- f now
    pure $ before ++ now : after
    where
        f (Logic log xs) = do
            i <- randomRIO (0, length xs)
            let (before,after) = splitAt i xs
            pure $ Logic log $ before ++ [Bang] : after
        f x = pure x


randomLogic :: IO [Logic] -- only Logic constructors
randomLogic = do
    rules <- randomRIO (1,100)
    f rules $ map Input inputRange
    where
        f 0 _ = pure []
        f i avail = do
            needs <- randomRIO (0,3)
            xs <- replicateM needs $ do
                ns <- randomRIO (0,3)
                replicateM ns $ randomElem avail
            let r = Logic i xs
            (r:) <$> f (i-1) (Output i:avail)
