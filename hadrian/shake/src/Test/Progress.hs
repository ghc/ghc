module Test.Progress(main) where

import Development.Shake.Internal.Progress
import Development.Shake.Internal.Options
import Test.Type
import System.Directory.Extra
import System.FilePath
import General.Extra


main = testBuild test $ pure ()


-- | Given a list of todo times, get out a list of how long is predicted
prog = progEx 10000000000000000


progEx :: Double -> [Double] -> IO [Double]
progEx mxDone todo = do
    let resolution = 10000 -- Use resolution to get extra detail on the numbers
    let done = scanl (+) 0 $ map (min mxDone . max 0) $ zipWith (-) todo (tailErr todo)
    let res = progressReplay $ zip (map (*resolution) [1..]) $ tailErr $ zipWith (\t d -> mempty{timeBuilt=d*resolution,timeTodo=(t*resolution,0)}) todo done
    pure $ (0/0) : map ((/ resolution) . actualSecs) res


test build = do
    -- perfect functions should match perfectly
    xs <- prog [10,9..1]
    drop 2 xs === [8,7..1]
    xs <- prog $ map (*5) [10,9..1]
    drop 2 xs === [8,7..1]
    xs <- prog $ map (*0.2) [10,9..1]
    let dp3 x = fromIntegral (round $ x * 1000 :: Int) / 1000
    map dp3 (drop 2 xs) === [8,7..1]

    -- The properties below this line could be weakened

    -- increasing functions can't match
    xs <- prog [5,6,7]
    last xs === 7

    -- the first value must be plausible, or missing
    xs <- prog [187]
    assertBool (isNaN $ headErr xs) "No first value"

    -- desirable properties, could be weakened
    xs <- progEx 2 $ 100:map (*2) [10,9..1]
    drop 5 xs === [6,5..1]
    xs <- progEx 1 [10,9,100,8,7,6,5,4,3,2,1]
    assertBool (all ((<= 1.5) . abs) $ zipWith (-) (drop 5 xs) [6,5..1]) "Close"

    -- if no progress is made, don't keep the time going up
    xs <- prog [10,9,8,7,7,7,7,7]
    drop 5 xs === [7,7,7]

    -- if the work rate changes, should somewhat reflect that
    xs <- prog [10,9,8,7,6.5,6,5.5,5]
    assertBool (last xs > 7.1) "Some discounting (factor=0 would give 7)"

    xs <- listFiles $ shakeRoot </> "src/Test/Progress"
    build $ ["--progress=replay=" ++ x | x <- xs, takeExtension x == ".prog"] ++
            ["--no-report","--report=-","--report=" ++ "progress.html"]
