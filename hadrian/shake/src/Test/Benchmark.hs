
module Test.Benchmark(main) where

import General.GetOpt
import Development.Shake
import Test.Type
import Text.Read
import Data.List.Extra
import Development.Shake.FilePath


data Opts = Depth Int | Breadth Int
opts = [Option "" ["depth"  ] (ReqArg (fmap Depth   . readEither) "INT") ""
       ,Option "" ["breadth"] (ReqArg (fmap Breadth . readEither) "INT") ""]

-- | Given a breadth and depth come up with a set of build files
main = testBuildArgs test opts $ \opts -> do
    let depth   = lastDef 75 [x | Depth   x <- opts]
    let breadth = lastDef 75 [x | Breadth x <- opts]

    want ["0." ++ show i | i <- [1..breadth]]
    "*" %> \out -> do
        let d = read $ takeBaseName out
        need [show (d + 1) ++ "." ++ show i | d < depth, i <- [1..breadth]]
        writeFile' out ""

test build = do
    -- these help to test the stack limit
    build ["clean"]
    build []
    build []
