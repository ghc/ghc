
module Test.Tup(main) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util
import Test.Type
import Data.Maybe
import Control.Monad
import System.Info.Extra


-- Running ar on Mac seems to break in CI - not sure why
main = testBuild (unless isMac . defaultTest) $ do
    -- Example inspired by http://gittup.org/tup/ex_multiple_directories.html
    usingConfigFile $ shakeRoot </> "src/Test/Tup/root.cfg"

    action $ do
        keys <- getConfigKeys
        need [x -<.> exe | x <- keys, takeExtension x == ".exe"]

    let objects dir key = do
            let f x | takeExtension x == ".c" = dir </> x -<.> "o"
                    | takeExtension x == ".a" = takeBaseName x </> "lib" ++ x
                    | otherwise = error $ "Unknown extension, " ++ x
            x <- fromMaybe (error $ "Missing config key, " ++ key) <$> getConfig key
            pure $ map f $ words x

    (\x -> x -<.> exe == x) ?> \out -> do
        os <- objects "" $ takeBaseName out <.> "exe"
        need os
        cmd "gcc" os "-o" [out]

    "//lib*.a" %> \out -> do
        os <- objects (drop 3 $ takeBaseName out) $ drop 3 $ takeFileName out
        need os
        cmd "ar crs" [out] os

    "//*.o" %> \out -> do
        let src = shakeRoot </> "src/Test/Tup" </> out -<.> "c"
        need [src]
        cmd_ "gcc -c -MMD -MF" [out -<.> "d"] [src] "-o" [out] "-O2 -Wall" ["-I" ++ shakeRoot </> "src/Test/Tup/newmath"]
        neededMakefileDependencies $ out -<.> "d"
