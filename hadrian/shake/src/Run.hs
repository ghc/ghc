
module Run(main) where

import Development.Ninja.All
import System.Environment
import Development.Shake
import Development.Shake.FilePath
import General.Timing(resetTimings)
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Maybe
import qualified System.Directory as IO
import General.Extra
import General.GetOpt
import System.Process
import System.Exit
import Data.Either.Extra


main :: IO ()
main = do
    resetTimings
    args <- getArgs
    hsExe <- findFile
        [".shake" </> "shake" <.> exe
        ,"Shakefile.hs","Shakefile.lhs"]
    case hsExe of
        Just file -> do
            (prog,args)<- pure $
                if takeExtension file `elem` [".hs",".lhs"] then ("runhaskell", file:args) else (toNative file, args)
            e <- rawSystem prog args
            when (e /= ExitSuccess) $ exitWith e
        Nothing -> do
            let opts = shakeOptions{shakeThreads=0,shakeCreationCheck=False,shakeNeedDirectory=True}
            let go = shakeArgsWith opts flags $ \opts targets -> do
                        let tool = listToMaybe [x | Tool x <- opts]
                        makefile <- case reverse [x | UseMakefile x <- opts] of
                            x:_ -> pure x
                            _ -> do
                                res <- findFile ["build.ninja"]
                                case res of
                                    Just x -> pure x
                                    Nothing -> errorIO "Could not find `build.ninja'"
                        runNinja go makefile targets tool
            withArgs ("--no-time":args) go

data Flag = UseMakefile FilePath
          | Tool String

flags = [Option "f" ["file","makefile"] (ReqArg (Right . UseMakefile) "FILE") "Read FILE as a makefile."
        ,Option "t" ["tool"] (ReqArg (Right . Tool) "TOOL") "Ninja-compatible tools."
        ]

findFile :: [FilePath] -> IO (Maybe FilePath)
findFile = findM (fmap (fromRight False) . tryIO . IO.doesFileExist)
