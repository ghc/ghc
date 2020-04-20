-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List (intercalate)
import System.IO
import GHC
import GHC.Driver.Session
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "LiteralsTest"

testOneFile libdir fileName = do
    t <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        let mn =mkModuleName fileName
        addTarget Target { targetId = TargetModule mn
                         , targetAllowObjCode = True
                         , targetContents = Nothing }
        load LoadAllTargets
        modSum <- getModSummary mn
        toks <- getRichTokenStream (ms_mod modSum)
        return toks

    putStrLn (intercalate "\n" [showToks t])

showToks ts = intercalate ",\n\n"
            $ map (\((L p t),s) ->
                         "(" ++ pp p ++ "," ++ show t ++ ",[" ++ s ++ "])") ts

pp a = showPpr unsafeGlobalDynFlags a
