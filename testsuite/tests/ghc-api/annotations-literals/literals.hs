-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data
import Data.List (intercalate)
import System.IO
import GHC
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Env
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )
import Control.Monad.IO.Class

main::IO()
main = do
        [libdir] <- getArgs
        testOneFile libdir "LiteralsTest"

testOneFile libdir fileName = do
    t <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        unit <- hscActiveUnit <$> getSession
        let mn =mkModuleName fileName
        addTarget Target { targetId = TargetModule mn
                         , targetAllowObjCode = True
                         , targetUnitId = unit
                         , targetContents = Nothing }
        load LoadAllTargets
        modSum <- getModSummary mn
        toks <- liftIO $ getRichTokenStream modSum
        return toks

    putStrLn (intercalate "\n" [showToks t])

showToks ts = intercalate ",\n\n"
            $ map (\((L p t),s) ->
                         "(" ++ pp p ++ "," ++ show t ++ ",[" ++ s ++ "])") ts

pp a = showPprUnsafe a
