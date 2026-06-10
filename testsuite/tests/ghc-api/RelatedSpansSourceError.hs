-- The 'Show' instance of 'SourceError' renders diagnostics with the pure
-- 'pprLocMsgEnvelope', which cannot draw carets. Check that the related
-- locations of a diagnostic ('diagnosticRelatedLocations') are listed under
-- an "At:" heading on this path rather than being lost.
-- See Note [The source span model for diagnostics] in GHC.Types.Error.
module Main where

import GHC
import GHC.Unit.Types (GenUnit(..), Definite(..))

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags { backend = noBackend, ghcLink = NoLink }
    let mn = mkModuleName "RelatedSpansSourceError_input"
    addTarget Target { targetId = TargetModule mn
                     , targetAllowObjCode = False
                     , targetUnitId = homeUnitId_ dflags
                     , targetContents = Nothing }
    -- depanal only parses; the duplicate declarations must surface as a
    -- 'SourceError' thrown by 'typecheckModule', not through the log action.
    _ <- depanal [] False
    modSum <- getModSummary (mkModule (RealUnit (Definite (homeUnitId_ dflags))) mn)
    pm <- parseModule modSum
    handleSourceError (liftIO . putStrLn . show) $ do
      _ <- typecheckModule StartAndStopTcMPlugins pm
      liftIO $ putStrLn "unexpected: typechecking succeeded"
