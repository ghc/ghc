-- | Check that the 'GHC.Essentials' module of the in-tree @base@ exports
-- every known-key entity in GHC's 'knownKeyTable', with agreeing uniques and
-- occ-names.
module Main (main) where

-- base
import Control.Monad.IO.Class
  ( liftIO )
import System.Environment
  ( getArgs )
import System.Exit
  ( exitFailure )
import System.IO
  ( hPutStrLn, stderr )

-- ghc
import GHC
  ( runGhc, getSessionDynFlags, setSessionDynFlags, getSession )
import GHC.Data.Maybe
  ( MaybeErr(..) )
import GHC.Iface.Load
  ( loadKnownKeyOccMaps, checkKnownKeyNamesIface )
import GHC.Tc.Utils.Monad
  ( initIfaceLoad )
import GHC.Types.Error
  ( pprDiagnostic )
import GHC.Utils.Outputable
  ( hang, showSDocUnsafe, text )

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    hsc_env <- getSession
    res <- liftIO $ initIfaceLoad hsc_env loadKnownKeyOccMaps
    liftIO $ case res of
      Failed err ->
        die $ hang (text "Could not load the known-key maps from GHC.Essentials:")
                 2 (pprDiagnostic err)
      Succeeded (kk_map, _occ_map) ->
        case checkKnownKeyNamesIface kk_map of
          Nothing -> return ()
          Just missing ->
            die $ hang (text "GHC.Essentials is missing known-key exports:")
                     2 missing
  where
    die doc = do
      hPutStrLn stderr (showSDocUnsafe doc)
      exitFailure
