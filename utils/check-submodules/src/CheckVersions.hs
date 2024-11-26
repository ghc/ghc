{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module CheckVersions
    ( checkVersions
    , summarize
    , maintainerEmails
    ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Function (on)
import Data.List (intercalate, sort, sortOn, nubBy)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Version
import Distribution.Types.PackageName qualified as C
import System.Exit

import Hackage (getVersions, PackageState (..))
import Pretty
import Package
import Packages

isPvpCompatible :: Version -> Version -> Bool
isPvpCompatible a b =
    take 2 (versionBranch a) == take 2 (versionBranch b)

updateVersion :: M.Map Version PackageState -> Version -> Maybe Version
updateVersion available v
  | [] <- compatible = Nothing
  | otherwise        = Just $ maximum compatible
  where
    compatible =
      [ v'
      | (v', Normal) <- M.assocs available -- non-deprecated versions available via Hackage...
      , v' > v                             -- that are newer than the submodule...
      , v' `isPvpCompatible` v             -- and are compatible with the submodule
      ]

checkPackage :: Package -> WriterT [(Severity, Doc)] IO ()
checkPackage pkg = do
    v <- liftIO $ getPackageVersion pkg
    available <- liftIO $ getVersions (pkgName pkg)

    case M.lookup v available of
        Nothing         -> tellMsg Error $ "Version" <+> ppVersion v <+> "is not on Hackage"
        Just Deprecated -> tellMsg Error $ "Version" <+> ppVersion v <+> "has been deprecated"
        Just Normal     -> return ()

    case updateVersion available v of
        Nothing -> return ()
        Just v' -> tellMsg Warning $ "Shipping with" <+> ppVersion v <+> "but newer version" <+> ppVersion v' <+> "is available"

tellMsg :: Severity -> Doc -> WriterT [(Severity, Doc)] IO ()
tellMsg sev msg = tell [(sev, msg)]

summarizeSubmodules :: [Package] -> IO ()
summarizeSubmodules pkgs = forM_ (sortOn pkgName pkgs) $ \pkg -> do
    v <- getPackageVersion pkg
    putStrLn $ "    " <> C.unPackageName (pkgName pkg) <> " " <> showVersion v <> " @ " <> pkgPath pkg

maintainerEmails :: IO String
maintainerEmails = do
    maintainers <- concat <$> mapM getPackageMaintainers packages
    return $ intercalate ", " $ map (T.unpack . contactRecipient) $ nubBy ((==) `on` contactEmail) $ sort maintainers

summarize :: IO ()
summarize =
    summarizeSubmodules packages

checkVersions :: IO ()
checkVersions = do
    errs <- mapM (\pkg -> map (pkg, ) <$> execWriterT (checkPackage pkg)) packages
    putDoc $ bulletList
      [ severityIcon sev <+> ppPackage pkg <> ":" <+> err
      | (pkg, (sev, err)) <- concat errs
      ]
    putStrLn ""
    exitWith $ if null errs then ExitSuccess else ExitFailure 1

