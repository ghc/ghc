import Test.Cabal.Prelude
import Control.Monad.IO.Class
import Control.Monad
import Distribution.Package
import Distribution.Simple.Configure
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.InstallDirs
import Distribution.Simple.Compiler
import Distribution.Types.TargetInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.UnqualComponentName
import System.Directory

-- Internal libraries used by a statically linked executable:
-- no libraries should get installed or registered.  (Note,
-- this does build shared libraries just to make sure they
-- don't get installed, so this test doesn't work on Windows.)
main = setupAndCabalTest $ do
    skipUnless =<< hasSharedLibraries
    withPackageDb $ do
        -- MULTI
        forM_ [False, True] $ \is_dynamic -> do
            setup_install $ [ if is_dynamic then "--enable-executable-dynamic"
                                            else "--disable-executable-dynamic"
                            , "--enable-shared"]
            dist_dir <- fmap testDistDir getTestEnv
            lbi <- liftIO $ getPersistBuildConfig dist_dir
            let pkg_descr = localPkgDescr lbi
                compiler_id = compilerId (compiler lbi)
                cname = CSubLibName $ mkUnqualComponentName "foo-internal"
                [target] = componentNameTargets' pkg_descr lbi cname
                uid = componentUnitId (targetCLBI target)
                InstallDirs{libdir=dir,dynlibdir=dyndir} =
                  absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest
            assertBool "interface files should be installed"
                =<< liftIO (doesFileExist (dir </> "Foo.hi"))
            assertBool "static library should be installed"
                =<< liftIO (doesFileExist (dir </> mkLibName uid))
            if is_dynamic
              then
                assertBool "dynamic library MUST be installed"
                    =<< liftIO (doesFileExist (dyndir </> mkSharedLibName
                                               compiler_id uid))
              else
                assertBool "dynamic library should be installed"
                    =<< liftIO (doesFileExist (dyndir </> mkSharedLibName
                                               compiler_id uid))
            fails $ ghcPkg "describe" ["foo"]
            -- clean away the dist directory so that we catch accidental
            -- dependence on the inplace files
            setup "clean" []
            runInstalledExe' "foo" [] >>= assertOutputContains "46"
