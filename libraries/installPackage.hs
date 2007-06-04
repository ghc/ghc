
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Cmd
import System.Environment
import System.Info

main :: IO ()
main = do args <- getArgs
          case args of
              pref : ghcpkg : args' ->
                  let verbosity = case args' of
                              [] -> normal
                              ['-':'v':v] ->
                                  let m = case v of
                                              "" -> Nothing
                                              _ -> Just v
                                  in flagToVerbosity m
                              _ -> error ("Bad arguments: " ++ show args)
                  in doit pref ghcpkg verbosity
              _ ->
                  error "Missing arguments"

doit :: FilePath -> FilePath -> Verbosity -> IO ()
doit pref ghcpkg verbosity =
       do let userHooks = simpleUserHooks
              copyFlags = CopyFlags {
                              copyDest = NoCopyDest,
                              copyVerbose = verbosity
                          }
              registerFlags = RegisterFlags {
                                  regUser = MaybeUserGlobal,
                                  regGenScript = False,
                                  regInPlace = False,
                                  regWithHcPkg = Just ghcpkg,
                                  regVerbose = verbosity
                              }
          pdFile <- defaultPackageDesc verbosity
          pd <- readPackageDescription verbosity pdFile
          lbi <- getPersistBuildConfig
          let -- XXX These are almighty hacks, shadowing the base
              -- Setup.hs hacks
              extraExtraLibs = if (os == "mingw32") &&
                                  (pkgName (package pd) == "base")
                               then ["wsock32", "msvcrt", "kernel32",
                                     "user32", "shell32"]
                               else []
              lib' = case library pd of
                         Just lib ->
                             let ems = filter (("GHC.Prim" /=))
                                     $ exposedModules lib
                                 lib_bi = libBuildInfo lib
                                 lib_bi' = lib_bi {
                                               extraLibs = extraExtraLibs
                                                       ++ extraLibs lib_bi
                                           }
                             in lib {
                                    exposedModules = ems,
                                    libBuildInfo = lib_bi'
                                 }
                         Nothing ->
                             error "Expected a library, but none found"
              pd' = pd { library = Just lib' }
              -- When coying, we need to actually give a concrete
              -- directory to copy to rather than "$topdir"
              lbi_copy = lbi { prefix = pref }
              -- When we run GHC we give it a $topdir that includes the
              -- $compiler/lib/ part of libsubdir, so we only want the
              -- $pkgid part in the package.conf file. This is a bit of
              -- a hack, really.
              lbi_register = lbi { libsubdir = "$pkgid" }
          (copyHook simpleUserHooks) pd' lbi_copy userHooks copyFlags
          (regHook simpleUserHooks) pd' lbi_register userHooks registerFlags
          return ()

