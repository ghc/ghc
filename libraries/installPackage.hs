
import Distribution.PackageDescription
import Distribution.Program
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
              pref : ghcpkg : ghcpkgconf : args' ->
                  let verbosity = case args' of
                              [] -> normal
                              ['-':'v':v] ->
                                  let m = case v of
                                              "" -> Nothing
                                              _ -> Just v
                                  in flagToVerbosity m
                              _ -> error ("Bad arguments: " ++ show args)
                  in doit pref ghcpkg ghcpkgconf verbosity
              _ ->
                  error "Missing arguments"

doit :: FilePath -> FilePath -> FilePath -> Verbosity -> IO ()
doit pref ghcpkg ghcpkgconf verbosity =
       do let userHooks = simpleUserHooks
              copyFlags = (emptyCopyFlags NoCopyDest) {
                              copyVerbose = verbosity
                          }
              registerFlags = emptyRegisterFlags {
                                  regUser = MaybeUserGlobal,
                                  regVerbose = verbosity,
                                  regGenScript = False,
                                  regInPlace = False
                              }
          lbi <- getPersistBuildConfig
          let pd = localPkgDescr lbi
              i = installDirTemplates lbi
              -- XXX This is an almighty hack, shadowing the base
              -- Setup.hs hack
              mkLib filt = case library pd of
                           Just lib ->
                               let ems = filter filt $ exposedModules lib
                               in lib {
                                      exposedModules = ems
                                   }
                           Nothing ->
                               error "Expected a library, but none found"
              -- There's no files for GHC.Prim, so we will fail if we
              -- try to copy them
              pd_copy = pd { library = Just (mkLib ("GHC.Prim" /=)) }
              pd_reg  = pd { library = Just (mkLib (const True)) }
              -- When coying, we need to actually give a concrete
              -- directory to copy to rather than "$topdir"
              i_copy = i { prefixDirTemplate = toPathTemplate pref }
              lbi_copy = lbi { installDirTemplates = i_copy }
              -- When we run GHC we give it a $topdir that includes the
              -- $compiler/lib/ part of libsubdir, so we only want the
              -- $pkgid part in the package.conf file. This is a bit of
              -- a hack, really.
              progs = withPrograms lbi
              prog = ConfiguredProgram {
                         programId = programName ghcPkgProgram,
                         programVersion = Nothing,
                         programArgs = ["--global-conf", ghcpkgconf],
                         programLocation = UserSpecified ghcpkg
                     }
              progs' = updateProgram prog progs
              i_reg = i { libSubdirTemplate = toPathTemplate "$pkgid" }
              lbi_reg = lbi { installDirTemplates = i_reg,
                              withPrograms = progs' }
          (copyHook simpleUserHooks) pd_copy lbi_copy userHooks copyFlags
          (regHook simpleUserHooks)  pd_reg  lbi_reg  userHooks registerFlags
          return ()

