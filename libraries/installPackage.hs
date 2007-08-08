
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import System.Environment

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
              copyFlags = (emptyCopyFlags NoCopyDest) {
                              copyVerbose = verbosity
                          }
              registerFlags = emptyRegisterFlags {
                                  regUser = MaybeUserGlobal,
                                  regVerbose = verbosity
                              }
          lbi <- getPersistBuildConfig
          let pd = localPkgDescr lbi
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
              lbi_copy = lbi { prefix = pref }
              -- When we run GHC we give it a $topdir that includes the
              -- $compiler/lib/ part of libsubdir, so we only want the
              -- $pkgid part in the package.conf file. This is a bit of
              -- a hack, really.
              lbi_reg = lbi { libsubdir = "$pkgid" }
          (copyHook simpleUserHooks) pd_copy lbi_copy userHooks copyFlags
          (regHook simpleUserHooks)  pd_reg  lbi_reg  userHooks registerFlags
          return ()

