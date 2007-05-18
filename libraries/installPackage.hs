
import Distribution.PackageDescription
import Distribution.Setup
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Environment

main :: IO ()
main = do args <- getArgs
          let verbosity = case args of
                              [] -> normal
                              ['-':'v':v] ->
                                  let m = case v of
                                              "" -> Nothing
                                              _ -> Just v
                                  in flagToVerbosity m
                              _ -> error ("Bad arguments: " ++ show args)
              userHooks = simpleUserHooks
              installFlags = InstallFlags {
                                 installUserFlags = MaybeUserGlobal,
                                 installVerbose = verbosity
                             }
          pdFile <- defaultPackageDesc verbosity
          pd <- readPackageDescription verbosity pdFile
          lbi <- getPersistBuildConfig
          let -- XXX This is an almighty hack, shadowing the base Setup.hs hack
              lib' = case library pd of
                         Just lib ->
                             lib {
                                 exposedModules = filter (("GHC.Prim" /=))
                                                $ exposedModules lib
                                 }
                         Nothing ->
                             error "Expected a library, but none found"
              pd' = pd { library = Just lib' }
              -- When installing we need to use the non-inplace ghc-pkg.
              -- We also set the compiler to be non-inplace, but that
              -- probably doesn't matter.
              c = compiler lbi
              c' = c { compilerPath = dropInPlace (compilerPath c),
                       compilerPkgTool = dropInPlace (compilerPkgTool c)
                     }
              lbi' = lbi { compiler = c' }
          (instHook simpleUserHooks) pd' lbi' userHooks installFlags

dropInPlace :: FilePath -> FilePath
dropInPlace "" = ""
dropInPlace xs@(x:xs') = case dropPrefix "-inplace" xs of
                             Nothing -> x : dropInPlace xs'
                             Just xs'' -> dropInPlace xs''

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix [] ys = Just ys
dropPrefix (x:xs) (y:ys)
 | x == y = dropPrefix xs ys
dropPrefix _ _ = Nothing

