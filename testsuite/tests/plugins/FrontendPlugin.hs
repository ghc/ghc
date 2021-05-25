{-# LANGUAGE NondecreasingIndentation #-}
module FrontendPlugin where

import GHC.Plugins
import qualified GHC
import GHC              ( Ghc, LoadHowMuch(..) )
import GHC.Utils.Monad

import GHC.Driver.Pipeline
import GHC.Driver.Phases
import System.Exit
import Control.Monad
import Data.List (partition)

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {
        frontend = doMake
    }

-- Copypasted from ghc/Main.hs
doMake :: [String] -> [(String,Maybe Phase)] -> Ghc ()
doMake opts srcs  = do
    liftIO $ print opts
    let (hs_srcs, non_hs_srcs) = partition haskellish srcs

        haskellish (f,Nothing) =
          looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
        haskellish (_,Just phase) =
          phase `notElem` [ As True, As False, Cc, Cobjc, Cobjcxx, CmmCpp, Cmm
                          , StopLn]

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env NoStop srcs)
       else do

    o_files <- mapMaybeM (\x -> liftIO $ compileFile hsc_env NoStop x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

    targets <- mapM (\(src, phase) -> GHC.guessTarget src Nothing phase) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    return ()
