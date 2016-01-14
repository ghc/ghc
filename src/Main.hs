module Main (main) where

import Development.Shake

import qualified Base
import qualified Rules
import qualified Rules.Cabal
import qualified Rules.Config
import qualified Rules.Generate
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Oracles
import qualified Rules.Perl
import qualified Test
import Oracles.Config.CmdLineFlag (putOptions, flags)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (chr)

main :: IO ()
main = shakeArgsWith options flags $ \cmdLineFlags targets -> do
    putOptions cmdLineFlags
    return . Just $ if null targets then rules else want targets
        >> withoutActions rules
  where
    rules :: Rules ()
    rules = mconcat
        [ Rules.Cabal.cabalRules
        , Rules.Config.configRules
        , Rules.Generate.copyRules
        , Rules.Generate.generateRules
        , Rules.Perl.perlScriptRules
        , Rules.generateTargets
        , Rules.Gmp.gmpRules
        , Rules.Libffi.libffiRules
        , Rules.Oracles.oracleRules
        , Rules.packageRules
        , Test.testRules ]
    options = shakeOptions
        { shakeFiles    = Base.shakeFilesPath
        , shakeProgress = progressSimple
        , shakeTimings  = True
        , shakeOutput   = const putMsg
        }

-- | Dynamic switch for @putStr@ and @putStrLn@ depending on the @msg@.
putMsg :: String -> IO ()
putMsg msg | dropEscSequence msg == "." = BS.putStr . UTF8.fromString $ msg
putMsg msg                              = BS.putStrLn . UTF8.fromString $ msg

-- | Drops ANSI Escape sequences from a string.
dropEscSequence :: String -> String
dropEscSequence = go
  where
    esc :: Char
    esc = Data.Char.chr 27
    go :: String -> String
    go []     = []
    go [x]    = [x]
    go (x:xs) | x == esc  = skip xs
    go (x:xs) | otherwise = x:go xs
    skip :: String -> String
    skip []    = []
    skip ['m'] = []
    skip ('m':xs) = go xs
    skip (_  :xs) = skip xs
