module Test.Haddock.Config where


import System.Console.GetOpt
import System.FilePath

import Test.Haddock.Process
import Test.Haddock.Utils


data Config = Config
    { cfgHaddockPath :: FilePath
    , cfgGhcPath :: FilePath
    , cfgFiles :: [FilePath]
    , cfgHaddockArgs :: [String]
    , cfgHaddockStdOut :: FilePath
    , cfgDiffTool :: Maybe FilePath
    , cfgEnv :: Environment
    }


data Flag
    = FlagHaddockPath FilePath
    | FlagGhcPath FilePath
    | FlagHaddockOptions String
    | FlagHaddockStdOut FilePath
    | FlagDiffTool FilePath
    | FlagNoDiff
    | FlagHelp
    deriving Eq


flagsHaddockPath :: [Flag] -> Maybe FilePath
flagsHaddockPath flags = mlast [ path | FlagHaddockPath path <- flags ]


flagsGhcPath :: [Flag] -> Maybe FilePath
flagsGhcPath flags = mlast [ path | FlagGhcPath path <- flags ]


flagsHaddockOptions :: [Flag] -> [String]
flagsHaddockOptions flags = concat
    [ words opts | FlagHaddockOptions opts <- flags ]


flagsHaddockStdOut :: [Flag] -> Maybe FilePath
flagsHaddockStdOut flags = mlast [ path | FlagHaddockStdOut path <- flags ]


flagsDiffTool :: [Flag] -> Maybe FilePath
flagsDiffTool flags = mlast [ path | FlagDiffTool path <- flags ]


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option [] ["ghc-path"] (ReqArg FlagGhcPath "FILE")
        "path to GHC executable"
    , Option [] ["haddock-options"] (ReqArg FlagHaddockOptions "OPTS")
        "additional options to run Haddock with"
    , Option [] ["haddock-stdout"] (ReqArg FlagHaddockStdOut "FILE")
        "where to redirect Haddock output"
    , Option [] ["diff-tool"] (ReqArg FlagDiffTool "PATH")
        "diff tool to use when printing failed cases"
    , Option [] ["no-diff"] (NoArg FlagNoDiff)
        "do not print diff for failed cases"
    , Option ['h'] ["help"] (NoArg FlagHelp)
        "display this help end exit"
    ]
