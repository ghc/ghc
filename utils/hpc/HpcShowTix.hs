module HpcShowTix (showtix_plugin) where

import Trace.Hpc.Mix
import Trace.Hpc.Tix

import HpcFlags

import qualified Data.Set as Set

showtix_options :: FlagOptSeq
showtix_options
        = excludeOpt
        . includeOpt
        . srcDirOpt
        . hpcDirOpt
        . resetHpcDirsOpt
        . outputOpt

showtix_plugin :: Plugin
showtix_plugin = Plugin { name = "show"
                       , usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]"
                       , options = showtix_options
                       , summary = "Show .tix file in readable, verbose format"
                       , implementation = showtix_main
                       , init_flags = default_flags
                       , final_flags = default_final_flags
                       }


showtix_main :: Flags -> [String] -> IO ()
showtix_main _     [] = hpcError showtix_plugin $ "no .tix file or executable name specified"
showtix_main flags (prog:modNames) = do
  let hpcflags1 = flags
                { includeMods = Set.fromList modNames
                                   `Set.union`
                                includeMods flags }

  optTixs <- readTix (getTixFileName prog)
  case optTixs of
    Nothing -> hpcError showtix_plugin $ "could not read .tix file : "  ++ prog
    Just (Tix tixs) -> do
       tixs_mixs <- sequence
               [ do mix <- readMixWithFlags hpcflags1 (Right tix)
                    return $ (tix,mix)
               | tix <- tixs
               , allowModule hpcflags1 (tixModuleName tix)
               ]

       let rjust n str = take (n - length str) (repeat ' ') ++ str
       let ljust n str = str ++ take (n - length str) (repeat ' ')

       sequence_ [ sequence_ [ putStrLn (rjust 5 (show ix) ++ " " ++
                                         rjust 10 (show count) ++ " " ++
                                         ljust 20  modName ++ " " ++ rjust 20 (show pos) ++ " " ++ show lab)
                             | (count,ix,(pos,lab)) <- zip3 tixs' [(0::Int)..] entries
                             ]
                 | ( TixModule modName _hash1 _ tixs'
                   , Mix _file _timestamp _hash2 _tab entries
                   ) <- tixs_mixs
                 ]

       return ()

