{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- (c) 2007 Andy Gill

-- Main driver for Hpc
import Control.Monad (forM, forM_, when)
import Data.Bifunctor (bimap)
import Data.List (intercalate, partition, uncons)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes, isJust)
import Data.Version
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Directory (doesPathExist)

import HpcFlags
import HpcReport
import HpcMarkup
import HpcCombine
import HpcShowTix
import HpcDraft
import HpcOverlay
import Paths_hpc_bin

helpList :: IO ()
helpList = do
     putStrLn $
           "Usage: hpc COMMAND ...\n\n" ++
           section "Commands" help ++
           section "Reporting Coverage" reporting ++
           section "Processing Coverage files" processing ++
           section "Coverage Overlays" overlays ++
           section "Others" other ++
           ""
     putStrLn ""
     putStrLn "or: hpc @response_file_1 @response_file_2 ..."
     putStrLn ""
     putStrLn "The contents of a Response File must have this format:"
     putStrLn "COMMAND ..."
     putStrLn ""
     putStrLn "example:"
     putStrLn "report my_library.tix --include=ModuleA \\"
     putStrLn "--include=ModuleB"
  where
    help       = ["help"]
    reporting  = ["report","markup"]
    overlays   = ["overlay","draft"]
    processing = ["sum","combine","map"]
    other     = [ name hook
                | hook <- hooks
                , name hook `notElem`
                     (concat [help,reporting,processing,overlays])
                ]

section :: String -> [String] -> String
section _   []   = ""
section msg cmds = msg ++ ":\n"
        ++ unlines [ take 14 ("  " ++ cmd ++ repeat ' ') ++ summary hook
                   | cmd <- cmds
                   , hook <- hooks
                   , name hook == cmd
                   ]

dispatch :: [String] -> IO ()
dispatch [] = do
    helpList
    exitWith ExitSuccess
dispatch (txt:args0) = do
    case lookup txt hooks' of
      Just plugin -> parse plugin args0
      _ -> case getResponseFileName txt of
        Nothing -> parse help_plugin (txt:args0)
        Just firstResponseFileName -> do
          let
            (responseFileNames', nonResponseFileNames) = partitionFileNames args0
          -- if arguments are combination of Response Files and non-Response Files, exit with error
          when (length nonResponseFileNames > 0) $ do
            let
            putStrLn $ "First argument '" <> txt <> "' is a Response File, " <>
              "followed by non-Response File(s): '" <> intercalate "', '" nonResponseFileNames <> "'"
            putStrLn $ "When first argument is a Response File, " <>
              "all arguments should be Response Files."
            exitFailure
          let
            responseFileNames :: NonEmpty FilePath
            responseFileNames = firstResponseFileName :| responseFileNames'

          forM_ responseFileNames $ \responseFileName -> do
            exists <- doesPathExist responseFileName
            when (not exists) $ do
              putStrLn $ "Response File '" <> responseFileName <> "' does not exist"
              exitFailure

          -- read all Response Files
          responseFileNamesAndText :: NonEmpty (FilePath, String) <-
            forM responseFileNames $ \responseFileName ->
              fmap (responseFileName, ) (readFile responseFileName)
          forM_ responseFileNamesAndText $ \(responseFileName, responseFileText) ->
            -- parse first word of Response File, which should be a command
            case uncons $ words responseFileText of
              Nothing -> do
                putStrLn $ "Response File '" <> responseFileName <> "' has no command"
                exitFailure
              Just (responseFileCommand, args1) -> case lookup responseFileCommand hooks' of
                -- check command for validity
                -- It is important than a Response File cannot specify another Response File;
                -- this is prevented
                Nothing -> do
                  putStrLn $ "Response File '" <> responseFileName <>
                    "' command '" <> responseFileCommand <> "' invalid"
                  exitFailure
                Just plugin -> do
                  putStrLn $ "Response File '" <> responseFileName <> "':"
                  parse plugin args1

  where
     getResponseFileName :: String -> Maybe FilePath
     getResponseFileName s = do
       (firstChar, filename) <- uncons s
       if firstChar == '@'
         then pure filename
         else Nothing

     -- first member of tuple is list of Response File names,
     -- second member of tuple is list of all other arguments
     partitionFileNames :: [String] -> ([FilePath], [String])
     partitionFileNames xs = let
       hasFileName :: [(String, Maybe FilePath)]
       hasFileName = fmap (\x -> (x, getResponseFileName x)) xs
       (fileNames, nonFileNames) :: ([Maybe FilePath], [String]) =
         bimap (fmap snd) (fmap fst) $ partition (isJust . snd) hasFileName
       in (catMaybes fileNames, nonFileNames)

     parse plugin args =
              case getOpt Permute (options plugin []) args of
                (_,_,errs) | not (null errs)
                     -> do putStrLn "hpc failed:"
                           sequence_ [ putStr ("  " ++ err)
                                    | err <- errs
                                    ]
                           putStrLn $ "\n"
                           command_usage plugin
                           exitFailure
                (o,ns,_) -> do
                         let flags = final_flags plugin
                                   . foldr (.) id o
                                   $ init_flags plugin
                         implementation plugin flags ns

main :: IO ()
main = do
 args <- getArgs
 dispatch args

------------------------------------------------------------------------------

hooks :: [Plugin]
hooks = [ help_plugin
        , report_plugin
        , markup_plugin
        , sum_plugin
        , combine_plugin
        , map_plugin
        , showtix_plugin
        , overlay_plugin
        , draft_plugin
        , version_plugin
        ]

hooks' :: [(String, Plugin)]
hooks' = [ (name hook,hook) | hook <- hooks ]

------------------------------------------------------------------------------

help_plugin :: Plugin
help_plugin = Plugin { name = "help"
                     , usage = "[<HPC_COMMAND>]"
                     , summary = "Display help for hpc or a single command"
                     , options = help_options
                     , implementation = help_main
                     , init_flags = default_flags
                     , final_flags = default_final_flags
                     }

help_main :: Flags -> [String] -> IO ()
help_main _ [] = do
            helpList
            exitWith ExitSuccess
help_main _ (sub_txt:_) = do
    case lookup sub_txt hooks' of
      Nothing -> do
          putStrLn $ "no such HPC command: " <> sub_txt
          exitFailure
      Just plugin' -> do
          command_usage plugin'
          exitWith ExitSuccess

help_options :: FlagOptSeq
help_options   = id

------------------------------------------------------------------------------

version_plugin :: Plugin
version_plugin = Plugin { name = "version"
                        , usage = ""
                        , summary = "Display version for hpc"
                        , options = id
                        , implementation = version_main
                        , init_flags = default_flags
                        , final_flags = default_final_flags
                        }

version_main :: Flags -> [String] -> IO ()
version_main _ _ = putStrLn ("hpc tools, version " ++ showVersion version)


------------------------------------------------------------------------------
