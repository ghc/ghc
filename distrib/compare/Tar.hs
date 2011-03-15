
module Tar where

import Data.Either
import Data.List
import System.Exit
import System.Process

import Utils

readTarLines :: FilePath -> IO [TarLine]
readTarLines fp
 = do (ec, out, err) <- readProcessWithExitCode "tar" ["-jtvf", fp] ""
      case (ec, err) of
          (ExitSuccess, []) ->
                  case parseTarLines fp out of
                  Left  errs -> die errs
                  Right tls  -> return tls
          _ ->
              die ["Failed running tar -jtvf " ++ show fp,
                   "Exit code: " ++ show ec,
                   "Stderr: " ++ show err]

parseTarLines :: FilePath -> String -> Either Errors [TarLine]
parseTarLines fp xs
    = case partitionEithers (zipWith (parseTarLine fp) [1..] (lines xs)) of
      ([],    tls) -> Right tls
      (errss, _)   -> Left (intercalate [""] errss)

data TarLine = TarLine {
                   tlPermissions :: String,
                   tlUser :: String,
                   tlGroup :: String,
                   tlSize :: Integer,
                   tlDateTime :: String,
                   tlFileName :: FilePath
               }

parseTarLine :: FilePath -> Int -> String -> Either Errors TarLine
parseTarLine fp line str
 = case re "^([^ ]+) ([^ ]+)/([^ ]+) +([0-9]+) ([^ ]+ [^ ]+) ([^ ]+)$"
           str of
   Just [perms, user, grp, sizeStr, dateTime, filename] ->
       case maybeRead sizeStr of
       Just size ->
           Right $ TarLine {
                       tlPermissions = perms,
                       tlUser        = user,
                       tlGroup       = grp,
                       tlSize        = size,
                       tlDateTime    = dateTime,
                       tlFileName    = filename
                   }
       _ -> error "Can't happen: Can't parse size"
   _ ->
       Left ["In " ++ show fp ++ ", at line " ++ show line,
             "Tar line doesn't parse: " ++ show str]

