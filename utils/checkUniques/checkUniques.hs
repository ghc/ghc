-- Some things could be improved, e.g.:
-- * Check that each file given contains at least one instance of the
--   function
-- * Check that we are testing all functions
-- * If a problem is found, give better location information, e.g.
--   which problem the file is in

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do args <- getArgs
          case args of
              function : files ->
                  doit function files

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

type M = StateT St IO

data St = St {
              stSeen :: Set Int,
              stLast :: Maybe Int,
              stHadAProblem :: Bool
          }

emptyState :: St
emptyState = St {
                 stSeen = Set.empty,
                 stLast = Nothing,
                 stHadAProblem = False
             }

use :: Int -> M ()
use n = do st <- get
           let seen = stSeen st
           put $ st { stSeen = Set.insert n seen, stLast = Just n }
           if (n `Set.member` seen)
               then problem ("Duplicate " ++ show n)
               else case stLast st of
                    Just l
                     | (l > n) ->
                        problem ("Decreasing order for " ++ show l
                                               ++ " -> " ++ show n)
                    _ ->
                        return ()

problem :: String -> M ()
problem str = do lift $ putStrLn str
                 st <- get
                 put $ st { stHadAProblem = True }

doit :: String -> [FilePath] -> IO ()
doit function files
 = do (hIn, hOut, hErr, ph) <- runInteractiveProcess
                                   "grep" ("-h" : function : files)
                                   Nothing Nothing
      hClose hIn
      strOut <- hGetContents hOut
      strErr <- hGetContents hErr
      forkIO $ do evaluate (length strOut)
                  return ()
      forkIO $ do evaluate (length strErr)
                  return ()
      ec <- waitForProcess ph
      case (ec, strErr) of
          (ExitSuccess, "") ->
              check function strOut
          _ ->
              error "grep failed"

check :: String -> String -> IO ()
check function str
    = do let ls = lines str
             -- filter out lines that start with whitespace. They're
             -- from things like:
             --     import M ( ...,
             --                ..., <function>, ...
             ls' = filter (not . all isSpace . take 1) ls
         ns <- mapM (parseLine function) ls'
         st <- execStateT (do mapM_ use ns
                              st <- get
                              when (Set.null (stSeen st)) $
                                  problem "No values found")
                          emptyState
         when (stHadAProblem st) exitFailure

parseLine :: String -> String -> IO Int
parseLine function str
    = -- words isn't necessarily quite right, e.g. we could have
    -- "var=" rather than "var =", but it works for the code
    -- we have
    case words str of
    _var : "=" : fun : numStr : rest
     | fun == function,
       null rest || "--" == head rest,
       [(num, "")] <- reads numStr
          -> return num
    _ -> error ("Bad line: " ++ show str)

