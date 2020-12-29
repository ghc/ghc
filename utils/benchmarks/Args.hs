module Args
    (
      theLast
    , ljust
    , nonNegative
    , parseArgs
    , positive
    , printUsage
    ) where

import Data.Monoid (Monoid(..), Last(..))
import System.Console.GetOpt (OptDescr, ArgOrder(Permute), getOpt, usageInfo)
import System.Environment (getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

-- | Deconstructor for 'Last' values.
theLast :: (cfg -> Last a)    -- ^ Field to access.
        -> cfg
        -> a
theLast f cfg = case f cfg of
                  Last Nothing  -> error "some horrible config sin has occurred"
                  Last (Just a) -> a

-- | Parse command line options.
parseArgs :: Monoid cfg => cfg -> [OptDescr (IO cfg)] -> [String]
          -> IO (cfg, [String])
parseArgs defCfg options args =
  case getOpt Permute options args of
    (_, _, (err:_)) -> parseError err
    (opts, rest, _) -> do
      cfg <- (mappend defCfg . mconcat) `fmap` sequence opts
      return (cfg, rest)

-- | Constructor for 'Last' values.
ljust :: a -> Last a
ljust = Last . Just

-- | Parse a positive number.
nonNegative :: (Num a, Ord a, Read a) =>
               String -> (Last a -> cfg) -> String -> IO cfg
nonNegative q f s =
    case reads s of
      [(n,"")] | n >= 0    -> return . f $ ljust n
               | otherwise -> parseError $ q ++ " must be non negative"
      _                    -> parseError $ "invalid " ++ q ++ " provided"

-- | Parse a positive number.
positive :: (Num a, Ord a, Read a) =>
            String -> (Last a -> cfg) -> String -> IO cfg
positive q f s =
    case reads s of
      [(n,"")] | n > 0     -> return . f $ ljust n
               | otherwise -> parseError $ q ++ " must be positive"
      _                    -> parseError $ "invalid " ++ q ++ " provided"

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  progName <- getProgName
  hPutStrLn stderr $ "Error: " ++ msg
  hPutStrLn stderr $ "Run \"" ++ progName ++ " --help\" for usage information\n"
  exitWith (ExitFailure 64)

printUsage :: [OptDescr b] -> ExitCode -> IO a
printUsage options exitCode = do
  p <- getProgName
  putStr (usageInfo ("Usage: " ++ p ++ " [OPTIONS] [ARGS]") options)
  mapM_ putStrLn [
       ""
     , "hi mom!"
    ]
  exitWith exitCode
