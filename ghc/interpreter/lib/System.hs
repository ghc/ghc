-----------------------------------------------------------------------------
-- Standard Library: System operations
--
-- Warning: the implementation of these functions in Hugs 98 is very weak.
-- The functions themselves are best suited to uses in compiled programs,
-- and not to use in an interpreter-based environment like Hugs.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module System (
	ExitCode(..), exitWith, exitFailure,
	getArgs, getProgName, getEnv, 
	system
	) where

data ExitCode = ExitSuccess | ExitFailure Int
                deriving (Eq, Ord, Read, Show)

getArgs                     :: IO [String]
getArgs                      = primGetRawArgs >>= \rawargs ->
                               return (drop 1 (dropWhile (/= "--") rawargs))

getProgName                 :: IO String
getProgName                  = primGetRawArgs >>= \rawargs ->
                               return (head rawargs)

getEnv                      :: String -> IO String
getEnv                       = primGetEnv

system                      :: String -> IO ExitCode
system s                     = error "System.system unimplemented"

exitWith                    :: ExitCode -> IO a
exitWith c                   = error "System.exitWith unimplemented"

exitFailure		    :: IO a
exitFailure		     = exitWith (ExitFailure 1)

toExitCode                  :: Int -> ExitCode
toExitCode 0                 = ExitSuccess
toExitCode n                 = ExitFailure n

fromExitCode                :: ExitCode -> Int
fromExitCode ExitSuccess     = 0
fromExitCode (ExitFailure n) = n

-----------------------------------------------------------------------------
