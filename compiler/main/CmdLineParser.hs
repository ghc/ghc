-----------------------------------------------------------------------------
--
-- Command-line parser
--
-- This is an abstract command-line parser used by both StaticFlags and
-- DynFlags.
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module CmdLineParser (
	processArgs, OptKind(..), 
	CmdLineP(..), getCmdLineState, putCmdLineState
  ) where

#include "HsVersions.h"

import Util	( maybePrefixMatch, notNull, removeSpaces )
#ifdef DEBUG
import Panic	( assertPanic )
#endif

data OptKind m		-- Suppose the flag is -f
	= NoArg (m ())			-- -f all by itself
	| HasArg    (String -> m ())    -- -farg or -f arg
	| SepArg    (String -> m ())    -- -f arg
	| Prefix    (String -> m ())    -- -farg 
	| OptPrefix (String -> m ())    -- -f or -farg (i.e. the arg is optional)
	| OptIntSuffix (Maybe Int -> m ())	-- -f or -f=n; pass n to fn
	| PassFlag  (String -> m ())    -- -f; pass "-f" fn
	| AnySuffix (String -> m ())    -- -f or -farg; pass entire "-farg" to fn
	| PrefixPred    (String -> Bool) (String -> m ())
	| AnySuffixPred (String -> Bool) (String -> m ())

processArgs :: Monad m
	    => [(String, OptKind m)]	-- cmdline parser spec
	    -> [String]			-- args
	    -> m (
	        [String],  -- spare args
                [String]   -- errors
		)
processArgs spec args = process spec args [] []
  where
    process _spec [] spare errs =
      return (reverse spare, reverse errs)
    
    process spec (dash_arg@('-':arg):args) spare errs =
      case findArg spec arg of
        Just (rest,action) -> 
           case processOneArg action rest arg args of
    	     Left err            -> process spec args spare (err:errs)
    	     Right (action,rest) -> action >> process spec rest spare errs
        Nothing -> process spec args (dash_arg:spare) errs
    
    process spec (arg:args) spare errs = 
      process spec args (arg:spare) errs


processOneArg :: OptKind m -> String -> String -> [String]
	      -> Either String (m (), [String])
processOneArg action rest arg args 
  = let dash_arg = '-' : arg
    in case action of
	NoArg  a -> ASSERT(null rest) Right (a, args)

	HasArg f | notNull rest -> Right (f rest, args)
		 | otherwise    -> case args of
				    [] -> missingArgErr dash_arg
				    (arg1:args1) -> Right (f arg1, args1)

	SepArg f -> case args of
			[] -> unknownFlagErr dash_arg
			(arg1:args1) -> Right (f arg1, args1)

	Prefix f | notNull rest -> Right (f rest, args)
		 | otherwise  -> unknownFlagErr dash_arg
	
	PrefixPred p f | notNull rest -> Right (f rest, args)
		       | otherwise    -> unknownFlagErr dash_arg
	
	PassFlag f  | notNull rest -> unknownFlagErr dash_arg
		    | otherwise    -> Right (f dash_arg, args)

	OptIntSuffix f | Just n <- parseInt rest -> Right (f n, args)
		       | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

	OptPrefix f       -> Right (f rest, args)
	AnySuffix f       -> Right (f dash_arg, args)
	AnySuffixPred p f -> Right (f dash_arg, args)



findArg :: [(String,OptKind a)] -> String -> Maybe (String,OptKind a)
findArg spec arg
  = case [ (removeSpaces rest, k) 
	 | (pat,k)   <- spec, 
	   Just rest <- [maybePrefixMatch pat arg],
	   arg_ok k rest arg ] 
    of
	[]      -> Nothing
	(one:_) -> Just one

arg_ok (NoArg _)            rest arg = null rest
arg_ok (HasArg _)           rest arg = True
arg_ok (SepArg _)           rest arg = null rest
arg_ok (Prefix _)	    rest arg = notNull rest
arg_ok (PrefixPred p _)     rest arg = notNull rest && p rest
arg_ok (OptIntSuffix _)	    rest arg = True
arg_ok (OptPrefix _)	    rest arg = True
arg_ok (PassFlag _)         rest arg = null rest 
arg_ok (AnySuffix _)        rest arg = True
arg_ok (AnySuffixPred p _)  rest arg = p arg

parseInt :: String -> Maybe (Maybe Int)
-- Looks for "433" or "=342", with no trailing gubbins
--   empty string => Just Nothing
--   n or =n	  => Just (Just n)
--   gibberish    => Nothing
parseInt s 
  | null s    = Just Nothing
  | otherwise = case reads (dropEq s) of
		  ((n,""):_) -> Just (Just n)
		  other	     -> Nothing

dropEq :: String -> String
-- Discards a leading equals sign
dropEq ('=' : s) = s
dropEq s         = s

unknownFlagErr :: String -> Either String a
unknownFlagErr f = Left ("unrecognised flag: " ++ f)

missingArgErr :: String -> Either String a
missingArgErr f = Left ("missing argument for flag: " ++ f)

-- -----------------------------------------------------------------------------
-- A state monad for use in the command-line parser

newtype CmdLineP s a = CmdLineP { runCmdLine :: s -> (a, s) }

instance Monad (CmdLineP s) where
	return a = CmdLineP $ \s -> (a, s)
	m >>= k  = CmdLineP $ \s -> let
		(a, s') = runCmdLine m s
		in runCmdLine (k a) s'

getCmdLineState   = CmdLineP $ \s -> (s,s)
putCmdLineState s = CmdLineP $ \_ -> ((),s)
