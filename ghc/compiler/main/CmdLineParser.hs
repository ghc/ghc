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

import Util		( maybePrefixMatch, notNull, removeSpaces )

data OptKind m
	= NoArg (m ())  -- flag with no argument
	| HasArg    (String -> m ())    -- flag has an argument (maybe prefix)
	| SepArg    (String -> m ())    -- flag has a separate argument
	| Prefix    (String -> m ())    -- flag is a prefix only
	| OptPrefix (String -> m ())    -- flag may be a prefix
	| AnySuffix (String -> m ())    -- flag is a prefix, pass whole arg to fn
	| PassFlag  (String -> m ())    -- flag with no arg, pass flag to fn
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
    
    process spec args@(('-':arg):args') spare errs =
      case findArg spec arg of
        Just (rest,action) -> 
           case processOneArg action rest args of
    	   Left err       -> process spec args' spare (err:errs)
    	   Right (action,rest) -> do
    		action >> process spec rest spare errs
        Nothing -> 
    	   process spec args' (('-':arg):spare) errs
    
    process spec (arg:args) spare errs = 
      process spec args (arg:spare) errs


processOneArg :: OptKind m -> String -> [String]
  -> Either String (m (), [String])
processOneArg action rest (dash_arg@('-':arg):args) =
  case action of
	NoArg  a -> ASSERT(null rest) Right (a, args)

	HasArg f -> 
		if rest /= "" 
			then Right (f rest, args)
			else case args of
				[] -> missingArgErr dash_arg
				(arg1:args1) -> Right (f arg1, args1)

	SepArg f -> 
		case args of
			[] -> unknownFlagErr dash_arg
			(arg1:args1) -> Right (f arg1, args1)

	Prefix f -> 
		if rest /= ""
			then Right (f rest, args)
			else unknownFlagErr dash_arg
	
	PrefixPred p f -> 
		if rest /= ""
			then Right (f rest, args)
			else unknownFlagErr dash_arg
	
	OptPrefix f       -> Right (f rest, args)

	AnySuffix f       -> Right (f dash_arg, args)

	AnySuffixPred p f -> Right (f dash_arg, args)

	PassFlag f  -> 
		if rest /= ""
			then unknownFlagErr dash_arg
			else Right (f dash_arg, args)


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
arg_ok (OptPrefix _)	    rest arg = True
arg_ok (PassFlag _)         rest arg = null rest 
arg_ok (AnySuffix _)        rest arg = True
arg_ok (AnySuffixPred p _)  rest arg = p arg

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
