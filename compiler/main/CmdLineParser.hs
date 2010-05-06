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
        CmdLineP(..), getCmdLineState, putCmdLineState,
        Flag(..), Deprecated(..),
        errorsToGhcException
  ) where

#include "HsVersions.h"

import Util
import Outputable
import Panic
import SrcLoc

import Data.List

data Flag m = Flag
    {
        flagName :: String,           -- flag, without the leading -
        flagOptKind :: (OptKind m),   -- what to do if we see it
        flagDeprecated :: Deprecated  -- is the flag deprecated?
    }

data Deprecated = Supported
                | Deprecated String
                | DeprecatedFullText String

data OptKind m                      -- Suppose the flag is -f
 = NoArg (m ())                     -- -f all by itself
 | HasArg    (String -> m ())       -- -farg or -f arg
 | SepArg    (String -> m ())       -- -f arg
 | Prefix    (String -> m ())       -- -farg
 | OptPrefix (String -> m ())       -- -f or -farg (i.e. the arg is optional)
 | OptIntSuffix (Maybe Int -> m ()) -- -f or -f=n; pass n to fn
 | IntSuffix (Int -> m ())          -- -f or -f=n; pass n to fn
 | PassFlag  (String -> m ())       -- -f; pass "-f" fn
 | AnySuffix (String -> m ())       -- -f or -farg; pass entire "-farg" to fn
 | PrefixPred    (String -> Bool) (String -> m ())
 | AnySuffixPred (String -> Bool) (String -> m ())

processArgs :: Monad m
            => [Flag m] -- cmdline parser spec
            -> [Located String]      -- args
            -> m (
                  [Located String],  -- spare args
                  [Located String],  -- errors
                  [Located String]   -- warnings
                 )
processArgs spec args = process spec args [] [] []
  where
    process _spec [] spare errs warns =
      return (reverse spare, reverse errs, reverse warns)

    process spec (locArg@(L loc dash_arg@('-' : arg)) : args) spare errs warns =
      case findArg spec arg of
        Just (rest, action, deprecated) ->
           let warns' = case deprecated of
                        Deprecated warning ->
                            L loc ("Warning: " ++ dash_arg ++ " is deprecated: " ++ warning) : warns
                        DeprecatedFullText warning ->
                            L loc ("Warning: " ++ warning) : warns
                        Supported -> warns
           in case processOneArg action rest arg args of
              Left err            -> process spec args spare (L loc err : errs) warns'
              Right (action,rest) -> do action
                                        process spec rest spare errs warns'
        Nothing -> process spec args (locArg : spare) errs warns

    process spec (arg : args) spare errs warns =
      process spec args (arg : spare) errs warns


processOneArg :: OptKind m -> String -> String -> [Located String]
              -> Either String (m (), [Located String])
processOneArg action rest arg args
  = let dash_arg = '-' : arg
        rest_no_eq = dropEq rest
    in case action of
        NoArg  a -> ASSERT(null rest) Right (a, args)

        HasArg f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise    -> case args of
                                    [] -> missingArgErr dash_arg
                                    (L _ arg1:args1) -> Right (f arg1, args1)

        SepArg f -> case args of
                        [] -> unknownFlagErr dash_arg
                        (L _ arg1:args1) -> Right (f arg1, args1)

        Prefix f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise  -> unknownFlagErr dash_arg

        PrefixPred _ f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                       | otherwise          -> unknownFlagErr dash_arg

        PassFlag f  | notNull rest -> unknownFlagErr dash_arg
                    | otherwise    -> Right (f dash_arg, args)

        OptIntSuffix f | null rest                     -> Right (f Nothing,  args)
                       | Just n <- parseInt rest_no_eq -> Right (f (Just n), args)
                       | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        IntSuffix f | Just n <- parseInt rest_no_eq -> Right (f n, args)
                    | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        OptPrefix f       -> Right (f rest_no_eq, args)
        AnySuffix f       -> Right (f dash_arg, args)
        AnySuffixPred _ f -> Right (f dash_arg, args)


findArg :: [Flag m] -> String -> Maybe (String, OptKind m, Deprecated)
findArg spec arg
  = case [ (removeSpaces rest, optKind, flagDeprecated flag)
         | flag <- spec,
           let optKind = flagOptKind flag,
           Just rest <- [stripPrefix (flagName flag) arg],
           arg_ok optKind rest arg ]
    of
        []      -> Nothing
        (one:_) -> Just one

arg_ok :: OptKind t -> [Char] -> String -> Bool
arg_ok (NoArg _)            rest _   = null rest
arg_ok (HasArg _)           _    _   = True
arg_ok (SepArg _)           rest _   = null rest
arg_ok (Prefix _)           rest _   = notNull rest
arg_ok (PrefixPred p _)     rest _   = notNull rest && p (dropEq rest)
arg_ok (OptIntSuffix _)     _    _   = True
arg_ok (IntSuffix _)        _    _   = True
arg_ok (OptPrefix _)        _    _   = True
arg_ok (PassFlag _)         rest _   = null rest
arg_ok (AnySuffix _)        _    _   = True
arg_ok (AnySuffixPred p _)  _    arg = p arg

parseInt :: String -> Maybe Int
-- Looks for "433" or "=342", with no trailing gubbins
--   n or =n      => Just n
--   gibberish    => Nothing
parseInt s = case reads s of
                ((n,""):_) -> Just n
                _          -> Nothing

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

getCmdLineState :: CmdLineP s s
getCmdLineState   = CmdLineP $ \s -> (s,s)
putCmdLineState :: s -> CmdLineP s ()
putCmdLineState s = CmdLineP $ \_ -> ((),s)

-- ---------------------------------------------------------------------
-- Utils

errorsToGhcException :: [Located String] -> GhcException
errorsToGhcException errs =
   let errors = vcat [ ppr l <> text ": " <> text e | L l e <- errs ]
   in UsageError (showSDoc errors)

