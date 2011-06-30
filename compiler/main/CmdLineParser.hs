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
        Flag(..), FlagSafety(..), flagA, flagR, flagC, flagN,
        errorsToGhcException, determineSafeLevel,

        EwM, addErr, addWarn, getArg, liftEwM, deprecate
  ) where

#include "HsVersions.h"

import Util
import Outputable
import Panic
import Bag
import SrcLoc

import Data.List

--------------------------------------------------------
--	   The Flag and OptKind types
--------------------------------------------------------

data Flag m = Flag
    {   flagName    :: String,       -- Flag, without the leading "-"
        flagSafety  :: FlagSafety,   -- Flag safety level (Safe Haskell)
        flagOptKind :: OptKind m     -- What to do if we see it
    }

-- | This determines how a flag should behave when Safe Haskell
-- mode is on.
data FlagSafety
 = EnablesSafe         -- ^ This flag is a little bit of a hack. We give
                       -- the safe haskell flags (-XSafe and -XSafeLanguage)
                       -- this safety type so we can easily detect when safe
                       -- haskell mode has been enable in a module pragma
                       -- as this changes how the rest of the parsing should
                       -- happen.

 | AlwaysAllowed       -- ^ Flag is always allowed
 | RestrictedFunction  -- ^ Flag is allowed but functions in a reduced way
 | CmdLineOnly         -- ^ Flag is only allowed on command line, not in pragma
 | NeverAllowed        -- ^ Flag isn't allowed at all
 deriving ( Eq, Ord )

determineSafeLevel :: Bool -> FlagSafety
determineSafeLevel False = RestrictedFunction
determineSafeLevel True  = CmdLineOnly

flagA, flagR, flagC, flagN :: String -> OptKind m -> Flag m
flagA n o = Flag n AlwaysAllowed o
flagR n o = Flag n RestrictedFunction o
flagC n o = Flag n CmdLineOnly o
flagN n o = Flag n NeverAllowed o

-------------------------------
data OptKind m                      -- Suppose the flag is -f
 = NoArg     (EwM m ())                 -- -f all by itself
 | HasArg    (String -> EwM m ())       -- -farg or -f arg
 | SepArg    (String -> EwM m ())       -- -f arg
 | Prefix    (String -> EwM m ())       -- -farg
 | OptPrefix (String -> EwM m ())       -- -f or -farg (i.e. the arg is optional)
 | OptIntSuffix (Maybe Int -> EwM m ()) -- -f or -f=n; pass n to fn
 | IntSuffix (Int -> EwM m ())          -- -f or -f=n; pass n to fn
 | PassFlag  (String -> EwM m ())       -- -f; pass "-f" fn
 | AnySuffix (String -> EwM m ())       -- -f or -farg; pass entire "-farg" to fn
 | PrefixPred    (String -> Bool) (String -> EwM m ())
 | AnySuffixPred (String -> Bool) (String -> EwM m ())


--------------------------------------------------------
--	   The EwM monad 
--------------------------------------------------------

type Err   = Located String
type Warn  = Located String
type Errs  = Bag Err
type Warns = Bag Warn

-- EwM (short for "errors and warnings monad") is a
-- monad transformer for m that adds an (err, warn) state
newtype EwM m a = EwM { unEwM :: Located String	    -- Current arg
                              -> FlagSafety         -- arg safety level
                              -> FlagSafety         -- global safety level
                              -> Errs -> Warns
                              -> m (Errs, Warns, a) }

instance Monad m => Monad (EwM m) where
  (EwM f) >>= k = EwM (\l s c e w -> do { (e', w', r) <- f l s c e w
                                        ; unEwM (k r) l s c e' w' })
  return v = EwM (\_ _ _ e w -> return (e, w, v))

setArg :: Monad m => Located String -> FlagSafety -> EwM m () -> EwM m ()
setArg l s (EwM f) = EwM (\_ _ c es ws ->
    let check | s <= c    = f l s c es ws
              | otherwise = err l es ws
        err (L loc ('-' : arg)) es ws =
            let msg = "Warning: " ++ arg ++ " is not allowed in "
                   ++ "Safe Haskell; ignoring " ++ arg
            in return (es, ws `snocBag` L loc msg, ())
        err _ _ _ = error "Bad pattern match in setArg"
    in check)

addErr :: Monad m => String -> EwM m ()
addErr e = EwM (\(L loc _) _ _ es ws -> return (es `snocBag` L loc e, ws, ()))

addWarn :: Monad m => String -> EwM m ()
addWarn msg = EwM (\(L loc _) _ _ es ws -> return (es, ws `snocBag` L loc w, ()))
  where
    w = "Warning: " ++ msg

deprecate :: Monad m => String -> EwM m ()
deprecate s 
  = do { arg <- getArg
       ; addWarn (arg ++ " is deprecated: " ++ s) }

getArg :: Monad m => EwM m String
getArg = EwM (\(L _ arg) _ _ es ws -> return (es, ws, arg))

liftEwM :: Monad m => m a -> EwM m a
liftEwM action = EwM (\_ _ _ es ws -> do { r <- action; return (es, ws, r) })

-- -----------------------------------------------------------------------------
-- A state monad for use in the command-line parser
-- (CmdLineP s) typically instantiates the 'm' in (EwM m) and (OptKind m)

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


--------------------------------------------------------
--	   Processing arguments
--------------------------------------------------------

processArgs :: Monad m
            => [Flag m] -- cmdline parser spec
            -> [Located String]      -- args
            -> FlagSafety            -- flag clearance lvl
            -> Bool
            -> m (
                  [Located String],  -- spare args
                  [Located String],  -- errors
                  [Located String]   -- warnings
                 )
processArgs spec args clvl0 cmdline
  = let (clvl1, action) = process clvl0 args []
    in do { (errs, warns, spare) <- unEwM action (panic "processArgs: no arg yet")
                                    AlwaysAllowed clvl1 emptyBag emptyBag
          ; return (spare, bagToList errs, bagToList warns) }
  where
    -- process :: FlagSafety -> [Located String] -> [Located String] -> (FlagSafety, EwM m [Located String])
    --
    process clvl [] spare = (clvl, return (reverse spare))

    process clvl (locArg@(L _ ('-' : arg)) : args) spare =
      case findArg spec arg of
        Just (rest, opt_kind, fsafe) ->
           let clvl1 = if fsafe == EnablesSafe then determineSafeLevel cmdline else clvl
           in case processOneArg opt_kind rest arg args of
               Left err ->
                   let (clvl2,b) = process clvl1 args spare
                       clvl3 = min clvl1 clvl2
                   in (clvl3, (setArg locArg fsafe $ addErr err) >> b)

               Right (action,rest) ->
                   let (clvl2,b) = process clvl1 rest spare
                       clvl3 = min clvl1 clvl2
                   in (clvl3, (setArg locArg fsafe $ action) >> b)

        Nothing -> process clvl args (locArg : spare) 

    process clvl (arg : args) spare = process clvl args (arg : spare) 


processOneArg :: OptKind m -> String -> String -> [Located String]
              -> Either String (EwM m (), [Located String])
processOneArg opt_kind rest arg args
  = let dash_arg = '-' : arg
        rest_no_eq = dropEq rest
    in case opt_kind of
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


findArg :: [Flag m] -> String -> Maybe (String, OptKind m, FlagSafety)
findArg spec arg
  = case [ (removeSpaces rest, optKind, flagSafe)
         | flag <- spec,
           let optKind  = flagOptKind flag,
           let flagSafe = flagSafety flag,
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

-- ---------------------------------------------------------------------
-- Utils

errorsToGhcException :: [Located String] -> GhcException
errorsToGhcException errs =
   let errors = vcat [ ppr l <> text ": " <> text e | L l e <- errs ]
   in UsageError (renderWithStyle errors cmdlineParserStyle)

