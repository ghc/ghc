{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
--
-- | Command-line parser
--
-- This is an abstract command-line parser used by DynFlags.
--
-- (c) The University of Glasgow 2005
--
-------------------------------------------------------------------------------

module CmdLineParser
    (
      processArgs, OptKind(..), GhcFlagMode(..),
      CmdLineP(..), getCmdLineState, putCmdLineState,
      Flag(..), defFlag, defGhcFlag, defGhciFlag, defHiddenFlag,
      errorsToGhcException,

      Err(..), Warn(..), WarnReason(..),

      EwM, runEwM, addErr, addWarn, addFlagWarn, getArg, getCurLoc, liftEwM,
      deprecate
    ) where

#include "HsVersions.h"

import GhcPrelude

import Util
import Outputable
import Panic
import Bag
import SrcLoc
import Json

import Data.Function
import Data.List

import Control.Monad (liftM, ap)

--------------------------------------------------------
--         The Flag and OptKind types
--------------------------------------------------------

data Flag m = Flag
    {   flagName    :: String,     -- Flag, without the leading "-"
        flagOptKind :: OptKind m,  -- What to do if we see it
        flagGhcMode :: GhcFlagMode    -- Which modes this flag affects
    }

defFlag :: String -> OptKind m -> Flag m
defFlag name optKind = Flag name optKind AllModes

defGhcFlag :: String -> OptKind m -> Flag m
defGhcFlag name optKind = Flag name optKind OnlyGhc

defGhciFlag :: String -> OptKind m -> Flag m
defGhciFlag name optKind = Flag name optKind OnlyGhci

defHiddenFlag :: String -> OptKind m -> Flag m
defHiddenFlag name optKind = Flag name optKind HiddenFlag

-- | GHC flag modes describing when a flag has an effect.
data GhcFlagMode
    = OnlyGhc  -- ^ The flag only affects the non-interactive GHC
    | OnlyGhci -- ^ The flag only affects the interactive GHC
    | AllModes -- ^ The flag affects multiple ghc modes
    | HiddenFlag -- ^ This flag should not be seen in cli completion

data OptKind m                             -- Suppose the flag is -f
    = NoArg     (EwM m ())                 -- -f all by itself
    | HasArg    (String -> EwM m ())       -- -farg or -f arg
    | SepArg    (String -> EwM m ())       -- -f arg
    | Prefix    (String -> EwM m ())       -- -farg
    | OptPrefix (String -> EwM m ())       -- -f or -farg (i.e. the arg is optional)
    | OptIntSuffix (Maybe Int -> EwM m ()) -- -f or -f=n; pass n to fn
    | IntSuffix (Int -> EwM m ())          -- -f or -f=n; pass n to fn
    | FloatSuffix (Float -> EwM m ())      -- -f or -f=n; pass n to fn
    | PassFlag  (String -> EwM m ())       -- -f; pass "-f" fn
    | AnySuffix (String -> EwM m ())       -- -f or -farg; pass entire "-farg" to fn
    | PrefixPred    (String -> Bool) (String -> EwM m ())
    | AnySuffixPred (String -> Bool) (String -> EwM m ())


--------------------------------------------------------
--         The EwM monad
--------------------------------------------------------

-- | Used when filtering warnings: if a reason is given
-- it can be filtered out when displaying.
data WarnReason
  = NoReason
  | ReasonDeprecatedFlag
  | ReasonUnrecognisedFlag
  deriving (Eq, Show)

instance Outputable WarnReason where
  ppr = text . show

instance ToJson WarnReason where
  json NoReason = JSNull
  json reason   = JSString $ show reason

-- | A command-line error message
newtype Err  = Err { errMsg :: Located String }

-- | A command-line warning message and the reason it arose
data Warn = Warn
  {   warnReason :: WarnReason,
      warnMsg    :: Located String
  }

type Errs  = Bag Err
type Warns = Bag Warn

-- EwM ("errors and warnings monad") is a monad
-- transformer for m that adds an (err, warn) state
newtype EwM m a = EwM { unEwM :: Located String -- Current parse arg
                              -> Errs -> Warns
                              -> m (Errs, Warns, a) }

instance Monad m => Functor (EwM m) where
    fmap = liftM

instance Monad m => Applicative (EwM m) where
    pure v = EwM (\_ e w -> return (e, w, v))
    (<*>) = ap

instance Monad m => Monad (EwM m) where
    (EwM f) >>= k = EwM (\l e w -> do (e', w', r) <- f l e w
                                      unEwM (k r) l e' w')

runEwM :: EwM m a -> m (Errs, Warns, a)
runEwM action = unEwM action (panic "processArgs: no arg yet") emptyBag emptyBag

setArg :: Located String -> EwM m () -> EwM m ()
setArg l (EwM f) = EwM (\_ es ws -> f l es ws)

addErr :: Monad m => String -> EwM m ()
addErr e = EwM (\(L loc _) es ws -> return (es `snocBag` Err (L loc e), ws, ()))

addWarn :: Monad m => String -> EwM m ()
addWarn = addFlagWarn NoReason

addFlagWarn :: Monad m => WarnReason -> String -> EwM m ()
addFlagWarn reason msg = EwM $
  (\(L loc _) es ws -> return (es, ws `snocBag` Warn reason (L loc msg), ()))

deprecate :: Monad m => String -> EwM m ()
deprecate s = do
    arg <- getArg
    addFlagWarn ReasonDeprecatedFlag (arg ++ " is deprecated: " ++ s)

getArg :: Monad m => EwM m String
getArg = EwM (\(L _ arg) es ws -> return (es, ws, arg))

getCurLoc :: Monad m => EwM m SrcSpan
getCurLoc = EwM (\(L loc _) es ws -> return (es, ws, loc))

liftEwM :: Monad m => m a -> EwM m a
liftEwM action = EwM (\_ es ws -> do { r <- action; return (es, ws, r) })


--------------------------------------------------------
-- A state monad for use in the command-line parser
--------------------------------------------------------

-- (CmdLineP s) typically instantiates the 'm' in (EwM m) and (OptKind m)
newtype CmdLineP s a = CmdLineP { runCmdLine :: s -> (a, s) }

instance Functor (CmdLineP s) where
    fmap = liftM

instance Applicative (CmdLineP s) where
    pure a = CmdLineP $ \s -> (a, s)
    (<*>) = ap

instance Monad (CmdLineP s) where
    m >>= k = CmdLineP $ \s ->
                  let (a, s') = runCmdLine m s
                  in runCmdLine (k a) s'


getCmdLineState :: CmdLineP s s
getCmdLineState   = CmdLineP $ \s -> (s,s)
putCmdLineState :: s -> CmdLineP s ()
putCmdLineState s = CmdLineP $ \_ -> ((),s)


--------------------------------------------------------
--         Processing arguments
--------------------------------------------------------

processArgs :: Monad m
            => [Flag m]               -- cmdline parser spec
            -> [Located String]       -- args
            -> m ( [Located String],  -- spare args
                   [Err],  -- errors
                   [Warn] ) -- warnings
processArgs spec args = do
    (errs, warns, spare) <- runEwM action
    return (spare, bagToList errs, bagToList warns)
  where
    action = process args []

    -- process :: [Located String] -> [Located String] -> EwM m [Located String]
    process [] spare = return (reverse spare)

    process (locArg@(L _ ('-' : arg)) : args) spare =
        case findArg spec arg of
            Just (rest, opt_kind) ->
                case processOneArg opt_kind rest arg args of
                    Left err ->
                        let b = process args spare
                        in (setArg locArg $ addErr err) >> b

                    Right (action,rest) ->
                        let b = process rest spare
                        in (setArg locArg $ action) >> b

            Nothing -> process args (locArg : spare)

    process (arg : args) spare = process args (arg : spare)


processOneArg :: OptKind m -> String -> String -> [Located String]
              -> Either String (EwM m (), [Located String])
processOneArg opt_kind rest arg args
  = let dash_arg = '-' : arg
        rest_no_eq = dropEq rest
    in case opt_kind of
        NoArg  a -> ASSERT(null rest) Right (a, args)

        HasArg f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise -> case args of
                                    []               -> missingArgErr dash_arg
                                    (L _ arg1:args1) -> Right (f arg1, args1)

        -- See Trac #9776
        SepArg f -> case args of
                        []               -> missingArgErr dash_arg
                        (L _ arg1:args1) -> Right (f arg1, args1)

        Prefix f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise          -> unknownFlagErr dash_arg

        PrefixPred _ f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                       | otherwise          -> unknownFlagErr dash_arg

        PassFlag f  | notNull rest -> unknownFlagErr dash_arg
                    | otherwise    -> Right (f dash_arg, args)

        OptIntSuffix f | null rest                     -> Right (f Nothing,  args)
                       | Just n <- parseInt rest_no_eq -> Right (f (Just n), args)
                       | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        IntSuffix f | Just n <- parseInt rest_no_eq -> Right (f n, args)
                    | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        FloatSuffix f | Just n <- parseFloat rest_no_eq -> Right (f n, args)
                      | otherwise -> Left ("malformed float argument in " ++ dash_arg)

        OptPrefix f       -> Right (f rest_no_eq, args)
        AnySuffix f       -> Right (f dash_arg, args)
        AnySuffixPred _ f -> Right (f dash_arg, args)

findArg :: [Flag m] -> String -> Maybe (String, OptKind m)
findArg spec arg =
    case sortBy (compare `on` (length . fst)) -- prefer longest matching flag
           [ (removeSpaces rest, optKind)
           | flag <- spec,
             let optKind  = flagOptKind flag,
             Just rest <- [stripPrefix (flagName flag) arg],
             arg_ok optKind rest arg ]
    of
        []      -> Nothing
        (one:_) -> Just one

arg_ok :: OptKind t -> [Char] -> String -> Bool
arg_ok (NoArg           _)  rest _   = null rest
arg_ok (HasArg          _)  _    _   = True
arg_ok (SepArg          _)  rest _   = null rest
arg_ok (Prefix          _)  rest _   = notNull rest
arg_ok (PrefixPred p    _)  rest _   = notNull rest && p (dropEq rest)
arg_ok (OptIntSuffix    _)  _    _   = True
arg_ok (IntSuffix       _)  _    _   = True
arg_ok (FloatSuffix     _)  _    _   = True
arg_ok (OptPrefix       _)  _    _   = True
arg_ok (PassFlag        _)  rest _   = null rest
arg_ok (AnySuffix       _)  _    _   = True
arg_ok (AnySuffixPred p _)  _    arg = p arg

-- | Parse an Int
--
-- Looks for "433" or "=342", with no trailing gubbins
--   * n or =n      => Just n
--   * gibberish    => Nothing
parseInt :: String -> Maybe Int
parseInt s = case reads s of
                 ((n,""):_) -> Just n
                 _          -> Nothing

parseFloat :: String -> Maybe Float
parseFloat s = case reads s of
                   ((n,""):_) -> Just n
                   _          -> Nothing

-- | Discards a leading equals sign
dropEq :: String -> String
dropEq ('=' : s) = s
dropEq s         = s

unknownFlagErr :: String -> Either String a
unknownFlagErr f = Left ("unrecognised flag: " ++ f)

missingArgErr :: String -> Either String a
missingArgErr f = Left ("missing argument for flag: " ++ f)

--------------------------------------------------------
-- Utils
--------------------------------------------------------


-- See Note [Handling errors when parsing flags]
errorsToGhcException :: [(String,    -- Location
                          String)]   -- Error
                     -> GhcException
errorsToGhcException errs =
    UsageError $ intercalate "\n" $ [ l ++ ": " ++ e | (l, e) <- errs ]

{- Note [Handling errors when parsing commandline flags]

Parsing of static and mode flags happens before any session is started, i.e.,
before the first call to 'GHC.withGhc'. Therefore, to report errors for
invalid usage of these two types of flags, we can not call any function that
needs DynFlags, as there are no DynFlags available yet (unsafeGlobalDynFlags
is not set either). So we always print "on the commandline" as the location,
which is true except for Api users, which is probably ok.

When reporting errors for invalid usage of dynamic flags we /can/ make use of
DynFlags, and we do so explicitly in DynFlags.parseDynamicFlagsFull.

Before, we called unsafeGlobalDynFlags when an invalid (combination of)
flag(s) was given on the commandline, resulting in panics (#9963).
-}
