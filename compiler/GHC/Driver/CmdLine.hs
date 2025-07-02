{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------
--
-- | Command-line parser
--
-- This is an abstract command-line parser used by DynFlags.
--
-- (c) The University of Glasgow 2005
--
-------------------------------------------------------------------------------

module GHC.Driver.CmdLine
    (
      processArgs, parseResponseFile, OptKind(..), GhcFlagMode(..),
      Flag(..), defFlag, defGhcFlag, defGhciFlag, defHiddenFlag, hoistFlag,
      errorsToGhcException,

      Err(..), Warn, warnsToMessages,

      EwM, runEwM, addErr, addWarn, addFlagWarn, getArg, getCurLoc, liftEwM
    ) where

import GHC.Prelude

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Bag
import GHC.Types.SrcLoc
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Driver.Errors.Types
import GHC.Driver.Errors.Ppr () -- instance Diagnostic DriverMessage
import GHC.Utils.Outputable (text)

import Data.Function
import Data.List (sortBy, intercalate, stripPrefix)
import Data.Word

import GHC.ResponseFile
import Control.Exception (IOException, catch)
import Control.Monad (ap)
import Control.Monad.IO.Class

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

hoistFlag :: forall m n. (forall a. m a -> n a) -> Flag m -> Flag n
hoistFlag f (Flag a b c) = Flag a (go b) c
  where
      go (NoArg k)  = NoArg (go2 k)
      go (HasArg k) = HasArg (\s -> go2 (k s))
      go (SepArg k) = SepArg (\s -> go2 (k s))
      go (Prefix k) = Prefix (\s -> go2 (k s))
      go (OptPrefix k) = OptPrefix (\s -> go2 (k s))
      go (OptIntSuffix k) = OptIntSuffix (\n -> go2 (k n))
      go (IntSuffix k) = IntSuffix (\n -> go2 (k n))
      go (Word64Suffix k) = Word64Suffix (\s -> go2 (k s))
      go (FloatSuffix k) = FloatSuffix (\s -> go2 (k s))
      go (PassFlag k) = PassFlag (\s -> go2 (k s))
      go (AnySuffix k) = AnySuffix (\s -> go2 (k s))

      go2 :: EwM m a -> EwM n a
      go2 (EwM g) = EwM $ \loc es ws -> f (g loc es ws)

-- | GHC flag modes describing when a flag has an effect.
data GhcFlagMode
    = OnlyGhc  -- ^ The flag only affects the non-interactive GHC
    | OnlyGhci -- ^ The flag only affects the interactive GHC
    | AllModes -- ^ The flag affects multiple ghc modes
    | HiddenFlag -- ^ This flag should not be seen in cli completion

data OptKind m                             -- Suppose the flag is -f
    = NoArg     (EwM m ())                 -- -f all by itself
    | HasArg    (String -> EwM m ())       -- -farg or -f=arg or -f arg
    | SepArg    (String -> EwM m ())       -- -f arg
    | Prefix    (String -> EwM m ())       -- -farg or -f=arg
    | OptPrefix (String -> EwM m ())       -- -f or -farg or -f=arg (i.e. the arg is optional)
    | OptIntSuffix (Maybe Int -> EwM m ()) -- -f or -fn or -f=n; pass n to fn
    | IntSuffix (Int -> EwM m ())          -- -fn or -f=n; pass n to fn
    | Word64Suffix (Word64 -> EwM m ())    -- -fn or -f=n; pass n to fn
    | FloatSuffix (Float -> EwM m ())      -- -fn or -f=n; pass n to fn
    | PassFlag  (String -> EwM m ())       -- -f; pass "-f" fn
    | AnySuffix (String -> EwM m ())       -- -f or -farg; pass entire "-farg" to fn


--------------------------------------------------------
--         The EwM monad
--------------------------------------------------------

-- | A command-line error message
newtype Err  = Err { errMsg :: Located String }

-- | A command-line warning message and the reason it arose
--
-- This used to be own type, but now it's just @'MsgEnvelope' 'DriverMessage'@.
type Warn = Located DriverMessage

type Errs  = Bag Err
type Warns = [Warn]

-- EwM ("errors and warnings monad") is a monad
-- transformer for m that adds an (err, warn) state
newtype EwM m a = EwM { unEwM :: Located String -- Current parse arg
                              -> Errs -> Warns
                              -> m (Errs, Warns, a) }
  deriving (Functor)

instance Monad m => Applicative (EwM m) where
    pure v = EwM (\_ e w -> return (e, w, v))
    (<*>) = ap

instance Monad m => Monad (EwM m) where
    (EwM f) >>= k = EwM (\l e w -> do (e', w', r) <- f l e w
                                      unEwM (k r) l e' w')
instance MonadIO m => MonadIO (EwM m) where
    liftIO = liftEwM . liftIO

runEwM :: EwM m a -> m (Errs, Warns, a)
runEwM action = unEwM action (panic "processArgs: no arg yet") emptyBag mempty

setArg :: Located String -> EwM m () -> EwM m ()
setArg l (EwM f) = EwM (\_ es ws -> f l es ws)

addErr :: Monad m => String -> EwM m ()
addErr e = EwM (\(L loc _) es ws -> return (es `snocBag` Err (L loc e), ws, ()))

addWarn :: Monad m => String -> EwM m ()
addWarn msg = addFlagWarn $ DriverUnknownMessage $ mkSimpleUnknownDiagnostic $
  mkPlainDiagnostic WarningWithoutFlag noHints $ text msg

addFlagWarn :: Monad m => DriverMessage -> EwM m ()
addFlagWarn msg = EwM
  (\(L loc _) es ws -> return (es, L loc msg : ws, ()))

getArg :: Monad m => EwM m String
getArg = EwM (\(L _ arg) es ws -> return (es, ws, arg))

getCurLoc :: Monad m => EwM m SrcSpan
getCurLoc = EwM (\(L loc _) es ws -> return (es, ws, loc))

liftEwM :: Monad m => m a -> EwM m a
liftEwM action = EwM (\_ es ws -> do { r <- action; return (es, ws, r) })

warnsToMessages :: DiagOpts -> [Warn] -> Messages DriverMessage
warnsToMessages diag_opts = foldr
  (\(L loc w) ws -> addMessage (mkPlainMsgEnvelope diag_opts loc w) ws)
  emptyMessages

--------------------------------------------------------
--         Processing arguments
--------------------------------------------------------

processArgs :: Monad m
            => [Flag m]               -- ^ cmdline parser spec
            -> [Located String]       -- ^ args
            -> (FilePath -> EwM m [Located String]) -- ^ response file handler
            -> m ( [Located String],  -- spare args
                   [Err],  -- errors
                   Warns ) -- warnings
processArgs spec args handleRespFile = do
    (errs, warns, spare) <- runEwM action
    return (spare, bagToList errs, warns)
  where
    action = process args []

    -- process :: [Located String] -> [Located String] -> EwM m [Located String]
    process [] spare = return (reverse spare)

    process (L _ ('@' : resp_file) : args) spare = do
        resp_args <- handleRespFile resp_file
        process (resp_args ++ args) spare

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
        NoArg  a -> assert (null rest) Right (a, args)

        HasArg f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise -> case args of
                                    []               -> missingArgErr dash_arg
                                    (L _ arg1:args1) -> Right (f arg1, args1)

        -- See #9776
        SepArg f -> case args of
                        []               -> missingArgErr dash_arg
                        (L _ arg1:args1) -> Right (f arg1, args1)

        -- See #12625
        Prefix f | notNull rest_no_eq -> Right (f rest_no_eq, args)
                 | otherwise          -> missingArgErr  dash_arg

        PassFlag f  | notNull rest -> unknownFlagErr dash_arg
                    | otherwise    -> Right (f dash_arg, args)

        OptIntSuffix f | null rest                     -> Right (f Nothing,  args)
                       | Just n <- parseInt rest_no_eq -> Right (f (Just n), args)
                       | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        IntSuffix f | Just n <- parseInt rest_no_eq -> Right (f n, args)
                    | otherwise -> Left ("malformed integer argument in " ++ dash_arg)

        Word64Suffix f | Just n <- parseWord64 rest_no_eq -> Right (f n, args)
                     | otherwise -> Left ("malformed natural argument in " ++ dash_arg)

        FloatSuffix f | Just n <- parseFloat rest_no_eq -> Right (f n, args)
                      | otherwise -> Left ("malformed float argument in " ++ dash_arg)

        OptPrefix f       -> Right (f rest_no_eq, args)
        AnySuffix f       -> Right (f dash_arg, args)

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
arg_ok (Prefix          _)  _    _   = True -- Missing argument checked for in processOneArg t
                                            -- to improve error message (#12625)
arg_ok (OptIntSuffix    _)  _    _   = True
arg_ok (IntSuffix       _)  _    _   = True
arg_ok (Word64Suffix    _)  _    _   = True
arg_ok (FloatSuffix     _)  _    _   = True
arg_ok (OptPrefix       _)  _    _   = True
arg_ok (PassFlag        _)  rest _   = null rest
arg_ok (AnySuffix       _)  _    _   = True

-- | Parse an Int
--
-- Looks for "433" or "=342", with no trailing gubbins
--   * n or =n      => Just n
--   * gibberish    => Nothing
parseInt :: String -> Maybe Int
parseInt s = case reads s of
                 ((n,""):_) -> Just n
                 _          -> Nothing

parseWord64 :: String -> Maybe Word64
parseWord64 s = case reads s of
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

-- | Parse a response file into arguments.
parseResponseFile :: MonadIO m => FilePath -> EwM m [Located String]
parseResponseFile path = do
  res <- liftIO $ fmap Right (readFile path) `catch`
    \(e :: IOException) -> pure (Left e)
  case res of
    Left _err -> addErr "Could not open response file" >> return []
    Right resp_file -> return $ map (mkGeneralLocated path) (unescapeArgs resp_file)

-- See Note [Handling errors when parsing command-line flags]
errorsToGhcException :: [(String,    -- Location
                          String)]   -- Error
                     -> GhcException
errorsToGhcException errs =
    UsageError $ intercalate "\n" $ [ l ++ ": " ++ e | (l, e) <- errs ]

{- Note [Handling errors when parsing command-line flags]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
