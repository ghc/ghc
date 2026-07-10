{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | A module for parsing and using config files in a Shake build system. Config files
--   consist of variable bindings, for example:
--
-- > # This is my Config file
-- > HEADERS_DIR = /path/to/dir
-- > CFLAGS = -g -I${HEADERS_DIR}
-- > CFLAGS = $CFLAGS -O2
-- > include extra/file.cfg
--
--   This defines the variable @HEADERS_DIR@ (equal to @\/path\/to\/dir@), and
--   @CFLAGS@ (equal to @-g -I\/path\/to\/dir -O2@), and also includes the configuration
--   statements in the file @extra/file.cfg@. The full lexical syntax for configuration
--   files is defined here: <https://ninja-build.org/manual.html#_lexical_syntax>.
--   The use of Ninja file syntax is due to convenience and the desire to reuse an
--    externally-defined specification (but the choice of configuration language is mostly arbitrary).
--
--   To use the configuration file either use 'readConfigFile' to parse the configuration file
--   and use the values directly, or 'usingConfigFile' and 'getConfig' to track the configuration
--   values, so they become build dependencies.
module Development.Shake.Config(
    readConfigFile, readConfigFileWithEnv,
    usingConfigFile, usingConfig,
    getConfig, getConfigKeys
    ) where

import Development.Shake
import Development.Shake.Classes
import qualified Development.Ninja.Parse as Ninja
import qualified Development.Ninja.Env as Ninja
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.UTF8 as UTF8
import Data.Tuple.Extra
import Data.List


-- | Read a config file, returning a list of the variables and their bindings.
--   Config files use the Ninja lexical syntax:
--   <https://ninja-build.org/manual.html#_lexical_syntax>
readConfigFile :: FilePath -> IO (Map.HashMap String String)
readConfigFile = readConfigFileWithEnv []


-- | Read a config file with an initial environment, returning a list of the variables and their bindings.
--   Config files use the Ninja lexical syntax:
--   <https://ninja-build.org/manual.html#_lexical_syntax>
readConfigFileWithEnv :: [(String, String)] -> FilePath -> IO (Map.HashMap String String)
readConfigFileWithEnv vars file = do
    env <- Ninja.newEnv
    mapM_ (uncurry (Ninja.addEnv env) . (UTF8.fromString *** UTF8.fromString)) vars
    Ninja.parse file env
    mp <- Ninja.fromEnv env
    pure $ Map.fromList $ map (UTF8.toString *** UTF8.toString) $ Map.toList mp


newtype Config = Config String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype ConfigKeys = ConfigKeys () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult Config = Maybe String
type instance RuleResult ConfigKeys = [String]


-- | Specify the file to use with 'getConfig'.
usingConfigFile :: FilePath -> Rules ()
usingConfigFile file = do
    mp <- newCache $ \() -> do
        need [file]
        liftIO $ readConfigFile file
    addOracle $ \(Config x) -> Map.lookup x <$> mp ()
    addOracle $ \(ConfigKeys ()) -> sort . Map.keys <$> mp ()
    pure ()


-- | Specify the values to use with 'getConfig', generally prefer
--   'usingConfigFile' unless you also need access to the values
--   of variables outside 'Action'.
usingConfig :: Map.HashMap String String -> Rules ()
usingConfig mp = do
    addOracle $ \(Config x) -> pure $ Map.lookup x mp
    addOracle $ \(ConfigKeys ()) -> pure $ sort $ Map.keys mp
    pure ()


-- | Obtain the value of a configuration variable, returns 'Nothing' to indicate the variable
--   has no binding. Any build system using 'getConfig' /must/ call either 'usingConfigFile'
--   or 'usingConfig'. The 'getConfig' function will introduce a dependency on the configuration
--   variable (but not the whole configuration file), and if the configuration variable changes, the rule will be rerun.
--   As an example:
--
-- @
-- 'usingConfigFile' \"myconfiguration.cfg\"
-- \"*.o\" '%>' \\out -> do
--     cflags <- 'getConfig' \"CFLAGS\"
--     'cmd' \"gcc\" [out '-<.>' \"c\"] (fromMaybe \"\" cflags)
-- @
getConfig :: String -> Action (Maybe String)
getConfig = askOracle . Config


-- | Obtain the configuration keys.
--   Any build system using 'getConfigKeys' /must/ call either 'usingConfigFile' or 'usingConfig'.
--   The 'getConfigKeys' function will introduce a dependency on the configuration keys
--   (but not the whole configuration file), and if the configuration keys change, the rule will be rerun.
--   Usually use as part of an action.
--   As an example:
--
-- @
-- 'usingConfigFile' \"myconfiguration.cfg\"
-- 'action' $ need =<< getConfigKeys
-- @
getConfigKeys :: Action [String]
getConfigKeys = askOracle $ ConfigKeys ()
