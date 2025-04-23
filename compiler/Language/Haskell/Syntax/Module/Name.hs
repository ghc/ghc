module Language.Haskell.Syntax.Module.Name where

import Prelude

import Data.Char (isAlphaNum)
import Data.Data
import Control.DeepSeq
import qualified Text.ParserCombinators.ReadP as Parse
import System.FilePath

import GHC.Data.FastString

-- | A ModuleName is essentially a simple string, e.g. @Data.List@.
newtype ModuleName = ModuleName FastString deriving (Show, Eq)

instance Data ModuleName where
  -- don't traverse?
  toConstr x   = constr
    where
      constr = mkConstr (dataTypeOf x) "{abstract:ModuleName}" [] Prefix
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "ModuleName"

instance Ord ModuleName where
  nm1 `compare` nm2 = stableModuleNameCmp nm1 nm2

instance NFData ModuleName where
  rnf x = x `seq` ()

stableModuleNameCmp :: ModuleName -> ModuleName -> Ordering
-- ^ Compares module names lexically, rather than by their 'Unique's
stableModuleNameCmp n1 n2 = moduleNameFS n1 `lexicalCompareFS` moduleNameFS n2

moduleNameFS :: ModuleName -> FastString
moduleNameFS (ModuleName mod) = mod

moduleNameString :: ModuleName -> String
moduleNameString (ModuleName mod) = unpackFS mod

mkModuleName :: String -> ModuleName
mkModuleName s = ModuleName (mkFastString s)

mkModuleNameFS :: FastString -> ModuleName
mkModuleNameFS s = ModuleName s

-- |Returns the string version of the module name, with dots replaced by slashes.
--
moduleNameSlashes :: ModuleName -> String
moduleNameSlashes = dots_to_slashes . moduleNameString
  where dots_to_slashes = map (\c -> if c == '.' then pathSeparator else c)

-- |Returns the string version of the module name, with dots replaced by colons.
--
moduleNameColons :: ModuleName -> String
moduleNameColons = dots_to_colons . moduleNameString
  where dots_to_colons = map (\c -> if c == '.' then ':' else c)

parseModuleName :: Parse.ReadP ModuleName
parseModuleName = fmap mkModuleName
                $ Parse.munch1 (\c -> isAlphaNum c || c `elem` "_.'")

