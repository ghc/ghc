{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ModuleName
-- Copyright   :  Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Data type for Haskell module names.

module Distribution.ModuleName (
        ModuleName (..), -- TODO: move Parsec instance here, don't export constructor
        fromString,
        fromComponents,
        components,
        toFilePath,
        main,
        simple,
        -- * Internal
        validModuleComponent,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Utils.ShortText
import System.FilePath ( pathSeparator )

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- | A valid Haskell module name.
--
newtype ModuleName = ModuleName ShortTextLst
  deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

instance Binary ModuleName

instance NFData ModuleName where
    rnf (ModuleName ms) = rnf ms

instance Pretty ModuleName where
  pretty (ModuleName ms) =
    Disp.hcat (intersperse (Disp.char '.') (map Disp.text $ stlToStrings ms))

instance Parsec ModuleName where
    parsec = fromComponents <$> P.sepBy1 component (P.char '.')
      where
        component = do
            c  <- P.satisfy isUpper
            cs <- P.munch validModuleChar
            return (c:cs)

instance Text ModuleName where
  parse = do
    ms <- Parse.sepBy1 component (Parse.char '.')
    return (ModuleName $ stlFromStrings ms)

    where
      component = do
        c  <- Parse.satisfy isUpper
        cs <- Parse.munch validModuleChar
        return (c:cs)

validModuleChar :: Char -> Bool
validModuleChar c = isAlphaNum c || c == '_' || c == '\''

validModuleComponent :: String -> Bool
validModuleComponent []     = False
validModuleComponent (c:cs) = isUpper c
                           && all validModuleChar cs

{-# DEPRECATED simple "use ModuleName.fromString instead" #-}
simple :: String -> ModuleName
simple str = ModuleName (stlFromStrings [str])

-- | Construct a 'ModuleName' from a valid module name 'String'.
--
-- This is just a convenience function intended for valid module strings. It is
-- an error if it is used with a string that is not a valid module name. If you
-- are parsing user input then use 'Distribution.Text.simpleParse' instead.
--
instance IsString ModuleName where
    fromString string = fromComponents (split string)
      where
        split cs = case break (=='.') cs of
          (chunk,[])     -> chunk : []
          (chunk,_:rest) -> chunk : split rest

-- | Construct a 'ModuleName' from valid module components, i.e. parts
-- separated by dots.
fromComponents :: [String] -> ModuleName
fromComponents components'
    | null components'                     = error zeroComponents
    | all validModuleComponent components' = ModuleName (stlFromStrings components')
    | otherwise                            = error badName
  where
    zeroComponents = "ModuleName.fromComponents: zero components"
    badName        = "ModuleName.fromComponents: invalid components " ++ show components'

-- | The module name @Main@.
--
main :: ModuleName
main = ModuleName (stlFromStrings ["Main"])

-- | The individual components of a hierarchical module name. For example
--
-- > components (fromString "A.B.C") = ["A", "B", "C"]
--
components :: ModuleName -> [String]
components (ModuleName ms) = stlToStrings ms

-- | Convert a module name to a file path, but without any file extension.
-- For example:
--
-- > toFilePath (fromString "A.B.C") = "A/B/C"
--
toFilePath :: ModuleName -> FilePath
toFilePath = intercalate [pathSeparator] . components

----------------------------------------------------------------------------
-- internal helper

-- | Strict/unpacked representation of @[ShortText]@
data ShortTextLst = STLNil
                  | STLCons !ShortText !ShortTextLst
                  deriving (Eq, Generic, Ord, Typeable, Data)

instance NFData ShortTextLst where
    rnf = flip seq ()

instance Show ShortTextLst where
    showsPrec p = showsPrec p . stlToList


instance Read ShortTextLst where
    readsPrec p = map (first stlFromList) . readsPrec p

instance Binary ShortTextLst where
    put = put . stlToList
    get = stlFromList <$> get

stlToList :: ShortTextLst -> [ShortText]
stlToList STLNil = []
stlToList (STLCons st next) = st : stlToList next

stlToStrings :: ShortTextLst -> [String]
stlToStrings = map fromShortText . stlToList

stlFromList :: [ShortText] -> ShortTextLst
stlFromList [] = STLNil
stlFromList (x:xs) = STLCons x (stlFromList xs)

stlFromStrings :: [String] -> ShortTextLst
stlFromStrings = stlFromList . map toShortText
