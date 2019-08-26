module OutputOptions where

import GhcPrelude

data OutputOptions = OutputOptions
  { hasPprDebug :: a -> Bool
  { hasNoDebugOutput :: a -> Bool
  { 1226   -- Output style options
  { 1227   pprUserLength         :: Int,
  { 1228   pprCols               :: Int,
  { 1229 
  { 1230   useUnicode            :: Bool,
  { 1231   useColor              :: OverridingBool,   
  { 1232   canUseColor           :: Bool,
1233   colScheme             :: Col.Scheme,


class HasOutputOptions a where
  hasPprDebug :: a -> Bool
  hasNoDebugOutput :: a -> Bool
