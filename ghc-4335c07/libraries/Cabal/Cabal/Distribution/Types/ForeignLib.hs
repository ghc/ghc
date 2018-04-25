{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.ForeignLib(
    ForeignLib(..),
    emptyForeignLib,
    foreignLibModules,
    foreignLibIsShared,
    foreignLibVersion,

    LibVersionInfo,
    mkLibVersionInfo,
    libVersionInfoCRA,
    libVersionNumber,
    libVersionNumberShow,
    libVersionMajor
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.System
import Distribution.Text
import Distribution.Types.BuildInfo
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.UnqualComponentName
import Distribution.Version

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP  as Parse
import qualified Text.PrettyPrint           as Disp
import qualified Text.Read                  as Read

import qualified Distribution.Types.BuildInfo.Lens as L

-- | A foreign library stanza is like a library stanza, except that
-- the built code is intended for consumption by a non-Haskell client.
data ForeignLib = ForeignLib {
      -- | Name of the foreign library
      foreignLibName       :: UnqualComponentName
      -- | What kind of foreign library is this (static or dynamic).
    , foreignLibType       :: ForeignLibType
      -- | What options apply to this foreign library (e.g., are we
      -- merging in all foreign dependencies.)
    , foreignLibOptions    :: [ForeignLibOption]
      -- | Build information for this foreign library.
    , foreignLibBuildInfo  :: BuildInfo
      -- | Libtool-style version-info data to compute library version.
      -- Refer to the libtool documentation on the
      -- current:revision:age versioning scheme.
    , foreignLibVersionInfo :: Maybe LibVersionInfo
      -- | Linux library version
    , foreignLibVersionLinux :: Maybe Version

      -- | (Windows-specific) module definition files
      --
      -- This is a list rather than a maybe field so that we can flatten
      -- the condition trees (for instance, when creating an sdist)
    , foreignLibModDefFile :: [FilePath]
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

data LibVersionInfo = LibVersionInfo Int Int Int deriving (Data, Eq, Generic, Typeable)

instance Ord LibVersionInfo where
    LibVersionInfo c r _ `compare` LibVersionInfo c' r' _ =
        case c `compare` c' of
            EQ -> r `compare` r'
            e  -> e

instance Show LibVersionInfo where
    showsPrec d (LibVersionInfo c r a) = showParen (d > 10)
        $ showString "mkLibVersionInfo "
        . showsPrec 11 (c,r,a)

instance Read LibVersionInfo where
    readPrec = Read.parens $ do
        Read.Ident "mkLibVersionInfo" <- Read.lexP
        t <- Read.step Read.readPrec
        return (mkLibVersionInfo t)

instance Binary LibVersionInfo

instance Pretty LibVersionInfo where
    pretty (LibVersionInfo c r a)
      = Disp.hcat $ Disp.punctuate (Disp.char ':') $ map Disp.int [c,r,a]

instance Parsec LibVersionInfo where
    parsec = do
        c <- P.integral
        (r, a) <- P.option (0,0) $ do
            _ <- P.char ':'
            r <- P.integral
            a <- P.option 0 $ do
                _ <- P.char ':'
                P.integral
            return (r,a)
        return $ mkLibVersionInfo (c,r,a)

instance Text LibVersionInfo where
    parse = do
        c <- parseNat
        (r, a) <- Parse.option (0,0) $ do
            _ <- Parse.char ':'
            r <- parseNat
            a <- Parse.option 0 (Parse.char ':' >> parseNat)
            return (r, a)
        return $ mkLibVersionInfo (c,r,a)
      where
        parseNat = read `fmap` Parse.munch1 isDigit

-- | Construct 'LibVersionInfo' from @(current, revision, age)@
-- numbers.
--
-- For instance, @mkLibVersionInfo (3,0,0)@ constructs a
-- 'LibVersionInfo' representing the version-info @3:0:0@.
--
-- All version components must be non-negative.
mkLibVersionInfo :: (Int, Int, Int) -> LibVersionInfo
mkLibVersionInfo (c,r,a) = LibVersionInfo c r a

-- | From a given 'LibVersionInfo', extract the @(current, revision,
-- age)@ numbers.
libVersionInfoCRA :: LibVersionInfo -> (Int, Int, Int)
libVersionInfoCRA (LibVersionInfo c r a) = (c,r,a)

-- | Given a version-info field, produce a @major.minor.build@ version
libVersionNumber :: LibVersionInfo -> (Int, Int, Int)
libVersionNumber (LibVersionInfo c r a) = (c-a , a , r)

-- | Given a version-info field, return @"major.minor.build"@ as a
-- 'String'
libVersionNumberShow :: LibVersionInfo -> String
libVersionNumberShow v =
    let (major, minor, build) = libVersionNumber v
    in show major ++ "." ++ show minor ++ "." ++ show build

-- | Return the @major@ version of a version-info field.
libVersionMajor :: LibVersionInfo -> Int
libVersionMajor (LibVersionInfo c _ a) = c-a

instance L.HasBuildInfo ForeignLib where
    buildInfo f l = (\x -> l { foreignLibBuildInfo = x }) <$> f (foreignLibBuildInfo l)

instance Binary ForeignLib

instance Semigroup ForeignLib where
  a <> b = ForeignLib {
      foreignLibName         = combine'  foreignLibName
    , foreignLibType         = combine   foreignLibType
    , foreignLibOptions      = combine   foreignLibOptions
    , foreignLibBuildInfo    = combine   foreignLibBuildInfo
    , foreignLibVersionInfo  = combine'' foreignLibVersionInfo
    , foreignLibVersionLinux = combine'' foreignLibVersionLinux
    , foreignLibModDefFile   = combine   foreignLibModDefFile
    }
    where combine field = field a `mappend` field b
          combine' field = case ( unUnqualComponentName $ field a
                                , unUnqualComponentName $ field b) of
            ("", _) -> field b
            (_, "") -> field a
            (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"
          combine'' field = field b

instance Monoid ForeignLib where
  mempty = ForeignLib {
      foreignLibName         = mempty
    , foreignLibType         = ForeignLibTypeUnknown
    , foreignLibOptions      = []
    , foreignLibBuildInfo    = mempty
    , foreignLibVersionInfo  = Nothing
    , foreignLibVersionLinux = Nothing
    , foreignLibModDefFile   = []
    }
  mappend = (<>)

-- | An empty foreign library.
emptyForeignLib :: ForeignLib
emptyForeignLib = mempty

-- | Modules defined by a foreign library.
foreignLibModules :: ForeignLib -> [ModuleName]
foreignLibModules = otherModules . foreignLibBuildInfo

-- | Is the foreign library shared?
foreignLibIsShared :: ForeignLib -> Bool
foreignLibIsShared = foreignLibTypeIsShared . foreignLibType

-- | Get a version number for a foreign library.
-- If we're on Linux, and a Linux version is specified, use that.
-- If we're on Linux, and libtool-style version-info is specified, translate
-- that field into appropriate version numbers.
-- Otherwise, this feature is unsupported so we don't return any version data.
foreignLibVersion :: ForeignLib -> OS -> [Int]
foreignLibVersion flib Linux =
  case foreignLibVersionLinux flib of
    Just v  -> versionNumbers v
    Nothing ->
      case foreignLibVersionInfo flib of
        Just v' ->
          let (major, minor, build) = libVersionNumber v'
          in [major, minor, build]
        Nothing -> []
foreignLibVersion _ _ = []
