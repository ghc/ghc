{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.MungedPackageName
  ( MungedPackageName, unMungedPackageName, mkMungedPackageName
  , computeCompatPackageName
  , decodeCompatPackageName
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import Distribution.ParseUtils
import Distribution.Text
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

-- | A combination of a package and component name used in various legacy
-- interfaces, chiefly bundled with a version as 'MungedPackageId'. It's generally
-- better to use a 'UnitId' to opaquely refer to some compilation/packing unit,
-- but that doesn't always work, e.g. where a "name" is needed, in which case
-- this can be used as a fallback.
--
-- Use 'mkMungedPackageName' and 'unMungedPackageName' to convert from/to a 'String'.
--
-- @since 2.0.0.2
newtype MungedPackageName = MungedPackageName ShortText
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Convert 'MungedPackageName' to 'String'
unMungedPackageName :: MungedPackageName -> String
unMungedPackageName (MungedPackageName s) = fromShortText s

-- | Construct a 'MungedPackageName' from a 'String'
--
-- 'mkMungedPackageName' is the inverse to 'unMungedPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'MungedPackageName' is valid
--
-- @since 2.0.0.2
mkMungedPackageName :: String -> MungedPackageName
mkMungedPackageName = MungedPackageName . toShortText

-- | 'mkMungedPackageName'
--
-- @since 2.0.0.2
instance IsString MungedPackageName where
  fromString = mkMungedPackageName

instance Binary MungedPackageName

instance Text MungedPackageName where
  disp = Disp.text . unMungedPackageName
  parse = mkMungedPackageName <$> parsePackageName

instance NFData MungedPackageName where
    rnf (MungedPackageName pkg) = rnf pkg

-- | Computes the package name for a library.  If this is the public
-- library, it will just be the original package name; otherwise,
-- it will be a munged package name recording the original package
-- name as well as the name of the internal library.
--
-- A lot of tooling in the Haskell ecosystem assumes that if something
-- is installed to the package database with the package name 'foo',
-- then it actually is an entry for the (only public) library in package
-- 'foo'.  With internal packages, this is not necessarily true:
-- a public library as well as arbitrarily many internal libraries may
-- come from the same package.  To prevent tools from getting confused
-- in this case, the package name of these internal libraries is munged
-- so that they do not conflict the public library proper.  A particular
-- case where this matters is ghc-pkg: if we don't munge the package
-- name, the inplace registration will OVERRIDE a different internal
-- library.
--
-- We munge into a reserved namespace, "z-", and encode both the
-- component name and the package name of an internal library using the
-- following format:
--
--      compat-pkg-name ::= "z-" package-name "-z-" library-name
--
-- where package-name and library-name have "-" ( "z" + ) "-"
-- segments encoded by adding an extra "z".
--
-- When we have the public library, the compat-pkg-name is just the
-- package-name, no surprises there!
--
computeCompatPackageName :: PackageName -> Maybe UnqualComponentName -> MungedPackageName
-- First handle the cases where we can just use the original 'PackageName'.
-- This is for the PRIMARY library, and it is non-Backpack, or the
-- indefinite package for us.
computeCompatPackageName pkg_name Nothing
    = mkMungedPackageName $ unPackageName pkg_name
computeCompatPackageName pkg_name (Just uqn)
    = mkMungedPackageName $
         "z-" ++ zdashcode (unPackageName pkg_name) ++
        "-z-" ++ zdashcode (unUnqualComponentName uqn)

decodeCompatPackageName :: MungedPackageName -> (PackageName, Maybe UnqualComponentName)
decodeCompatPackageName m =
    case unMungedPackageName m of
        'z':'-':rest | [([pn, cn], "")] <- Parse.readP_to_S parseZDashCode rest
            -> (mkPackageName pn, Just (mkUnqualComponentName cn))
        s   -> (mkPackageName s, Nothing)

zdashcode :: String -> String
zdashcode s = go s (Nothing :: Maybe Int) []
    where go [] _ r = reverse r
          go ('-':z) (Just n) r | n > 0 = go z (Just 0) ('-':'z':r)
          go ('-':z) _        r = go z (Just 0) ('-':r)
          go ('z':z) (Just n) r = go z (Just (n+1)) ('z':r)
          go (c:z)   _        r = go z Nothing (c:r)

parseZDashCode :: Parse.ReadP r [String]
parseZDashCode = do
    ns <- Parse.sepBy1 (Parse.many1 (Parse.satisfy (/= '-'))) (Parse.char '-')
    Parse.eof
    return (go ns)
  where
    go ns = case break (=="z") ns of
                (_, []) -> [paste ns]
                (as, "z":bs) -> paste as : go bs
                _ -> error "parseZDashCode: go"
    unZ :: String -> String
    unZ "" = error "parseZDashCode: unZ"
    unZ r@('z':zs) | all (=='z') zs = zs
                   | otherwise      = r
    unZ r = r
    paste :: [String] -> String
    paste = intercalate "-" . map unZ
