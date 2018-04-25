{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.License
-- Description :  The License data type.
-- Copyright   :  Isaac Jones 2003-2005
--                Duncan Coutts 2008
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Package descriptions contain fields for specifying the name of a software
-- license and the name of the file containing the text of that license. While
-- package authors may choose any license they like, Cabal provides an
-- enumeration of a small set of common free and open source software licenses.
-- This is done so that Hackage can recognise licenses, so that tools can detect
-- <https://en.wikipedia.org/wiki/License_compatibility licensing conflicts>,
-- and to deter
-- <https://en.wikipedia.org/wiki/License_proliferation license proliferation>.
--
-- It is recommended that all package authors use the @license-file@ or
-- @license-files@ fields in their package descriptions. Further information
-- about these fields can be found in the
-- <http://www.haskell.org/cabal/users-guide/developing-packages.html#package-descriptions Cabal users guide>.
--
-- = Additional resources
--
-- The following websites provide information about free and open source
-- software licenses:
--
-- * <http://www.opensource.org The Open Source Initiative (OSI)>
--
-- * <https://www.fsf.org The Free Software Foundation (FSF)>
--
-- = Disclaimer
--
-- The descriptions of software licenses provided by this documentation are
-- intended for informational purposes only and in no way constitute legal
-- advice. Please read the text of the licenses and consult a lawyer for any
-- advice regarding software licensing.

module Distribution.License (
    License(..),
    knownLicenses,
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Distribution.Version

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP  as Parse
import qualified Text.PrettyPrint           as Disp

-- | Indicates the license under which a package's source code is released.
-- Versions of the licenses not listed here will be rejected by Hackage and
-- cause @cabal check@ to issue a warning.
data License =
    -- TODO: * remove BSD4

    -- | GNU General Public License,
    -- <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html version 2> or
    -- <https://www.gnu.org/licenses/gpl.html version 3>.
    GPL (Maybe Version)

    -- | <https://www.gnu.org/licenses/agpl.html GNU Affero General Public License, version 3>.
  | AGPL (Maybe Version)

    -- | GNU Lesser General Public License,
    -- <https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html version 2.1> or
    -- <https://www.gnu.org/licenses/lgpl.html version 3>.
  | LGPL (Maybe Version)

    -- | <http://www.opensource.org/licenses/bsd-license 2-clause BSD license>.
  | BSD2

    -- | <http://www.opensource.org/licenses/bsd-3-clause 3-clause BSD license>.
  | BSD3

    -- | <http://directory.fsf.org/wiki/License:BSD_4Clause 4-clause BSD license>.
    -- This license has not been approved by the OSI and is incompatible with
    -- the GNU GPL. It is provided for historical reasons and should be avoided.
  | BSD4

    -- | <http://www.opensource.org/licenses/MIT MIT license>.
  | MIT

    -- | <http://www.isc.org/downloads/software-support-policy/isc-license/ ISC license>
  | ISC

    -- | <https://www.mozilla.org/MPL/ Mozilla Public License, version 2.0>.
  | MPL Version

    -- | <https://www.apache.org/licenses/ Apache License, version 2.0>.
  | Apache (Maybe Version)

    -- | The author of a package disclaims any copyright to its source code and
    -- dedicates it to the public domain. This is not a software license. Please
    -- note that it is not possible to dedicate works to the public domain in
    -- every jurisdiction, nor is a work that is in the public domain in one
    -- jurisdiction necessarily in the public domain elsewhere.
  | PublicDomain

    -- | Explicitly 'All Rights Reserved', eg for proprietary software. The
    -- package may not be legally modified or redistributed by anyone but the
    -- rightsholder.
  | AllRightsReserved

    -- | No license specified which legally defaults to 'All Rights Reserved'.
    -- The package may not be legally modified or redistributed by anyone but
    -- the rightsholder.
  | UnspecifiedLicense

    -- | Any other software license.
  | OtherLicense

    -- | Indicates an erroneous license name.
  | UnknownLicense String
  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary License

-- | The list of all currently recognised licenses.
knownLicenses :: [License]
knownLicenses = [ GPL  unversioned, GPL  (version [2]),    GPL  (version [3])
                , LGPL unversioned, LGPL (version [2, 1]), LGPL (version [3])
                , AGPL unversioned,                        AGPL (version [3])
                , BSD2, BSD3, MIT, ISC
                , MPL (mkVersion [2, 0])
                , Apache unversioned, Apache (version [2, 0])
                , PublicDomain, AllRightsReserved, OtherLicense]
 where
   unversioned = Nothing
   version     = Just . mkVersion

instance Pretty License where
  pretty (GPL  version)         = Disp.text "GPL"    <<>> dispOptVersion version
  pretty (LGPL version)         = Disp.text "LGPL"   <<>> dispOptVersion version
  pretty (AGPL version)         = Disp.text "AGPL"   <<>> dispOptVersion version
  pretty (MPL  version)         = Disp.text "MPL"    <<>> dispVersion    version
  pretty (Apache version)       = Disp.text "Apache" <<>> dispOptVersion version
  pretty (UnknownLicense other) = Disp.text other
  pretty other                  = Disp.text (show other)

instance Parsec License where
  parsec = do
    name    <- P.munch1 isAlphaNum
    version <- P.optionMaybe (P.char '-' *> parsec)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      )  -> GPL  version
      ("LGPL",              _      )  -> LGPL version
      ("AGPL",              _      )  -> AGPL version
      ("BSD2",              Nothing)  -> BSD2
      ("BSD3",              Nothing)  -> BSD3
      ("BSD4",              Nothing)  -> BSD4
      ("ISC",               Nothing)  -> ISC
      ("MIT",               Nothing)  -> MIT
      ("MPL",         Just version')  -> MPL version'
      ("Apache",            _      )  -> Apache version
      ("PublicDomain",      Nothing)  -> PublicDomain
      ("AllRightsReserved", Nothing)  -> AllRightsReserved
      ("OtherLicense",      Nothing)  -> OtherLicense
      _                               -> UnknownLicense $ name ++
                                         maybe "" (('-':) . display) version

instance Text License where
  parse = do
    name    <- Parse.munch1 (\c -> isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("AGPL",              _      ) -> AGPL version
      ("BSD2",              Nothing) -> BSD2
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("ISC",               Nothing) -> ISC
      ("MIT",               Nothing) -> MIT
      ("MPL",         Just version') -> MPL version'
      ("Apache",            _      ) -> Apache version
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name ++
                                        maybe "" (('-':) . display) version

dispOptVersion :: Maybe Version -> Disp.Doc
dispOptVersion Nothing  = Disp.empty
dispOptVersion (Just v) = dispVersion v

dispVersion :: Version -> Disp.Doc
dispVersion v = Disp.char '-' <<>> disp v
