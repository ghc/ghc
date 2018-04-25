{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.System
-- Copyright   :  Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Cabal often needs to do slightly different things on specific platforms. You
-- probably know about the 'System.Info.os' however using that is very
-- inconvenient because it is a string and different Haskell implementations
-- do not agree on using the same strings for the same platforms! (In
-- particular see the controversy over \"windows\" vs \"mingw32\"). So to make it
-- more consistent and easy to use we have an 'OS' enumeration.
--
module Distribution.System (
  -- * Operating System
  OS(..),
  buildOS,

  -- * Machine Architecture
  Arch(..),
  buildArch,

  -- * Platform is a pair of arch and OS
  Platform(..),
  buildPlatform,
  platformFromTriple,

  -- * Internal
  knownOSs,
  knownArches,

  -- * Classification
  ClassificationStrictness (..),
  classifyOS,
  classifyArch,
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Control.Applicative (liftA2)

import qualified System.Info (os, arch)
import Distribution.Utils.Generic (lowercase)

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as P
import qualified Text.PrettyPrint as Disp

-- | How strict to be when classifying strings into the 'OS' and 'Arch' enums.
--
-- The reason we have multiple ways to do the classification is because there
-- are two situations where we need to do it.
--
-- For parsing OS and arch names in .cabal files we really want everyone to be
-- referring to the same or or arch by the same name. Variety is not a virtue
-- in this case. We don't mind about case though.
--
-- For the System.Info.os\/arch different Haskell implementations use different
-- names for the same or\/arch. Also they tend to distinguish versions of an
-- OS\/arch which we just don't care about.
--
-- The 'Compat' classification allows us to recognise aliases that are already
-- in common use but it allows us to distinguish them from the canonical name
-- which enables us to warn about such deprecated aliases.
--
data ClassificationStrictness = Permissive | Compat | Strict

-- ------------------------------------------------------------
-- * Operating System
-- ------------------------------------------------------------

-- | These are the known OS names: Linux, Windows, OSX
--  ,FreeBSD, OpenBSD, NetBSD, DragonFly
--  ,Solaris, AIX, HPUX, IRIX
--  ,HaLVM ,Hurd ,IOS, Android,Ghcjs
--
-- The following aliases can also be used:,
--    * Windows aliases: mingw32, win32, cygwin32
--    * OSX alias: darwin
--    * Hurd alias: gnu
--    * FreeBSD alias: kfreebsdgnu
--    * Solaris alias: solaris2
--
data OS = Linux | Windows | OSX        -- tier 1 desktop OSs
        | FreeBSD | OpenBSD | NetBSD   -- other free Unix OSs
        | DragonFly
        | Solaris | AIX | HPUX | IRIX  -- ageing Unix OSs
        | HaLVM                        -- bare metal / VMs / hypervisors
        | Hurd                         -- GNU's microkernel
        | IOS  | Android               -- mobile OSs
        | Ghcjs
        | OtherOS String
  deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

instance Binary OS

knownOSs :: [OS]
knownOSs = [Linux, Windows, OSX
           ,FreeBSD, OpenBSD, NetBSD, DragonFly
           ,Solaris, AIX, HPUX, IRIX
           ,HaLVM
           ,Hurd
           ,IOS, Android
           ,Ghcjs]

osAliases :: ClassificationStrictness -> OS -> [String]
osAliases Permissive Windows = ["mingw32", "win32", "cygwin32"]
osAliases Compat     Windows = ["mingw32", "win32"]
osAliases _          OSX     = ["darwin"]
osAliases _          Hurd    = ["gnu"]
osAliases Permissive FreeBSD = ["kfreebsdgnu"]
osAliases Compat     FreeBSD = ["kfreebsdgnu"]
osAliases Permissive Solaris = ["solaris2"]
osAliases Compat     Solaris = ["solaris2"]
osAliases _          _       = []

instance Pretty OS where
  pretty (OtherOS name) = Disp.text name
  pretty other          = Disp.text (lowercase (show other))

instance Parsec OS where
  parsec = classifyOS Compat <$> parsecIdent

instance Text OS where
  parse = fmap (classifyOS Compat) ident

classifyOS :: ClassificationStrictness -> String -> OS
classifyOS strictness s =
  fromMaybe (OtherOS s) $ lookup (lowercase s) osMap
  where
    osMap = [ (name, os)
            | os <- knownOSs
            , name <- display os : osAliases strictness os ]

buildOS :: OS
buildOS = classifyOS Permissive System.Info.os

-- ------------------------------------------------------------
-- * Machine Architecture
-- ------------------------------------------------------------

-- | These are the known Arches: I386, X86_64, PPC, PPC64, Sparc
-- ,Arm, Mips, SH, IA64, S39, Alpha, Hppa, Rs6000, M68k, Vax
-- and JavaScript.
--
-- The following aliases can also be used:
--    * PPC alias: powerpc
--    * PPC64 alias : powerpc64
--    * Sparc aliases: sparc64, sun4
--    * Mips aliases: mipsel, mipseb
--    * Arm aliases: armeb, armel
--
data Arch = I386  | X86_64 | PPC | PPC64 | Sparc
          | Arm   | Mips   | SH
          | IA64  | S390
          | Alpha | Hppa   | Rs6000
          | M68k  | Vax
          | JavaScript
          | OtherArch String
  deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

instance Binary Arch

knownArches :: [Arch]
knownArches = [I386, X86_64, PPC, PPC64, Sparc
              ,Arm, Mips, SH
              ,IA64, S390
              ,Alpha, Hppa, Rs6000
              ,M68k, Vax
              ,JavaScript]

archAliases :: ClassificationStrictness -> Arch -> [String]
archAliases Strict _     = []
archAliases Compat _     = []
archAliases _      PPC   = ["powerpc"]
archAliases _      PPC64 = ["powerpc64"]
archAliases _      Sparc = ["sparc64", "sun4"]
archAliases _      Mips  = ["mipsel", "mipseb"]
archAliases _      Arm   = ["armeb", "armel"]
archAliases _      _     = []

instance Pretty Arch where
  pretty (OtherArch name) = Disp.text name
  pretty other            = Disp.text (lowercase (show other))

instance Parsec Arch where
  parsec = classifyArch Strict <$> parsecIdent

instance Text Arch where
  parse = fmap (classifyArch Strict) ident

classifyArch :: ClassificationStrictness -> String -> Arch
classifyArch strictness s =
  fromMaybe (OtherArch s) $ lookup (lowercase s) archMap
  where
    archMap = [ (name, arch)
              | arch <- knownArches
              , name <- display arch : archAliases strictness arch ]

buildArch :: Arch
buildArch = classifyArch Permissive System.Info.arch

-- ------------------------------------------------------------
-- * Platform
-- ------------------------------------------------------------

data Platform = Platform Arch OS
  deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

instance Binary Platform

instance Pretty Platform where
  pretty (Platform arch os) = pretty arch <<>> Disp.char '-' <<>> pretty os

instance Parsec Platform where
    parsec = do
        arch <- parsecDashlessArch
        _ <- P.char '-'
        os <- parsec
        return (Platform arch os)
      where
        parsecDashlessArch = classifyArch Strict <$> dashlessIdent

        dashlessIdent = liftA2 (:) firstChar rest
          where
            firstChar = P.satisfy isAlpha
            rest = P.munch (\c -> isAlphaNum c || c == '_')

instance Text Platform where
  -- TODO: there are ambigious platforms like: `arch-word-os`
  -- which could be parsed as
  --   * Platform "arch-word" "os"
  --   * Platform "arch" "word-os"
  -- We could support that preferring variants 'OtherOS' or 'OtherArch'
  --
  -- For now we split into arch and os parts on the first dash.
  parse = do
    arch <- parseDashlessArch
    _ <- Parse.char '-'
    os   <- parse
    return (Platform arch os)
      where
        parseDashlessArch :: Parse.ReadP r Arch
        parseDashlessArch = fmap (classifyArch Strict) dashlessIdent

        dashlessIdent :: Parse.ReadP r String
        dashlessIdent = liftM2 (:) firstChar rest
          where firstChar = Parse.satisfy isAlpha
                rest = Parse.munch (\c -> isAlphaNum c || c == '_')

-- | The platform Cabal was compiled on. In most cases,
-- @LocalBuildInfo.hostPlatform@ should be used instead (the platform we're
-- targeting).
buildPlatform :: Platform
buildPlatform = Platform buildArch buildOS

-- Utils:

ident :: Parse.ReadP r String
ident = liftM2 (:) firstChar rest
  where firstChar = Parse.satisfy isAlpha
        rest = Parse.munch (\c -> isAlphaNum c || c == '_' || c == '-')

parsecIdent :: ParsecParser String
parsecIdent = (:) <$> firstChar <*> rest
  where
    firstChar = P.satisfy isAlpha
    rest      = P.munch (\c -> isAlphaNum c || c == '_' || c == '-')

platformFromTriple :: String -> Maybe Platform
platformFromTriple triple =
  fmap fst (listToMaybe $ Parse.readP_to_S parseTriple triple)
  where parseWord = Parse.munch1 (\c -> isAlphaNum c || c == '_')
        parseTriple = do
          arch <- fmap (classifyArch Permissive) parseWord
          _ <- Parse.char '-'
          _ <- parseWord -- Skip vendor
          _ <- Parse.char '-'
          os <- fmap (classifyOS Permissive) ident -- OS may have hyphens, like
                                               -- 'nto-qnx'
          return $ Platform arch os
