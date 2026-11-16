{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-- For PackageDescription and friends
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- For encodeCtrN/decodeCtrBodyN/etc
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Macro.CBOR (serialise, deserialise, deserialiseNull) where

import Macro.Types

import Codec.Serialise.Class
import Codec.Serialise.Encoding hiding (Tokens(..))
import Codec.Serialise.Decoding hiding (DecodeAction(Done, Fail))
import Codec.CBOR.Read
import Codec.CBOR.Write

#if ! MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS

import Control.Exception (throw)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Word
#endif

serialise :: [GenericPackageDescription] -> BS.ByteString
--serialise :: Serialise a => a -> BS.ByteString
serialise = BS.toLazyByteString . toBuilder . encode

deserialise :: BS.ByteString -> [GenericPackageDescription]
deserialise = either throw snd . deserialiseFromBytes decode

deserialiseNull :: BS.ByteString -> ()
deserialiseNull = either throw snd . deserialiseFromBytes decodeListNull
  where
    decodeListNull :: Decoder s ()
    decodeListNull = do decodeListLenIndef; go

    go = do stop <- decodeBreakOr
            if stop then return ()
                    else do !_ <- decode :: Decoder s GenericPackageDescription
                            go

encodeCtr0 n     = encodeListLen 1 <> encode (n :: Word)
encodeCtr1 n a   = encodeListLen 2 <> encode (n :: Word) <> encode a
encodeCtr2 n a b = encodeListLen 3 <> encode (n :: Word) <> encode a <> encode b
encodeCtr3 n a b c
                 = encodeListLen 4 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c
encodeCtr4 n a b c d
                 = encodeListLen 5 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d
encodeCtr6 n a b c d e f
                 = encodeListLen 7 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
encodeCtr7 n a b c d e f g
                 = encodeListLen 8 <> encode (n :: Word) <> encode a <> encode b
                      <> encode c <> encode d <> encode e <> encode f
                      <> encode g

{-# INLINE encodeCtr0 #-}
{-# INLINE encodeCtr1 #-}
{-# INLINE encodeCtr2 #-}
{-# INLINE encodeCtr3 #-}
{-# INLINE encodeCtr4 #-}
{-# INLINE encodeCtr6 #-}
{-# INLINE encodeCtr7 #-}

{-# INLINE decodeCtrTag #-}
{-# INLINE decodeCtrBody0 #-}
{-# INLINE decodeCtrBody1 #-}
{-# INLINE decodeCtrBody2 #-}

decodeCtrTag = (\len tag -> (tag, len)) <$> decodeListLen <*> decodeWord

decodeCtrBody0 1 f = pure f
decodeCtrBody0 x _ = error $ "decodeCtrBody0: impossible tag " ++ show x
decodeCtrBody1 2 f = do x1 <- decode
                        return $! f x1
decodeCtrBody1 x _ = error $ "decodeCtrBody1: impossible tag " ++ show x
decodeCtrBody2 3 f = do x1 <- decode
                        x2 <- decode
                        return $! f x1 x2
decodeCtrBody2 x _ = error $ "decodeCtrBody2: impossible tag " ++ show x

{-# INLINE decodeSingleCtr1 #-}
{-# INLINE decodeSingleCtr2 #-}
{-# INLINE decodeSingleCtr3 #-}
{-# INLINE decodeSingleCtr4 #-}
{-# INLINE decodeSingleCtr6 #-}
{-# INLINE decodeSingleCtr7 #-}

decodeSingleCtr1 v f = decodeListLenOf 2 *> decodeWordOf v *> pure f <*> decode
decodeSingleCtr2 v f = decodeListLenOf 3 *> decodeWordOf v *> pure f <*> decode <*> decode
decodeSingleCtr3 v f = decodeListLenOf 4 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode
decodeSingleCtr4 v f = decodeListLenOf 5 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode
decodeSingleCtr6 v f = decodeListLenOf 7 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode
decodeSingleCtr7 v f = decodeListLenOf 8 *> decodeWordOf v *> pure f <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode


instance Serialise PackageName where
  encode (PackageName a) = encodeCtr1 1 a
  decode = decodeSingleCtr1 1 PackageName

instance Serialise Version where
  encode (Version a b) = encodeCtr2 1 a b
  decode = decodeSingleCtr2 1 Version

instance Serialise PackageId where
  encode (PackageId a b) = encodeCtr2 1 a b
  decode = decodeSingleCtr2 1 PackageId

instance Serialise VersionRange where
  encode AnyVersion                   = encodeCtr0 1
  encode (ThisVersion            a)   = encodeCtr1 2 a
  encode (LaterVersion           a)   = encodeCtr1 3 a
  encode (EarlierVersion         a)   = encodeCtr1 4 a
  encode (WildcardVersion        a)   = encodeCtr1 5 a
  encode (UnionVersionRanges     a b) = encodeCtr2 6 a b
  encode (IntersectVersionRanges a b) = encodeCtr2 7 a b
  encode (VersionRangeParens     a)   = encodeCtr1 8 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l AnyVersion
      2 -> decodeCtrBody1 l ThisVersion
      3 -> decodeCtrBody1 l LaterVersion
      4 -> decodeCtrBody1 l EarlierVersion
      5 -> decodeCtrBody1 l WildcardVersion
      6 -> decodeCtrBody2 l UnionVersionRanges
      7 -> decodeCtrBody2 l IntersectVersionRanges
      8 -> decodeCtrBody1 l VersionRangeParens
      x -> error $ "Serialise VersionRange: decode: impossible tag " ++ show x


instance Serialise Dependency where
  encode (Dependency a b) = encodeCtr2 1 a b
  decode = decodeSingleCtr2 1 Dependency

instance Serialise CompilerFlavor where
  encode GHC    = encodeCtr0 1
  encode NHC    = encodeCtr0 2
  encode YHC    = encodeCtr0 3
  encode Hugs   = encodeCtr0 4
  encode HBC    = encodeCtr0 5
  encode Helium = encodeCtr0 6
  encode JHC    = encodeCtr0 7
  encode LHC    = encodeCtr0 8
  encode UHC    = encodeCtr0 9
  encode (HaskellSuite a)  = encodeCtr1 10 a
  encode (OtherCompiler a) = encodeCtr1 11 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l GHC
      2 -> decodeCtrBody0 l NHC
      3 -> decodeCtrBody0 l YHC
      4 -> decodeCtrBody0 l Hugs
      5 -> decodeCtrBody0 l HBC
      6 -> decodeCtrBody0 l Helium
      7 -> decodeCtrBody0 l JHC
      8 -> decodeCtrBody0 l LHC
      9 -> decodeCtrBody0 l UHC
      10 -> decodeCtrBody1 l HaskellSuite
      11 -> decodeCtrBody1 l OtherCompiler
      x -> error $ "Serialise CompilerFlavor: decode: impossible tag " ++ show x

instance Serialise License where
  encode (GPL  a)   = encodeCtr1 1 a
  encode (AGPL a)   = encodeCtr1 2 a
  encode (LGPL a)   = encodeCtr1 3 a
  encode BSD3       = encodeCtr0 4
  encode BSD4       = encodeCtr0 5
  encode MIT        = encodeCtr0 6
  encode (Apache a) = encodeCtr1 7 a
  encode PublicDomain       = encodeCtr0 8
  encode AllRightsReserved  = encodeCtr0 9
  encode OtherLicense       = encodeCtr0 10
  encode (UnknownLicense a) = encodeCtr1 11 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l GPL
      2 -> decodeCtrBody1 l AGPL
      3 -> decodeCtrBody1 l LGPL
      4 -> decodeCtrBody0 l BSD3
      5 -> decodeCtrBody0 l BSD4
      6 -> decodeCtrBody0 l MIT
      7 -> decodeCtrBody1 l Apache
      8 -> decodeCtrBody0 l PublicDomain
      9 -> decodeCtrBody0 l AllRightsReserved
      10 -> decodeCtrBody0 l OtherLicense
      11 -> decodeCtrBody1 l UnknownLicense
      x -> error $ "Serialise License: decode: impossible tag " ++ show x

instance Serialise SourceRepo where
  encode (SourceRepo a b c d e f g) = encodeCtr7 1 a b c d e f g
  decode = decodeSingleCtr7 1 SourceRepo

instance Serialise RepoKind where
  encode RepoHead  = encodeCtr0 1
  encode RepoThis  = encodeCtr0 2
  encode (RepoKindUnknown a) = encodeCtr1 3 a
  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l RepoHead
      2 -> decodeCtrBody0 l RepoThis
      3 -> decodeCtrBody1 l RepoKindUnknown
      x -> error $ "Serialise RepoKind: decode: impossible tag " ++ show x

instance Serialise RepoType where
  encode Darcs     = encodeCtr0 1
  encode Git       = encodeCtr0 2
  encode SVN       = encodeCtr0 3
  encode CVS       = encodeCtr0 4
  encode Mercurial = encodeCtr0 5
  encode GnuArch   = encodeCtr0 6
  encode Bazaar    = encodeCtr0 7
  encode Monotone  = encodeCtr0 8
  encode (OtherRepoType a) = encodeCtr1 9 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l Darcs
      2 -> decodeCtrBody0 l Git
      3 -> decodeCtrBody0 l SVN
      4 -> decodeCtrBody0 l CVS
      5 -> decodeCtrBody0 l Mercurial
      6 -> decodeCtrBody0 l GnuArch
      7 -> decodeCtrBody0 l Bazaar
      8 -> decodeCtrBody0 l Monotone
      9 -> decodeCtrBody1 l OtherRepoType
      x -> error $ "Serialise RepoType: decode: impossible tag " ++ show x

instance Serialise BuildType where
  encode Simple    = encodeCtr0 1
  encode Configure = encodeCtr0 2
  encode Make      = encodeCtr0 3
  encode Custom    = encodeCtr0 4
  encode (UnknownBuildType a) = encodeCtr1 5 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l Simple
      2 -> decodeCtrBody0 l Configure
      3 -> decodeCtrBody0 l Make
      4 -> decodeCtrBody0 l Custom
      5 -> decodeCtrBody1 l UnknownBuildType
      x -> error $ "Serialise BuildType: decode: impossible tag " ++ show x

instance Serialise Library where
  encode (Library a b c) = encodeCtr3 1 a b c
  decode = decodeSingleCtr3 1 Library

instance Serialise Executable where
  encode (Executable a b c) = encodeCtr3 1 a b c
  decode = decodeSingleCtr3 1 Executable

instance Serialise TestSuite where
  encode (TestSuite a b c d) = encodeCtr4 1 a b c d
  decode = decodeSingleCtr4 1 TestSuite

instance Serialise TestSuiteInterface where
  encode (TestSuiteExeV10 a b)    = encodeCtr2 1 a b
  encode (TestSuiteLibV09 a b)    = encodeCtr2 2 a b
  encode (TestSuiteUnsupported a) = encodeCtr1 3 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody2 l TestSuiteExeV10
      2 -> decodeCtrBody2 l TestSuiteLibV09
      3 -> decodeCtrBody1 l TestSuiteUnsupported
      x -> error $
               "Serialise TestSuiteInterface: decode: impossible tag " ++ show x

instance Serialise TestType where
  encode (TestTypeExe a) = encodeCtr1 1 a
  encode (TestTypeLib a) = encodeCtr1 2 a
  encode (TestTypeUnknown a b) = encodeCtr2 3 a b

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l TestTypeExe
      2 -> decodeCtrBody1 l TestTypeLib
      3 -> decodeCtrBody2 l TestTypeUnknown
      x -> error $ "Serialise TestType: decode: impossible tag " ++ show x

instance Serialise Benchmark where
  encode (Benchmark a b c d) = encodeCtr4 1 a b c d
  decode = decodeSingleCtr4 1 Benchmark

instance Serialise BenchmarkInterface where
  encode (BenchmarkExeV10 a b)    = encodeCtr2 1 a b
  encode (BenchmarkUnsupported a) = encodeCtr1 2 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody2 l BenchmarkExeV10
      2 -> decodeCtrBody1 l BenchmarkUnsupported
      x -> error $
               "Serialise BenchmarkInterface: decode: impossible tag " ++ show x

instance Serialise BenchmarkType where
  encode (BenchmarkTypeExe     a)   = encodeCtr1 1 a
  encode (BenchmarkTypeUnknown a b) = encodeCtr2 2 a b

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l BenchmarkTypeExe
      2 -> decodeCtrBody2 l BenchmarkTypeUnknown
      x -> error $ "Serialise BenchmarkType: decode: impossible tag " ++ show x

instance Serialise ModuleName where
  encode (ModuleName a) = encodeCtr1 1 a
  decode = decodeSingleCtr1 1 ModuleName

instance Serialise BuildInfo where
  encode (BuildInfo a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                    a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                    a21 a22 a23 a24 a25) =
    encodeListLen 26 <> encode (1 :: Word) <>
    encode a1  <> encode a2  <> encode a3  <> encode a4  <> encode a5  <>
    encode a6  <> encode a7  <> encode a8  <> encode a9  <> encode a10 <>
    encode a11 <> encode a12 <> encode a13 <> encode a14 <> encode a15 <>
    encode a16 <> encode a17 <> encode a18 <> encode a19 <> encode a20 <>
    encode a21 <> encode a22 <> encode a23 <> encode a24 <> encode a25

  decode = decodeListLenOf 26 *> decodeWordOf 1 *>
           pure BuildInfo <*> decode <*> decode <*> decode <*> decode <*> decode
                          <*> decode <*> decode <*> decode <*> decode <*> decode
                          <*> decode <*> decode <*> decode <*> decode <*> decode
                          <*> decode <*> decode <*> decode <*> decode <*> decode
                          <*> decode <*> decode <*> decode <*> decode <*> decode

instance Serialise Language where
  encode Haskell98   = encodeCtr0 1
  encode Haskell2010 = encodeCtr0 2
  encode (UnknownLanguage a) = encodeCtr1 3 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody0 l Haskell98
      2 -> decodeCtrBody0 l Haskell2010
      3 -> decodeCtrBody1 l UnknownLanguage
      x -> error $ "Serialise Language: decode: impossible tag " ++ show x

instance Serialise Extension where
  encode (EnableExtension  a) = encodeCtr1 1 a
  encode (DisableExtension a) = encodeCtr1 2 a
  encode (UnknownExtension a) = encodeCtr1 3 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l EnableExtension
      2 -> decodeCtrBody1 l DisableExtension
      3 -> decodeCtrBody1 l UnknownExtension
      x -> error $ "Serialise Extension: decode: impossible tag " ++ show x

instance Serialise KnownExtension where
  encode ke = encodeCtr1 1 (fromEnum ke)
  decode = decodeSingleCtr1 1 toEnum

instance Serialise PackageDescription where
  encode (PackageDescription a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                             a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                             a21 a22 a23 a24 a25 a26 a27 a28) =
    encodeListLen 29 <> encode (1 :: Word) <>
    encode a1  <> encode a2  <> encode a3  <> encode a4  <> encode a5  <>
    encode a6  <> encode a7  <> encode a8  <> encode a9  <> encode a10 <>
    encode a11 <> encode a12 <> encode a13 <> encode a14 <> encode a15 <>
    encode a16 <> encode a17 <> encode a18 <> encode a19 <> encode a20 <>
    encode a21 <> encode a22 <> encode a23 <> encode a24 <> encode a25 <>
    encode a26 <> encode a27 <> encode a28

  decode = decodeListLenOf 29 *> decodeWordOf 1 *>
           pure PackageDescription
              <*> decode <*> decode <*> decode <*> decode <*> decode
              <*> decode <*> decode <*> decode <*> decode <*> decode
              <*> decode <*> decode <*> decode <*> decode <*> decode
              <*> decode <*> decode <*> decode <*> decode <*> decode
              <*> decode <*> decode <*> decode <*> decode <*> decode
              <*> decode <*> decode <*> decode

instance Serialise OS where
  encode Linux   = encodeCtr0 1
  encode Windows = encodeCtr0 2
  encode OSX     = encodeCtr0 3
  encode FreeBSD = encodeCtr0 4
  encode OpenBSD = encodeCtr0 5
  encode NetBSD  = encodeCtr0 6
  encode Solaris = encodeCtr0 7
  encode AIX     = encodeCtr0 8
  encode HPUX    = encodeCtr0 9
  encode IRIX    = encodeCtr0 10
  encode HaLVM   = encodeCtr0 11
  encode IOS     = encodeCtr0 12
  encode (OtherOS a) = encodeCtr1 13 a

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1  -> decodeCtrBody0 l Linux
      2  -> decodeCtrBody0 l Windows
      3  -> decodeCtrBody0 l OSX
      4  -> decodeCtrBody0 l FreeBSD
      5  -> decodeCtrBody0 l OpenBSD
      6  -> decodeCtrBody0 l NetBSD
      7  -> decodeCtrBody0 l Solaris
      8  -> decodeCtrBody0 l AIX
      9  -> decodeCtrBody0 l HPUX
      10 -> decodeCtrBody0 l IRIX
      11 -> decodeCtrBody0 l HaLVM
      12 -> decodeCtrBody0 l IOS
      13 -> decodeCtrBody1 l OtherOS
      x -> error $ "Serialise OS: decode: impossible tag " ++ show x

instance Serialise Arch where
  encode I386   = encodeCtr0 1
  encode X86_64 = encodeCtr0 2
  encode PPC    = encodeCtr0 3
  encode PPC64  = encodeCtr0 4
  encode Sparc  = encodeCtr0 5
  encode Arm    = encodeCtr0 6
  encode Mips   = encodeCtr0 7
  encode SH     = encodeCtr0 8
  encode IA64   = encodeCtr0 9
  encode S390   = encodeCtr0 10
  encode Alpha  = encodeCtr0 11
  encode Hppa   = encodeCtr0 12
  encode Rs6000 = encodeCtr0 13
  encode M68k   = encodeCtr0 14
  encode (OtherArch a) = encodeCtr1 15 a
  encode Vax    = encodeCtr0 16

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1  -> decodeCtrBody0 l I386
      2  -> decodeCtrBody0 l X86_64
      3  -> decodeCtrBody0 l PPC
      4  -> decodeCtrBody0 l PPC64
      5  -> decodeCtrBody0 l Sparc
      6  -> decodeCtrBody0 l Arm
      7  -> decodeCtrBody0 l Mips
      8  -> decodeCtrBody0 l SH
      9  -> decodeCtrBody0 l IA64
      10 -> decodeCtrBody0 l S390
      11 -> decodeCtrBody0 l Alpha
      12 -> decodeCtrBody0 l Hppa
      13 -> decodeCtrBody0 l Rs6000
      14 -> decodeCtrBody0 l M68k
      15 -> decodeCtrBody1 l OtherArch
      16 -> decodeCtrBody0 l Vax
      x -> error $ "Serialise Arch: decode: impossible tag " ++ show x

instance Serialise Flag where
  encode (MkFlag a b c d) = encodeCtr4 1 a b c d
  decode = decodeSingleCtr4 1 MkFlag

instance Serialise FlagName where
  encode (FlagName a) = encodeCtr1 1 a
  decode = decodeSingleCtr1 1 FlagName

instance (Serialise a, Serialise b, Serialise c) => Serialise (CondTree a b c) where
  encode (CondNode a b c) = encodeCtr3 1 a b c
  decode = decodeSingleCtr3 1 CondNode

  {-# SPECIALIZE instance Serialise c => Serialise (CondTree ConfVar [Dependency] c) #-}

instance Serialise ConfVar where
  encode (OS   a)   = encodeCtr1 1 a
  encode (Arch a)   = encodeCtr1 2 a
  encode (Flag a)   = encodeCtr1 3 a
  encode (Impl a b) = encodeCtr2 4 a b

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l OS
      2 -> decodeCtrBody1 l Arch
      3 -> decodeCtrBody1 l Flag
      4 -> decodeCtrBody2 l Impl
      x -> error $ "Serialise ConfVar: decode: impossible tag " ++ show x

instance Serialise a => Serialise (Condition a) where
  encode (Var  a)   = encodeCtr1 1 a
  encode (Lit  a)   = encodeCtr1 2 a
  encode (CNot a)   = encodeCtr1 3 a
  encode (COr  a b) = encodeCtr2 4 a b
  encode (CAnd a b) = encodeCtr2 5 a b

  decode = do
    (t,l) <- decodeCtrTag
    case t of
      1 -> decodeCtrBody1 l Var
      2 -> decodeCtrBody1 l Lit
      3 -> decodeCtrBody1 l CNot
      4 -> decodeCtrBody2 l COr
      5 -> decodeCtrBody2 l CAnd
      x -> error $ "Serialise (Condition a): decode: impossible tag " ++ show x

  {-# SPECIALIZE instance Serialise (Condition ConfVar) #-}

instance Serialise GenericPackageDescription where
  encode (GenericPackageDescription a b c d e f) = encodeCtr6 1 a b c d e f
  decode = decodeSingleCtr6 1 GenericPackageDescription

