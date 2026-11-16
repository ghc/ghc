{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro.DeepSeq where

import Macro.Types

import Control.DeepSeq
#if MIN_VERSION_deepseq(1,4,3)
     hiding (rnf1, rnf2)
#endif

rnf0 :: ()
rnf1 :: NFData a => a -> ()
rnf2 :: (NFData a, NFData a1) => a -> a1 -> ()
rnf3 :: (NFData a, NFData a1, NFData a2) =>
        a -> a1 -> a2 -> ()
rnf4 :: (NFData a, NFData a1, NFData a2, NFData a3) =>
        a -> a1 -> a2 -> a3 -> ()
rnf5 :: (NFData a, NFData a1, NFData a2, NFData a3, NFData a4) =>
        a -> a1 -> a2 -> a3 -> a4 -> ()
rnf6 :: (NFData a, NFData a1, NFData a2, NFData a3, NFData a4,
         NFData a5) =>
        a -> a1 -> a2 -> a3 -> a4 -> a5 -> ()
rnf7 :: (NFData a, NFData a1, NFData a2, NFData a3, NFData a4, NFData a5,
         NFData a6) =>
        a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> ()


rnf0           = ()
rnf1 a         = rnf a `seq` ()
rnf2 a b       = rnf a `seq` rnf b `seq` ()
rnf3 a b c     = rnf a `seq` rnf b `seq` rnf c `seq` ()
rnf4 a b c d   = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` ()
rnf5 a b c d e = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` ()
rnf6 a b c d e
     f         = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
                 rnf f `seq` ()
rnf7 a b c d e
     f g       = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
                 rnf f `seq` rnf g `seq` ()
{-# INLINE rnf0 #-}
{-# INLINE rnf1 #-}
{-# INLINE rnf2 #-}
{-# INLINE rnf3 #-}
{-# INLINE rnf4 #-}
{-# INLINE rnf5 #-}
{-# INLINE rnf6 #-}
{-# INLINE rnf7 #-}

instance NFData PackageName where
  rnf (PackageName a) = rnf1 a

instance NFData PackageId where
  rnf (PackageId a b) = rnf2 a b

instance NFData Version where
  rnf (Version a b) = rnf2 a b

instance NFData VersionRange where
  rnf AnyVersion                   = rnf0
  rnf (ThisVersion            a)   = rnf1 a
  rnf (LaterVersion           a)   = rnf1 a
  rnf (EarlierVersion         a)   = rnf1 a
  rnf (WildcardVersion        a)   = rnf1 a
  rnf (UnionVersionRanges     a b) = rnf2 a b
  rnf (IntersectVersionRanges a b) = rnf2 a b
  rnf (VersionRangeParens     a)   = rnf1 a

instance NFData Dependency where
  rnf (Dependency a b) = rnf2 a b

instance NFData CompilerFlavor where
  rnf GHC    = rnf0
  rnf NHC    = rnf0
  rnf YHC    = rnf0
  rnf Hugs   = rnf0
  rnf HBC    = rnf0
  rnf Helium = rnf0
  rnf JHC    = rnf0
  rnf LHC    = rnf0
  rnf UHC    = rnf0
  rnf (HaskellSuite a)  = rnf1 a
  rnf (OtherCompiler a) = rnf1 a

instance NFData License where
  rnf (GPL a)    = rnf1 a
  rnf (AGPL a)   = rnf1 a
  rnf (LGPL a)   = rnf1 a
  rnf BSD3       = rnf0
  rnf BSD4       = rnf0
  rnf MIT        = rnf0
  rnf (Apache a) = rnf1 a
  rnf PublicDomain       = rnf0
  rnf AllRightsReserved  = rnf0
  rnf OtherLicense       = rnf0
  rnf (UnknownLicense a) = rnf1 a

instance NFData SourceRepo where
  rnf (SourceRepo a b c d e f g) = rnf7 a b c d e f g

instance NFData RepoKind where
  rnf RepoHead  = rnf0
  rnf RepoThis  = rnf0
  rnf (RepoKindUnknown a) = rnf1 a


instance NFData RepoType where
  rnf Darcs     = rnf0
  rnf Git       = rnf0
  rnf SVN       = rnf0
  rnf CVS       = rnf0
  rnf Mercurial = rnf0
  rnf GnuArch   = rnf0
  rnf Bazaar    = rnf0
  rnf Monotone  = rnf0
  rnf (OtherRepoType a) = rnf1 a

instance NFData BuildType where
  rnf Simple    = rnf0
  rnf Configure = rnf0
  rnf Make      = rnf0
  rnf Custom    = rnf0
  rnf (UnknownBuildType a) = rnf1 a

instance NFData Library where
  rnf (Library a b c) = rnf3 a b c

instance NFData Executable where
  rnf (Executable a b c) = rnf3 a b c

instance NFData TestSuite where
  rnf (TestSuite a b c d) = rnf4 a b c d

instance NFData TestSuiteInterface where
  rnf (TestSuiteExeV10 a b)    = rnf2 a b
  rnf (TestSuiteLibV09 a b)    = rnf2 a b
  rnf (TestSuiteUnsupported a) = rnf1 a

instance NFData TestType where
  rnf (TestTypeExe a) = rnf1 a
  rnf (TestTypeLib a) = rnf1 a
  rnf (TestTypeUnknown a b) = rnf2 a b

instance NFData Benchmark where
  rnf (Benchmark a b c d) = rnf4 a b c d

instance NFData BenchmarkInterface where
  rnf (BenchmarkExeV10 a b)    = rnf2 a b
  rnf (BenchmarkUnsupported a) = rnf1 a

instance NFData BenchmarkType where
  rnf (BenchmarkTypeExe     a)   = rnf1 a
  rnf (BenchmarkTypeUnknown a b) = rnf2 a b

instance NFData ModuleName where
  rnf (ModuleName a) = rnf1 a

instance NFData BuildInfo where
  rnf (BuildInfo a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                 a21 a22 a23 a24 a25) =
    rnf a1  `seq` rnf a2  `seq` rnf a3  `seq` rnf a4  `seq` rnf a5  `seq`
    rnf a6  `seq` rnf a7  `seq` rnf a8  `seq` rnf a9  `seq` rnf a10 `seq`
    rnf a11 `seq` rnf a12 `seq` rnf a13 `seq` rnf a14 `seq` rnf a15 `seq`
    rnf a16 `seq` rnf a17 `seq` rnf a18 `seq` rnf a19 `seq` rnf a20 `seq`
    rnf a21 `seq` rnf a22 `seq` rnf a23 `seq` rnf a24 `seq` rnf a25 `seq` ()


instance NFData Language where
  rnf Haskell98   = rnf0
  rnf Haskell2010 = rnf0
  rnf (UnknownLanguage a) = rnf1 a

instance NFData Extension where
  rnf (EnableExtension  a) = rnf1 a
  rnf (DisableExtension a) = rnf1 a
  rnf (UnknownExtension a) = rnf1 a

instance NFData KnownExtension

instance NFData PackageDescription where
  rnf (PackageDescription a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                          a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                          a21 a22 a23 a24 a25 a26 a27 a28) =
    rnf a1  `seq` rnf a2  `seq` rnf a3  `seq` rnf a4  `seq` rnf a5  `seq`
    rnf a6  `seq` rnf a7  `seq` rnf a8  `seq` rnf a9  `seq` rnf a10 `seq`
    rnf a11 `seq` rnf a12 `seq` rnf a13 `seq` rnf a14 `seq` rnf a15 `seq`
    rnf a16 `seq` rnf a17 `seq` rnf a18 `seq` rnf a19 `seq` rnf a20 `seq`
    rnf a21 `seq` rnf a22 `seq` rnf a23 `seq` rnf a24 `seq` rnf a25 `seq`
    rnf a26 `seq` rnf a27 `seq` rnf a28 `seq` ()

instance NFData OS where
  rnf Linux   = rnf0
  rnf Windows = rnf0
  rnf OSX     = rnf0
  rnf FreeBSD = rnf0
  rnf OpenBSD = rnf0
  rnf NetBSD  = rnf0
  rnf Solaris = rnf0
  rnf AIX     = rnf0
  rnf HPUX    = rnf0
  rnf IRIX    = rnf0
  rnf HaLVM   = rnf0
  rnf IOS     = rnf0
  rnf (OtherOS a) = rnf1 a

instance NFData Arch where
  rnf I386   = rnf0
  rnf X86_64 = rnf0
  rnf PPC    = rnf0
  rnf PPC64  = rnf0
  rnf Sparc  = rnf0
  rnf Arm    = rnf0
  rnf Mips   = rnf0
  rnf SH     = rnf0
  rnf IA64   = rnf0
  rnf S390   = rnf0
  rnf Alpha  = rnf0
  rnf Hppa   = rnf0
  rnf Rs6000 = rnf0
  rnf M68k   = rnf0
  rnf (OtherArch a) = rnf1 a
  rnf Vax    = rnf0

instance NFData Flag where
  rnf (MkFlag a b c d) = rnf4 a b c d

instance NFData FlagName where
  rnf (FlagName a) = rnf1 a

instance (NFData a, NFData b, NFData c) => NFData (CondTree a b c) where
  rnf (CondNode a b c) = rnf3 a b c

instance NFData ConfVar where
  rnf (OS   a)   = rnf1 a
  rnf (Arch a)   = rnf1 a
  rnf (Flag a)   = rnf1 a
  rnf (Impl a b) = rnf2 a b

instance NFData a => NFData (Condition a) where
  rnf (Var  a)   = rnf1 a
  rnf (Lit  a)   = rnf1 a
  rnf (CNot a)   = rnf1 a
  rnf (COr  a b) = rnf2 a b
  rnf (CAnd a b) = rnf2 a b

instance NFData GenericPackageDescription where
  rnf (GenericPackageDescription a b c d e f) = rnf6 a b c d e f

