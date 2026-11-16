{-# LANGUAGE BangPatterns #-}
module Macro.MemSize where

import Macro.Types

class MemSize a where
  memSize :: a -> Int -> Int

memSize0 :: Int -> Int
memSize1 :: MemSize a => a -> Int -> Int
memSize2 :: (MemSize a, MemSize a1) =>
            a -> a1 -> Int -> Int
memSize3 :: (MemSize a, MemSize a1, MemSize a2) =>
            a -> a1 -> a2 -> Int -> Int
memSize4 :: (MemSize a, MemSize a1, MemSize a2, MemSize a3) =>
            a -> a1 -> a2 -> a3 -> Int -> Int
memSize5 :: (MemSize a, MemSize a1, MemSize a2, MemSize a3, MemSize a4) =>
            a -> a1 -> a2 -> a3 -> a4 -> Int -> Int
memSize6 :: (MemSize a, MemSize a1, MemSize a2, MemSize a3, MemSize a4,
             MemSize a5) =>
            a -> a1 -> a2 -> a3 -> a4 -> a5 -> Int -> Int
memSize7 :: (MemSize a, MemSize a1, MemSize a2, MemSize a3, MemSize a4,
             MemSize a5, MemSize a6) =>
            a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Int -> Int


memSize0           = \ !sz -> sz
memSize1 a         = \ !sz -> memSize a $ 2 + sz
memSize2 a b       = \ !sz -> memSize a . memSize b $ 2 + sz
memSize3 a b c     = \ !sz -> memSize a . memSize b . memSize c $ 2 + sz
memSize4 a b c d   = \ !sz -> memSize a . memSize b . memSize c . memSize d $ 2 + sz
memSize5 a b c d
         e         = \ !sz -> memSize a . memSize b . memSize c . memSize d .
                              memSize e $ 6 + sz
memSize6 a b c d
         e f       = \ !sz -> memSize a . memSize b . memSize c . memSize d .
                              memSize e . memSize f $ 7 + sz
memSize7 a b c d
         e f g     = \ !sz -> memSize a . memSize b . memSize c . memSize d .
                              memSize e . memSize f . memSize g $ 7 + sz
{-# INLINE memSize0 #-}
{-# INLINE memSize1 #-}
{-# INLINE memSize2 #-}
{-# INLINE memSize3 #-}
{-# INLINE memSize4 #-}
{-# INLINE memSize5 #-}
{-# INLINE memSize6 #-}
{-# INLINE memSize7 #-}

instance MemSize Int where
  memSize _ = (+2)

instance MemSize Char where
  memSize _ = memSize0

instance MemSize Bool where
  memSize _ = memSize0

instance MemSize a => MemSize [a] where
  memSize []     = memSize0
  memSize (x:xs) = memSize2 x xs

instance (MemSize a, MemSize b) => MemSize (a,b) where
  memSize (a,b) = memSize2 a b

instance (MemSize a, MemSize b, MemSize c) => MemSize (a,b,c) where
  memSize (a,b,c) = memSize3 a b c

instance MemSize a => MemSize (Maybe a) where
  memSize Nothing  = memSize0
  memSize (Just a) = memSize1 a

instance (MemSize a, MemSize b) => MemSize (Either a b) where
  memSize (Left  a) = memSize1 a
  memSize (Right b) = memSize1 b


instance MemSize PackageName where
  memSize (PackageName a) = memSize1 a

instance MemSize PackageId where
  memSize (PackageId a b) = memSize2 a b

instance MemSize Version where
  memSize (Version a b) = memSize2 a b

instance MemSize VersionRange where
  memSize AnyVersion                   = memSize0
  memSize (ThisVersion            a)   = memSize1 a
  memSize (LaterVersion           a)   = memSize1 a
  memSize (EarlierVersion         a)   = memSize1 a
  memSize (WildcardVersion        a)   = memSize1 a
  memSize (UnionVersionRanges     a b) = memSize2 a b
  memSize (IntersectVersionRanges a b) = memSize2 a b
  memSize (VersionRangeParens     a)   = memSize1 a

instance MemSize Dependency where
  memSize (Dependency a b) = memSize2 a b

instance MemSize CompilerFlavor where
  memSize GHC    = memSize0
  memSize NHC    = memSize0
  memSize YHC    = memSize0
  memSize Hugs   = memSize0
  memSize HBC    = memSize0
  memSize Helium = memSize0
  memSize JHC    = memSize0
  memSize LHC    = memSize0
  memSize UHC    = memSize0
  memSize (HaskellSuite a)  = memSize1 a
  memSize (OtherCompiler a) = memSize1 a

instance MemSize License where
  memSize (GPL a)    = memSize1 a
  memSize (AGPL a)   = memSize1 a
  memSize (LGPL a)   = memSize1 a
  memSize BSD3       = memSize0
  memSize BSD4       = memSize0
  memSize MIT        = memSize0
  memSize (Apache a) = memSize1 a
  memSize PublicDomain       = memSize0
  memSize AllRightsReserved  = memSize0
  memSize OtherLicense       = memSize0
  memSize (UnknownLicense a) = memSize1 a

instance MemSize SourceRepo where
  memSize (SourceRepo a b c d e f g) = memSize7 a b c d e f g

instance MemSize RepoKind where
  memSize RepoHead  = memSize0
  memSize RepoThis  = memSize0
  memSize (RepoKindUnknown a) = memSize1 a


instance MemSize RepoType where
  memSize Darcs     = memSize0
  memSize Git       = memSize0
  memSize SVN       = memSize0
  memSize CVS       = memSize0
  memSize Mercurial = memSize0
  memSize GnuArch   = memSize0
  memSize Bazaar    = memSize0
  memSize Monotone  = memSize0
  memSize (OtherRepoType a) = memSize1 a

instance MemSize BuildType where
  memSize Simple    = memSize0
  memSize Configure = memSize0
  memSize Make      = memSize0
  memSize Custom    = memSize0
  memSize (UnknownBuildType a) = memSize1 a

instance MemSize Library where
  memSize (Library a b c) = memSize3 a b c

instance MemSize Executable where
  memSize (Executable a b c) = memSize3 a b c

instance MemSize TestSuite where
  memSize (TestSuite a b c d) = memSize4 a b c d

instance MemSize TestSuiteInterface where
  memSize (TestSuiteExeV10 a b)    = memSize2 a b
  memSize (TestSuiteLibV09 a b)    = memSize2 a b
  memSize (TestSuiteUnsupported a) = memSize1 a

instance MemSize TestType where
  memSize (TestTypeExe a) = memSize1 a
  memSize (TestTypeLib a) = memSize1 a
  memSize (TestTypeUnknown a b) = memSize2 a b

instance MemSize Benchmark where
  memSize (Benchmark a b c d) = memSize4 a b c d

instance MemSize BenchmarkInterface where
  memSize (BenchmarkExeV10 a b)    = memSize2 a b
  memSize (BenchmarkUnsupported a) = memSize1 a

instance MemSize BenchmarkType where
  memSize (BenchmarkTypeExe     a)   = memSize1 a
  memSize (BenchmarkTypeUnknown a b) = memSize2 a b

instance MemSize ModuleName where
  memSize (ModuleName a) = memSize1 a

instance MemSize BuildInfo where
  memSize (BuildInfo a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                     a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                     a21 a22 a23 a24 a25) =
    memSize a1  . memSize a2  . memSize a3  . memSize a4  . memSize a5  .
    memSize a6  . memSize a7  . memSize a8  . memSize a9  . memSize a10 .
    memSize a11 . memSize a12 . memSize a13 . memSize a14 . memSize a15 .
    memSize a16 . memSize a17 . memSize a18 . memSize a19 . memSize a20 .
    memSize a21 . memSize a22 . memSize a23 . memSize a24 . memSize a25 . (+26)


instance MemSize Language where
  memSize Haskell98   = memSize0
  memSize Haskell2010 = memSize0
  memSize (UnknownLanguage a) = memSize1 a

instance MemSize Extension where
  memSize (EnableExtension  a) = memSize1 a
  memSize (DisableExtension a) = memSize1 a
  memSize (UnknownExtension a) = memSize1 a

instance MemSize KnownExtension where
  memSize _ = memSize0

instance MemSize PackageDescription where
  memSize (PackageDescription a1  a2  a3  a4  a5  a6  a7  a8  a9  a10
                              a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                              a21 a22 a23 a24 a25 a26 a27 a28) =
    memSize a1  . memSize a2  . memSize a3  . memSize a4  . memSize a5  .
    memSize a6  . memSize a7  . memSize a8  . memSize a9  . memSize a10 .
    memSize a11 . memSize a12 . memSize a13 . memSize a14 . memSize a15 .
    memSize a16 . memSize a17 . memSize a18 . memSize a19 . memSize a20 .
    memSize a21 . memSize a22 . memSize a23 . memSize a24 . memSize a25 .
    memSize a26 . memSize a27 . memSize a28 . (+29)

instance MemSize OS where
  memSize Linux   = memSize0
  memSize Windows = memSize0
  memSize OSX     = memSize0
  memSize FreeBSD = memSize0
  memSize OpenBSD = memSize0
  memSize NetBSD  = memSize0
  memSize Solaris = memSize0
  memSize AIX     = memSize0
  memSize HPUX    = memSize0
  memSize IRIX    = memSize0
  memSize HaLVM   = memSize0
  memSize IOS     = memSize0
  memSize (OtherOS a) = memSize1 a

instance MemSize Arch where
  memSize I386   = memSize0
  memSize X86_64 = memSize0
  memSize PPC    = memSize0
  memSize PPC64  = memSize0
  memSize Sparc  = memSize0
  memSize Arm    = memSize0
  memSize Mips   = memSize0
  memSize SH     = memSize0
  memSize IA64   = memSize0
  memSize S390   = memSize0
  memSize Alpha  = memSize0
  memSize Hppa   = memSize0
  memSize Rs6000 = memSize0
  memSize M68k   = memSize0
  memSize (OtherArch a) = memSize1 a
  memSize Vax    = memSize0

instance MemSize Flag where
  memSize (MkFlag a b c d) = memSize4 a b c d

instance MemSize FlagName where
  memSize (FlagName a) = memSize1 a

instance (MemSize a, MemSize b, MemSize c) => MemSize (CondTree a b c) where
  memSize (CondNode a b c) = memSize3 a b c

instance MemSize ConfVar where
  memSize (OS   a)   = memSize1 a
  memSize (Arch a)   = memSize1 a
  memSize (Flag a)   = memSize1 a
  memSize (Impl a b) = memSize2 a b

instance MemSize a => MemSize (Condition a) where
  memSize (Var  a)   = memSize1 a
  memSize (Lit  a)   = memSize1 a
  memSize (CNot a)   = memSize1 a
  memSize (COr  a b) = memSize2 a b
  memSize (CAnd a b) = memSize2 a b

instance MemSize GenericPackageDescription where
  memSize (GenericPackageDescription a b c d e f) = memSize6 a b c d e f

