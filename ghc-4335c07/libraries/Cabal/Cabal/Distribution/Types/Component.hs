{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Component (
    Component(..),
    foldComponent,
    componentBuildInfo,
    componentBuildable,
    componentName,
    partitionComponents,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark

import Distribution.Types.ComponentName
import Distribution.Types.BuildInfo

import qualified Distribution.Types.BuildInfo.Lens as L

data Component = CLib   Library
               | CFLib  ForeignLib
               | CExe   Executable
               | CTest  TestSuite
               | CBench Benchmark
               deriving (Show, Eq, Read)

instance Semigroup Component where
    CLib   l <> CLib   l' = CLib   (l <> l')
    CFLib  l <> CFLib  l' = CFLib  (l <> l')
    CExe   e <> CExe   e' = CExe   (e <> e')
    CTest  t <> CTest  t' = CTest  (t <> t')
    CBench b <> CBench b' = CBench (b <> b')
    _        <> _         = error "Cannot merge Component"

instance L.HasBuildInfo Component where
    buildInfo f (CLib l)   = CLib <$> L.buildInfo f l
    buildInfo f (CFLib l)  = CFLib <$> L.buildInfo f l
    buildInfo f (CExe e)   = CExe <$> L.buildInfo f e
    buildInfo f (CTest t)  = CTest <$> L.buildInfo f t
    buildInfo f (CBench b) = CBench <$> L.buildInfo f b

foldComponent :: (Library -> a)
              -> (ForeignLib -> a)
              -> (Executable -> a)
              -> (TestSuite -> a)
              -> (Benchmark -> a)
              -> Component
              -> a
foldComponent f _ _ _ _ (CLib   lib) = f lib
foldComponent _ f _ _ _ (CFLib  flib)= f flib
foldComponent _ _ f _ _ (CExe   exe) = f exe
foldComponent _ _ _ f _ (CTest  tst) = f tst
foldComponent _ _ _ _ f (CBench bch) = f bch

componentBuildInfo :: Component -> BuildInfo
componentBuildInfo =
  foldComponent libBuildInfo foreignLibBuildInfo buildInfo testBuildInfo benchmarkBuildInfo

-- | Is a component buildable (i.e., not marked with @buildable: False@)?
-- See also this note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.2
--
componentBuildable :: Component -> Bool
componentBuildable = buildable . componentBuildInfo

componentName :: Component -> ComponentName
componentName =
  foldComponent (libraryComponentName . libName)
                (CFLibName  . foreignLibName)
                (CExeName   . exeName)
                (CTestName  . testName)
                (CBenchName . benchmarkName)

partitionComponents
    :: [Component]
    -> ([Library], [ForeignLib], [Executable], [TestSuite], [Benchmark])
partitionComponents = foldr (foldComponent fa fb fc fd fe) ([],[],[],[],[])
  where
    fa x ~(a,b,c,d,e) = (x:a,b,c,d,e)
    fb x ~(a,b,c,d,e) = (a,x:b,c,d,e)
    fc x ~(a,b,c,d,e) = (a,b,x:c,d,e)
    fd x ~(a,b,c,d,e) = (a,b,c,x:d,e)
    fe x ~(a,b,c,d,e) = (a,b,c,d,x:e)
