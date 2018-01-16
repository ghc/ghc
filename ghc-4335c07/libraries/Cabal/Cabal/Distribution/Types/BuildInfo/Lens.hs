module Distribution.Types.BuildInfo.Lens (
    BuildInfo,
    HasBuildInfo (..),
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Compiler                  (CompilerFlavor)
import Distribution.ModuleName                (ModuleName)
import Distribution.Types.BuildInfo           (BuildInfo)
import Distribution.Types.Dependency          (Dependency)
import Distribution.Types.ExeDependency       (ExeDependency)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency)
import Distribution.Types.Mixin               (Mixin)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency)
import Language.Haskell.Extension             (Extension, Language)

import qualified Distribution.Types.BuildInfo as T

-- | Classy lenses for 'BuildInfo'.
class HasBuildInfo a where
   buildInfo :: Lens' a BuildInfo

   buildable :: Lens' a Bool
   buildable = buildInfo . buildable
   {-# INLINE buildable #-}

   buildTools :: Lens' a [LegacyExeDependency]
   buildTools = buildInfo . buildTools
   {-# INLINE buildTools #-}

   buildToolDepends :: Lens' a [ExeDependency]
   buildToolDepends = buildInfo . buildToolDepends
   {-# INLINE buildToolDepends #-}

   cppOptions :: Lens' a [String]
   cppOptions = buildInfo . cppOptions
   {-# INLINE cppOptions #-}

   asmOptions :: Lens' a [String]
   asmOptions = buildInfo . asmOptions
   {-# INLINE asmOptions #-}

   cmmOptions :: Lens' a [String]
   cmmOptions = buildInfo . cmmOptions
   {-# INLINE cmmOptions #-}

   ccOptions :: Lens' a [String]
   ccOptions = buildInfo . ccOptions
   {-# INLINE ccOptions #-}

   cxxOptions :: Lens' a [String]
   cxxOptions = buildInfo . cxxOptions
   {-# INLINE cxxOptions #-}

   ldOptions :: Lens' a [String]
   ldOptions = buildInfo . ldOptions
   {-# INLINE ldOptions #-}

   pkgconfigDepends :: Lens' a [PkgconfigDependency]
   pkgconfigDepends = buildInfo . pkgconfigDepends
   {-# INLINE pkgconfigDepends #-}

   frameworks :: Lens' a [String]
   frameworks = buildInfo . frameworks
   {-# INLINE frameworks #-}

   extraFrameworkDirs :: Lens' a [String]
   extraFrameworkDirs = buildInfo . extraFrameworkDirs
   {-# INLINE extraFrameworkDirs #-}

   asmSources :: Lens' a [FilePath]
   asmSources = buildInfo . asmSources
   {-# INLINE asmSources #-}

   cmmSources :: Lens' a [FilePath]
   cmmSources = buildInfo . cmmSources
   {-# INLINE cmmSources #-}

   cSources :: Lens' a [FilePath]
   cSources = buildInfo . cSources
   {-# INLINE cSources #-}

   cxxSources :: Lens' a [FilePath]
   cxxSources = buildInfo . cxxSources
   {-# INLINE cxxSources #-}

   jsSources :: Lens' a [FilePath]
   jsSources = buildInfo . jsSources
   {-# INLINE jsSources #-}

   hsSourceDirs :: Lens' a [FilePath]
   hsSourceDirs = buildInfo . hsSourceDirs
   {-# INLINE hsSourceDirs #-}

   otherModules :: Lens' a [ModuleName]
   otherModules = buildInfo . otherModules
   {-# INLINE otherModules #-}

   virtualModules :: Lens' a [ModuleName]
   virtualModules = buildInfo . virtualModules
   {-# INLINE virtualModules #-}

   autogenModules :: Lens' a [ModuleName]
   autogenModules = buildInfo . autogenModules
   {-# INLINE autogenModules #-}

   defaultLanguage :: Lens' a (Maybe Language)
   defaultLanguage = buildInfo . defaultLanguage
   {-# INLINE defaultLanguage #-}

   otherLanguages :: Lens' a [Language]
   otherLanguages = buildInfo . otherLanguages
   {-# INLINE otherLanguages #-}

   defaultExtensions :: Lens' a [Extension]
   defaultExtensions = buildInfo . defaultExtensions
   {-# INLINE defaultExtensions #-}

   otherExtensions :: Lens' a [Extension]
   otherExtensions = buildInfo . otherExtensions
   {-# INLINE otherExtensions #-}

   oldExtensions :: Lens' a [Extension]
   oldExtensions = buildInfo . oldExtensions
   {-# INLINE oldExtensions #-}

   extraLibs :: Lens' a [String]
   extraLibs = buildInfo . extraLibs
   {-# INLINE extraLibs #-}

   extraGHCiLibs :: Lens' a [String]
   extraGHCiLibs = buildInfo . extraGHCiLibs
   {-# INLINE extraGHCiLibs #-}

   extraBundledLibs :: Lens' a [String]
   extraBundledLibs = buildInfo . extraBundledLibs
   {-# INLINE extraBundledLibs #-}

   extraLibFlavours :: Lens' a [String]
   extraLibFlavours = buildInfo . extraLibFlavours
   {-# INLINE extraLibFlavours #-}

   extraLibDirs :: Lens' a [String]
   extraLibDirs = buildInfo . extraLibDirs
   {-# INLINE extraLibDirs #-}

   includeDirs :: Lens' a [FilePath]
   includeDirs = buildInfo . includeDirs
   {-# INLINE includeDirs #-}

   includes :: Lens' a [FilePath]
   includes = buildInfo . includes
   {-# INLINE includes #-}

   installIncludes :: Lens' a [FilePath]
   installIncludes = buildInfo . installIncludes
   {-# INLINE installIncludes #-}

   options :: Lens' a [(CompilerFlavor,[String])]
   options = buildInfo . options
   {-# INLINE options #-}

   profOptions :: Lens' a [(CompilerFlavor,[String])]
   profOptions = buildInfo . profOptions
   {-# INLINE profOptions #-}

   sharedOptions :: Lens' a [(CompilerFlavor,[String])]
   sharedOptions = buildInfo . sharedOptions
   {-# INLINE sharedOptions #-}

   staticOptions :: Lens' a [(CompilerFlavor,[String])]
   staticOptions = buildInfo . staticOptions
   {-# INLINE staticOptions #-}

   customFieldsBI :: Lens' a [(String,String)]
   customFieldsBI = buildInfo . customFieldsBI
   {-# INLINE customFieldsBI #-}

   targetBuildDepends :: Lens' a [Dependency]
   targetBuildDepends = buildInfo . targetBuildDepends
   {-# INLINE targetBuildDepends #-}

   mixins :: Lens' a [Mixin]
   mixins = buildInfo . mixins
   {-# INLINE mixins #-}


instance HasBuildInfo BuildInfo where
    buildInfo = id
    {-# INLINE buildInfo #-}

    buildable f s = fmap (\x -> s { T.buildable = x }) (f (T.buildable s))
    {-# INLINE buildable #-}

    buildTools f s = fmap (\x -> s { T.buildTools = x }) (f (T.buildTools s))
    {-# INLINE buildTools #-}

    buildToolDepends f s = fmap (\x -> s { T.buildToolDepends = x }) (f (T.buildToolDepends s))
    {-# INLINE buildToolDepends #-}

    cppOptions f s = fmap (\x -> s { T.cppOptions = x }) (f (T.cppOptions s))
    {-# INLINE cppOptions #-}

    asmOptions f s = fmap (\x -> s { T.asmOptions = x }) (f (T.asmOptions s))
    {-# INLINE asmOptions #-}

    cmmOptions f s = fmap (\x -> s { T.cmmOptions = x }) (f (T.cmmOptions s))
    {-# INLINE cmmOptions #-}

    ccOptions f s = fmap (\x -> s { T.ccOptions = x }) (f (T.ccOptions s))
    {-# INLINE ccOptions #-}

    cxxOptions f s = fmap (\x -> s { T.cxxOptions = x }) (f (T.cxxOptions s))
    {-# INLINE cxxOptions #-}

    ldOptions f s = fmap (\x -> s { T.ldOptions = x }) (f (T.ldOptions s))
    {-# INLINE ldOptions #-}

    pkgconfigDepends f s = fmap (\x -> s { T.pkgconfigDepends = x }) (f (T.pkgconfigDepends s))
    {-# INLINE pkgconfigDepends #-}

    frameworks f s = fmap (\x -> s { T.frameworks = x }) (f (T.frameworks s))
    {-# INLINE frameworks #-}

    extraFrameworkDirs f s = fmap (\x -> s { T.extraFrameworkDirs = x }) (f (T.extraFrameworkDirs s))
    {-# INLINE extraFrameworkDirs #-}

    asmSources f s = fmap (\x -> s { T.asmSources = x }) (f (T.asmSources s))
    {-# INLINE asmSources #-}

    cmmSources f s = fmap (\x -> s { T.cmmSources = x }) (f (T.cmmSources s))
    {-# INLINE cmmSources #-}

    cSources f s = fmap (\x -> s { T.cSources = x }) (f (T.cSources s))
    {-# INLINE cSources #-}

    cxxSources f s = fmap (\x -> s { T.cSources = x }) (f (T.cxxSources s))
    {-# INLINE cxxSources #-}

    jsSources f s = fmap (\x -> s { T.jsSources = x }) (f (T.jsSources s))
    {-# INLINE jsSources #-}

    hsSourceDirs f s = fmap (\x -> s { T.hsSourceDirs = x }) (f (T.hsSourceDirs s))
    {-# INLINE hsSourceDirs #-}

    otherModules f s = fmap (\x -> s { T.otherModules = x }) (f (T.otherModules s))
    {-# INLINE otherModules #-}

    virtualModules f s = fmap (\x -> s { T.virtualModules = x }) (f (T.virtualModules s))
    {-# INLINE virtualModules #-}

    autogenModules f s = fmap (\x -> s { T.autogenModules = x }) (f (T.autogenModules s))
    {-# INLINE autogenModules #-}

    defaultLanguage f s = fmap (\x -> s { T.defaultLanguage = x }) (f (T.defaultLanguage s))
    {-# INLINE defaultLanguage #-}

    otherLanguages f s = fmap (\x -> s { T.otherLanguages = x }) (f (T.otherLanguages s))
    {-# INLINE otherLanguages #-}

    defaultExtensions f s = fmap (\x -> s { T.defaultExtensions = x }) (f (T.defaultExtensions s))
    {-# INLINE defaultExtensions #-}

    otherExtensions f s = fmap (\x -> s { T.otherExtensions = x }) (f (T.otherExtensions s))
    {-# INLINE otherExtensions #-}

    oldExtensions f s = fmap (\x -> s { T.oldExtensions = x }) (f (T.oldExtensions s))
    {-# INLINE oldExtensions #-}

    extraLibs f s = fmap (\x -> s { T.extraLibs = x }) (f (T.extraLibs s))
    {-# INLINE extraLibs #-}

    extraGHCiLibs f s = fmap (\x -> s { T.extraGHCiLibs = x }) (f (T.extraGHCiLibs s))
    {-# INLINE extraGHCiLibs #-}

    extraBundledLibs f s = fmap (\x -> s { T.extraBundledLibs = x }) (f (T.extraBundledLibs s))
    {-# INLINE extraBundledLibs #-}

    extraLibFlavours f s = fmap (\x -> s { T.extraLibFlavours = x }) (f (T.extraLibFlavours s))
    {-# INLINE extraLibFlavours #-}

    extraLibDirs f s = fmap (\x -> s { T.extraLibDirs = x }) (f (T.extraLibDirs s))
    {-# INLINE extraLibDirs #-}

    includeDirs f s = fmap (\x -> s { T.includeDirs = x }) (f (T.includeDirs s))
    {-# INLINE includeDirs #-}

    includes f s = fmap (\x -> s { T.includes = x }) (f (T.includes s))
    {-# INLINE includes #-}

    installIncludes f s = fmap (\x -> s { T.installIncludes = x }) (f (T.installIncludes s))
    {-# INLINE installIncludes #-}

    options f s = fmap (\x -> s { T.options = x }) (f (T.options s))
    {-# INLINE options #-}

    profOptions f s = fmap (\x -> s { T.profOptions = x }) (f (T.profOptions s))
    {-# INLINE profOptions #-}

    sharedOptions f s = fmap (\x -> s { T.sharedOptions = x }) (f (T.sharedOptions s))
    {-# INLINE sharedOptions #-}

    staticOptions f s = fmap (\x -> s { T.staticOptions = x }) (f (T.staticOptions s))
    {-# INLINE staticOptions #-}

    customFieldsBI f s = fmap (\x -> s { T.customFieldsBI = x }) (f (T.customFieldsBI s))
    {-# INLINE customFieldsBI #-}

    targetBuildDepends f s = fmap (\x -> s { T.targetBuildDepends = x }) (f (T.targetBuildDepends s))
    {-# INLINE targetBuildDepends #-}

    mixins f s = fmap (\x -> s { T.mixins = x }) (f (T.mixins s))
    {-# INLINE mixins #-}
