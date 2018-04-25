-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.PrettyPrint
-- Copyright   :  Jürgen Nicklisch-Franken 2010
-- License     :  BSD3
--
-- Maintainer  : cabal-devel@haskell.org
-- Stability   : provisional
-- Portability : portable
--
-- Pretty printing for cabal files
--
-----------------------------------------------------------------------------

module Distribution.PackageDescription.PrettyPrint (
    -- * Generic package descriptions
    writeGenericPackageDescription,
    showGenericPackageDescription,

    -- * Package descriptions
     writePackageDescription,
     showPackageDescription,

     -- ** Supplementary build information
     writeHookedBuildInfo,
     showHookedBuildInfo,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.ForeignLib (ForeignLib (foreignLibName))
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import Distribution.PackageDescription
import Distribution.Simple.Utils
import Distribution.ParseUtils
import Distribution.Text

import Distribution.FieldGrammar (PrettyFieldGrammar', prettyFieldGrammar)
import Distribution.PackageDescription.FieldGrammar
       (packageDescriptionFieldGrammar, buildInfoFieldGrammar,
        flagFieldGrammar, foreignLibFieldGrammar, libraryFieldGrammar,
        benchmarkFieldGrammar, testSuiteFieldGrammar,
        setupBInfoFieldGrammar, sourceRepoFieldGrammar, executableFieldGrammar)

import qualified Distribution.PackageDescription.FieldGrammar as FG

import Text.PrettyPrint
       (hsep, space, parens, char, nest, ($$), (<+>),
        text, vcat, ($+$), Doc, render)

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

-- | Writes a .cabal file from a generic package description
writeGenericPackageDescription :: FilePath -> GenericPackageDescription -> NoCallStackIO ()
writeGenericPackageDescription fpath pkg = writeUTF8File fpath (showGenericPackageDescription pkg)

-- | Writes a generic package description to a string
showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription            = render . ppGenericPackageDescription

ppGenericPackageDescription :: GenericPackageDescription -> Doc
ppGenericPackageDescription gpd          =
        ppPackageDescription (packageDescription gpd)
        $+$ ppSetupBInfo (setupBuildInfo (packageDescription gpd))
        $+$ ppGenPackageFlags (genPackageFlags gpd)
        $+$ ppCondLibrary (condLibrary gpd)
        $+$ ppCondSubLibraries (condSubLibraries gpd)
        $+$ ppCondForeignLibs (condForeignLibs gpd)
        $+$ ppCondExecutables (condExecutables gpd)
        $+$ ppCondTestSuites (condTestSuites gpd)
        $+$ ppCondBenchmarks (condBenchmarks gpd)

ppPackageDescription :: PackageDescription -> Doc
ppPackageDescription pd =
    prettyFieldGrammar packageDescriptionFieldGrammar pd
    $+$ ppSourceRepos (sourceRepos pd)

ppSourceRepos :: [SourceRepo] -> Doc
ppSourceRepos []                         = mempty
ppSourceRepos (hd:tl)                    = ppSourceRepo hd $+$ ppSourceRepos tl

ppSourceRepo :: SourceRepo -> Doc
ppSourceRepo repo =
    emptyLine $ text "source-repository" <+> disp kind $+$
    nest indentWith (prettyFieldGrammar (sourceRepoFieldGrammar kind) repo)
  where
    kind = repoKind repo

ppSetupBInfo :: Maybe SetupBuildInfo -> Doc
ppSetupBInfo Nothing = mempty
ppSetupBInfo (Just sbi)
    | defaultSetupDepends sbi = mempty
    | otherwise =
        emptyLine $ text "custom-setup" $+$
        nest indentWith (prettyFieldGrammar (setupBInfoFieldGrammar False) sbi)

ppGenPackageFlags :: [Flag] -> Doc
ppGenPackageFlags flds = vcat [ppFlag f | f <- flds]

ppFlag :: Flag -> Doc
ppFlag flag@(MkFlag name _ _ _)  =
    emptyLine $ text "flag" <+> ppFlagName name $+$
    nest indentWith (prettyFieldGrammar (flagFieldGrammar name) flag)

ppCondTree2 :: PrettyFieldGrammar' s -> CondTree ConfVar [Dependency] s -> Doc
ppCondTree2 grammar = go
  where
    -- TODO: recognise elif opportunities
    go (CondNode it _ ifs) =
        prettyFieldGrammar grammar it
        $+$ vcat (map ppIf ifs)

    ppIf (CondBranch c thenTree Nothing)
--        | isEmpty thenDoc = mempty
        | otherwise       = ppIfCondition c $$ nest indentWith thenDoc
      where
        thenDoc = go thenTree

    ppIf (CondBranch c thenTree (Just elseTree)) =
          case (False, False) of
 --       case (isEmpty thenDoc, isEmpty elseDoc) of
              (True,  True)  -> mempty
              (False, True)  -> ppIfCondition c $$ nest indentWith thenDoc
              (True,  False) -> ppIfCondition (cNot c) $$ nest indentWith elseDoc
              (False, False) -> (ppIfCondition c $$ nest indentWith thenDoc)
                                $+$ (text "else" $$ nest indentWith elseDoc)
      where
        thenDoc = go thenTree
        elseDoc = go elseTree

ppCondLibrary :: Maybe (CondTree ConfVar [Dependency] Library) -> Doc
ppCondLibrary Nothing = mempty
ppCondLibrary (Just condTree) =
    emptyLine $ text "library" $+$
    nest indentWith (ppCondTree2 (libraryFieldGrammar Nothing) condTree)

ppCondSubLibraries :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> Doc
ppCondSubLibraries libs = vcat
    [ emptyLine $ (text "library" <+> disp n) $+$
      nest indentWith (ppCondTree2 (libraryFieldGrammar $ Just n) condTree)
    | (n, condTree) <- libs
    ]

ppCondForeignLibs :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> Doc
ppCondForeignLibs flibs = vcat
    [ emptyLine $ (text "library" <+> disp n) $+$
      nest indentWith (ppCondTree2 (foreignLibFieldGrammar n) condTree)
    | (n, condTree) <- flibs
    ]

ppCondExecutables :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> Doc
ppCondExecutables exes = vcat
    [ emptyLine $ (text "executable" <+> disp n) $+$
      nest indentWith (ppCondTree2 (executableFieldGrammar n) condTree)
    | (n, condTree) <- exes
    ]

ppCondTestSuites :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> Doc
ppCondTestSuites suites = vcat
    [ emptyLine $ (text "test-suite" <+> disp n) $+$
      nest indentWith (ppCondTree2 testSuiteFieldGrammar (fmap FG.unvalidateTestSuite condTree))
    | (n, condTree) <- suites
    ]

ppCondBenchmarks :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> Doc
ppCondBenchmarks suites = vcat
    [ emptyLine $ (text "benchmark" <+> disp n) $+$
      nest indentWith (ppCondTree2 benchmarkFieldGrammar (fmap FG.unvalidateBenchmark condTree))
    | (n, condTree) <- suites
    ]

ppCondition :: Condition ConfVar -> Doc
ppCondition (Var x)                      = ppConfVar x
ppCondition (Lit b)                      = text (show b)
ppCondition (CNot c)                     = char '!' <<>> (ppCondition c)
ppCondition (COr c1 c2)                  = parens (hsep [ppCondition c1, text "||"
                                                         <+> ppCondition c2])
ppCondition (CAnd c1 c2)                 = parens (hsep [ppCondition c1, text "&&"
                                                         <+> ppCondition c2])
ppConfVar :: ConfVar -> Doc
ppConfVar (OS os)                        = text "os"   <<>> parens (disp os)
ppConfVar (Arch arch)                    = text "arch" <<>> parens (disp arch)
ppConfVar (Flag name)                    = text "flag" <<>> parens (ppFlagName name)
ppConfVar (Impl c v)                     = text "impl" <<>> parens (disp c <+> disp v)

ppFlagName :: FlagName -> Doc
ppFlagName                               = text . unFlagName

ppIfCondition :: (Condition ConfVar) -> Doc
ppIfCondition c = (emptyLine $ text "if" <+> ppCondition c)

emptyLine :: Doc -> Doc
emptyLine d                              = text "" $+$ d

-- | @since 2.0.0.2
writePackageDescription :: FilePath -> PackageDescription -> NoCallStackIO ()
writePackageDescription fpath pkg = writeUTF8File fpath (showPackageDescription pkg)

--TODO: make this use section syntax
-- add equivalent for GenericPackageDescription

-- | @since 2.0.0.2
showPackageDescription :: PackageDescription -> String
showPackageDescription = showGenericPackageDescription . pdToGpd

pdToGpd :: PackageDescription -> GenericPackageDescription
pdToGpd pd = GenericPackageDescription
    { packageDescription = pd
    , genPackageFlags    = []
    , condLibrary        = mkCondTree <$> library pd
    , condSubLibraries   = mkCondTreeL <$> subLibraries pd
    , condForeignLibs    = mkCondTree' foreignLibName <$> foreignLibs pd
    , condExecutables    = mkCondTree' exeName <$> executables pd
    , condTestSuites     = mkCondTree' testName <$> testSuites pd
    , condBenchmarks     = mkCondTree' benchmarkName <$> benchmarks pd
    }
  where
    -- We set CondTree's [Dependency] to an empty list, as it
    -- is not pretty printed anyway.
    mkCondTree  x = CondNode x [] []
    mkCondTreeL l = (fromMaybe (mkUnqualComponentName "") (libName l), CondNode l [] [])

    mkCondTree'
        :: (a -> UnqualComponentName)
        -> a -> (UnqualComponentName, CondTree ConfVar [Dependency] a)
    mkCondTree' f x = (f x, CondNode x [] [])

-- | @since 2.0.0.2
writeHookedBuildInfo :: FilePath -> HookedBuildInfo -> NoCallStackIO ()
writeHookedBuildInfo fpath = writeFileAtomic fpath . BS.Char8.pack
                             . showHookedBuildInfo

-- | @since 2.0.0.2
showHookedBuildInfo :: HookedBuildInfo -> String
showHookedBuildInfo (mb_lib_bi, ex_bis) = render $
    maybe mempty (prettyFieldGrammar buildInfoFieldGrammar) mb_lib_bi
    $$ vcat
        [ space
        $$ (text "executable:" <+> disp name)
        $$  prettyFieldGrammar buildInfoFieldGrammar bi
        | (name, bi) <- ex_bis
        ]
