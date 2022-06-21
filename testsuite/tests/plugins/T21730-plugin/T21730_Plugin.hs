module T21730_Plugin (plugin) where

import GHC.Hs
import GHC.Plugins
import GHC.Types.Fixity
import qualified GHC.Types.Name.Occurrence as NameSpace

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_ _ parsedResult ->
      pure $ parsedResult
        { parsedResultModule =
            let modl = parsedResultModule parsedResult
            in modl { hpm_module = update <$> hpm_module modl }
        }
  }
  where
    update modl =
      modl
        { hsmodImports = newImport : hsmodImports modl
        , hsmodDecls = newDecl : hsmodDecls modl
        }

    newImport = genLoc $ simpleImportDecl $ mkModuleName "Data.Char"
    newFuncName = genLoc $ mkRdrName "toLower2"
    newDecl =
      genLoc . ValD NoExtField $
        mkFunBind (Generated OtherExpansion SkipPmc) newFuncName $
          [ mkSimpleMatch
              (mkPrefixFunRhs newFuncName noAnn)
              (L noAnn [])
              (nlHsVar (mkRdrName "toLower"))
          ]

genLoc :: (NoAnn ann) => e -> GenLocated (EpAnn ann) e
genLoc = L (noAnnSrcSpan generatedSrcSpan)

mkRdrName :: String -> RdrName
mkRdrName = mkRdrUnqual . mkOccName NameSpace.varName
