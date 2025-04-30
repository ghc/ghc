{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Rename.String (
  rewriteInterString,
) where

import GHC.Prelude

import GHC.Builtin.Names (
  interpolateName,
  mconcatName,
 )
import GHC.Builtin.Types (stringTyConName)
import GHC.Data.FastString (fsLit, unpackFS)
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Env (lookupOccRn)
import GHC.Rename.Pat (rnOverLit)
import GHC.Tc.Errors.Types (WhatLooking (WL_None))
import GHC.Tc.Utils.Monad
import GHC.Types.Name (Name)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrQual)
import GHC.Types.Name.Set (FreeVars, emptyFVs, plusFVs)
import GHC.Types.SourceText (SourceText (..))
import GHC.Types.SrcLoc (unLoc)

{- Note [Desugaring interpolated strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Cross-references:
* Note [Parsing interpolated strings]
* Note [Doing XXExprGhcRn in the Renamer vs Typechecker]

Interpolated strings are represented with the following HsExpr tree:

    HsInterString ext mQualMod strType
      [ HsInterStringRaw ext "Hello "
      , HsInterStringExp ext $
          HsApp ext
            (HsVar ext 'Text.toUpper)
            (HsVar ext 'name)
      , HsInterStringRaw ext "!"
      ]

We'll expand this during the renamer phase into the equivalent of:

    mconcat
      [ fromString "Hello "
      , interpolate (Text.toUpper name)
      , fromString "!"
      ]

If using QualifiedLiterals (mQualMod is Just), expand to:

    ModName.fromParts
      [ ModName.fromString "Hello "
      , ModName.interpolate (Text.toUpper name)
      , ModName.fromString "!"
      ]

We're doing this in the renamer phase so that the expanded expression
can be typechecked as usual, without any additional work.
-}

-- | Rewrite hsInterStringParts into the expanded version. Assumes expressions
-- have already been renamed, but handles renaming of overloaded strings, if
-- necessary.
--
-- TODO(bchinn): allow -XRebindableSyntax -- lookupSyntaxName
rewriteInterString ::
  Maybe ModuleName
  -> HsStringType
  -> [HsInterStringPart GhcRn]
  -> RnM (HsExpr GhcRn, FreeVars)
rewriteInterString mQualMod strType parts = do
  overloaded <- xoptM LangExt.OverloadedStrings
  mQualNames <- traverse lookupQualifiedLiteralStringsNames mQualMod
  rewriteInterStringImpl overloaded mQualNames strType parts

rewriteInterStringImpl ::
  Bool
  -> Maybe QualifiedLiteralStringsNames
  -> HsStringType
  -> [HsInterStringPart GhcRn]
  -> RnM (HsExpr GhcRn, FreeVars)
rewriteInterStringImpl overloaded mQualNames strType parts = do
  (parts', fvs) <- unzip <$> mapM rewritePart parts
  let expr =
        addSig
        . (nlHsApp $ nlHsVar $ maybe mconcatName qualFromParts mQualNames)
        $ noLocA (ExplicitList noExtField parts')
  pure (unLoc expr, plusFVs fvs)
  where
    rewritePart = \case
      HsInterStringRaw _ s -> mkStringLit s
      HsInterStringExpr _ e -> do
        let interpolateName' = maybe interpolateName qualInterpolate mQualNames
        pure (nlHsApp (nlHsVar interpolateName') e, emptyFVs)

    addSig e
      | Just _ <- mQualNames = e
      | overloaded = e
      | otherwise =
          -- explicitly add ":: String" if not overloaded
          noLocA . ExprWithTySig noExtField e $
            HsWC
              { hswc_ext = []
              , hswc_body =
                  noLocA
                    HsSig
                      { sig_ext   = noExtField
                      , sig_bndrs = HsOuterImplicit []
                      , sig_body  = nlHsTyVar NotPromoted stringTyConName
                      }
              }

    mkStringLit s = do
      let src = SourceText $ fsLit $ "\"" ++ unpackFS s ++ "\""
      let lit = nlHsLit $ HsString src strType s
      if
        | Just qualNames <- mQualNames -> do
            pure (nlHsApp (nlHsVar $ qualFromString qualNames) lit, emptyFVs)
        | overloaded -> do
            (expr, fvs) <- rnOverLit noExtField $ OverLit noExtField (HsIsString src s)
            pure (noLocA expr, fvs)
        | otherwise -> do
            pure (lit, emptyFVs)

data QualifiedLiteralStringsNames = QualifiedLiteralStringsNames
  { qualFromString :: Name
  , qualInterpolate :: Name
  , qualFromParts :: Name
  }

lookupQualifiedLiteralStringsNames ::
  ModuleName -> RnM QualifiedLiteralStringsNames
lookupQualifiedLiteralStringsNames modName = do
  qualFromString <- lookup "fromString"
  qualInterpolate <- lookup "interpolate"
  qualFromParts <- lookup "fromParts"
  pure QualifiedLiteralStringsNames{..}
  where
    lookup = lookupOccRn WL_None . mkRdrQual modName . mkVarOcc
