{-# LANGUAGE LambdaCase #-}

module GHC.Rename.String (
  rewriteInterString,
) where

import GHC.Prelude

import GHC.Builtin.Names (
  fromBuilderName,
  interpolateName,
  mappendName,
  memptyName,
  toBuilderName,
 )
import GHC.Builtin.Types (stringTyConName)
import GHC.Data.FastString (fsLit, unpackFS)
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Pat (rnOverLit)
import GHC.Tc.Utils.Monad
import GHC.Types.Name.Set (FreeVars, emptyFVs, plusFVs)
import GHC.Types.SourceText (SourceText (..))
import GHC.Types.SrcLoc (unLoc)

import qualified Data.List.NonEmpty as NE

{- Note [Desugaring interpolated strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Cross-references:
* Note [Parsing interpolated strings]
* Note [Doing XXExprGhcRn in the Renamer vs Typechecker]

Interpolated strings are represented with the following HsExpr tree:

    HsInterString ext
      [ HsInterStringRaw ext "Hello "
      , HsInterStringExp ext $
          HsApp ext
            (HsVar ext 'Text.toUpper)
            (HsVar ext 'name)
      , HsInterStringRaw ext "!"
      ]

We'll expand this during the renamer phase into the equivalent of:

    import GHC.Internal.Data.String.Interpolate

    fromBuilder $
         toBuilder "Hello "
      <> interpolate (Text.toUpper name)
      <> toBuilder "!"

We're doing this in the renamer phase so that the expanded expression
can be typechecked as usual, without any additional work.
-}

-- | Rewrite hsInterStringParts into the expanded version. Assumes expressions
-- have already been renamed, but handles renaming of overloaded strings, if
-- necessary.
--
-- TODO(bchinn): allow -XRebindableSyntax -- lookupSyntaxName
rewriteInterString :: HsStringType -> [HsInterStringPart GhcRn] -> RnM (HsExpr GhcRn, FreeVars)
rewriteInterString strType parts = do
  overloaded <- xoptM LangExt.OverloadedStrings
  (parts', fvs) <- unzip <$> mapM (rewritePart overloaded) parts
  let expr =
        (if overloaded then id else addSig) . nlHsApp (nlHsVar fromBuilderName) $
          maybe (nlHsVar memptyName) (foldr1 appendParts) (NE.nonEmpty parts')
  pure (unLoc expr, plusFVs fvs)
  where
    appendParts l r = nlHsApps mappendName [l, r]
    rewritePart overloaded = \case
      HsInterStringRaw _ s -> do
        (lit, fvs) <- mkStringLit overloaded s
        pure (nlHsApps toBuilderName [lit], fvs)
      HsInterStringExpr _ e ->
        pure (nlHsApps interpolateName [e], emptyFVs)

    -- Add ":: String" to the given expression
    addSig e =
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

    mkStringLit overloaded s = do
      let src = SourceText $ fsLit $ "\"" ++ unpackFS s ++ "\""
      if overloaded
        then do
          (expr, fvs) <- rnOverLit noExtField $ OverLit noExtField (HsIsString src s)
          pure (noLocA expr, fvs)
        else
          pure (nlHsLit $ HsString src strType s, emptyFVs)
