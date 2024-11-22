{-# LANGUAGE LambdaCase #-}

module GHC.Rename.String (
  rewriteInterString,
) where

import GHC.Prelude

import GHC.Builtin.Names (
  interpolateStringName,
 )
import GHC.Builtin.Types (stringTyConName)
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Pat (rnOverLit)
import GHC.Tc.Utils.Monad
import GHC.Types.Name (mkVarOcc)
import GHC.Types.Name.Set (FreeVars, emptyFVs, plusFVs)
import GHC.Types.SrcLoc (unLoc)

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

    interpolateString $ \convert raw mappend mempty ->
                raw "Hello "
      `mappend` convert (Text.toUpper name)
      `mappend` raw "!"
      `mappend` mempty

We're doing this in the renamer phase so that the expanded expression
can be typechecked as usual, without any additional work.
-}

-- | Rewrite 'HsInterStringParts' into the expanded version. Assumes expressions
-- have already been renamed, but handles renaming of overloaded strings, if
-- necessary.
rewriteInterString :: [HsInterStringPart GhcRn] -> RnM (HsExpr GhcRn, FreeVars)
rewriteInterString parts = do
  overloaded <- xoptM LangExt.OverloadedStrings
  convertName <- newName (mkVarOcc "convert")
  rawName <- newName (mkVarOcc "raw")
  mappendName <- newName (mkVarOcc "mappend")
  memptyName <- newName (mkVarOcc "mempty")

  (parts', fvs) <- unzip <$> mapM (rewritePart overloaded convertName rawName) parts
  let expr =
        (if overloaded then id else addSig) -- FIXME(bchinn): no addSig if string is qualified
          . nlHsApp (nlHsVar interpolateStringName)
          . mkLam
              [ nlVarPat convertName
              , nlVarPat rawName
              , nlVarPat mappendName
              , nlVarPat memptyName
              ]
          $ foldr (\p acc -> nlHsApps mappendName [p, acc]) (nlHsVar memptyName) parts'

  pure (unLoc expr, plusFVs fvs)
  where
    mkLam pats body = mkHsLam (noLocA pats) body

    rewritePart overloaded convertName rawName = \case
      HsInterStringRaw st s -> do
        (lit, fvs) <- mkStringLit overloaded st s
        pure (nlHsApps rawName [lit], fvs)
      HsInterStringExpr _ e ->
        pure (nlHsApps convertName [e], emptyFVs)

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

    mkStringLit overloaded st s = do
      if overloaded
        then do
          -- FIXME(bchinn): allow -XRebindableSyntax -- lookupSyntaxName
          (expr, fvs) <- rnOverLit noExtField $ OverLit noExtField (HsIsString st s)
          pure (noLocA expr, fvs)
        else
          pure (nlHsLit $ HsString st s, emptyFVs)
