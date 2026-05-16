{-# LANGUAGE LambdaCase #-}

module GHC.Rename.String (
  rewriteInterString,
) where

import GHC.Prelude

import Data.Maybe (isNothing)
import qualified GHC.Builtin.Names as Builtin
import GHC.Data.StringMeta (StringMeta (..))
import GHC.Hs
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Env (lookupNameWithQualifier, lookupSyntaxName)
import GHC.Tc.Utils.Monad
import GHC.Types.Name (Name)
import GHC.Types.Name.Set (FreeNames, emptyFNs, plusFNs)
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
rewriteInterString ::
  StringMeta ->
  [HsInterStringPart GhcRn] ->
  RnM (HsExpr GhcRn, FreeNames)
rewriteInterString meta parts = do
  mkOverloaded <- get_mkOverloaded

  let lookupName' = lookupName (strMetaQualified meta)
  (rawName, fns1) <- lookupName' Builtin.interpolateRawName
  (convertName, fns2) <- lookupName' Builtin.interpolateValueName
  (appendName, fns3) <- lookupName' Builtin.interpolateAppendName
  (emptyName, fns4) <- lookupName' Builtin.interpolateEmptyName
  (finalizeName, fns5) <- lookupName' Builtin.interpolateFinalizeName

  let expr =
        unLoc
          . mkOverloaded
          . nlHsApp (nlHsVar finalizeName)
          . foldr (\p acc -> nlHsApps appendName [p, acc]) (nlHsVar emptyName)
          $ map (rewritePart convertName rawName) parts

  pure (expr, plusFNs [fns1, fns2, fns3, fns4, fns5])
  where
    rewritePart convertName rawName = \case
      HsInterStringRaw st s -> nlHsApps rawName [nlHsLit $ HsString st s]
      HsInterStringExpr _ e -> nlHsApps convertName [e]

    -- Handle -XOverloadedStrings
    get_mkOverloaded = do
      overloaded <- xoptM LangExt.OverloadedStrings
      pure $
        if overloaded && isNothing (strMetaQualified meta)
          then nlHsApp (nlHsVar Builtin.fromStringName)
          else id

-- | Look up the given name in the following places:
--     1. If the given module is provided, in the module
--     2. If -XRebindableSyntax, any name in scope
--     3. Otherwise, return the built-in name.
lookupName :: Maybe ModuleName -> Name -> RnM (Name, FreeNames)
lookupName mQualMod name
  | Just mod <- mQualMod =
      lookupNameWithQualifier name mod
  | otherwise = do
      isRebindable <- xoptM LangExt.RebindableSyntax
      if isRebindable
        then lookupSyntaxName name
        else pure (name, emptyFNs)
