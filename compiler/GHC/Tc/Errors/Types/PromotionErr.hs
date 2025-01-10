{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Tc.Errors.Types.PromotionErr ( PromotionErr(..)
                                        , pprPECategory
                                        , peCategory
                                        , TermLevelUseErr(..)
                                        , TermLevelUseCtxt(..)
                                        , pprTermLevelUseCtxt
                                        , teCategory
                                        ) where

import GHC.Prelude
import GHC.Core.Type (ThetaType)
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Generics (Generic)
import GHC.Types.Name.Reader (GlobalRdrElt, pprNameProvenance)
import GHC.Types.Name (Name, nameSrcLoc)

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors]
                     -- in GHC.Tc.Utils.Env.
  | ConstrainedDataConPE ThetaType -- Data constructor with a context
                                   -- See Note [No constraints in kinds] in GHC.Tc.Validity
  | PatSynPE         -- Pattern synonyms
                     -- See Note [Don't promote pattern synonyms] in GHC.Tc.Utils.Env

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [Recursion and promoting data constructors] in GHC.Tc.TyCl
  | TermVariablePE   -- See Note [Demotion of unqualified variables] in GHC.Rename.Env
  | TypeVariablePE   -- See Note [Type variable scoping errors during typechecking]
  deriving (Generic)

instance Outputable PromotionErr where
  ppr ClassPE              = text "ClassPE"
  ppr TyConPE              = text "TyConPE"
  ppr PatSynPE             = text "PatSynPE"
  ppr FamDataConPE         = text "FamDataConPE"
  ppr (ConstrainedDataConPE theta) = text "ConstrainedDataConPE" <+> parens (ppr theta)
  ppr RecDataConPE         = text "RecDataConPE"
  ppr TermVariablePE       = text "TermVariablePE"
  ppr TypeVariablePE       = text "TypeVariablePE"

pprPECategory :: PromotionErr -> SDoc
pprPECategory = text . capitalise . peCategory

peCategory :: PromotionErr -> String
peCategory ClassPE              = "class"
peCategory TyConPE              = "type constructor"
peCategory PatSynPE             = "pattern synonym"
peCategory FamDataConPE         = "data constructor"
peCategory ConstrainedDataConPE{} = "data constructor"
peCategory RecDataConPE         = "data constructor"
peCategory TermVariablePE       = "term variable"
peCategory TypeVariablePE       = "type variable"

-- The opposite of a promotion error (a demotion error, in a sense).
data TermLevelUseErr
  = TyConTE   -- Type constructor used at the term level, e.g. x = Int
  | ClassTE   -- Class used at the term level,            e.g. x = Functor
  | TyVarTE   -- Type variable used at the term level,    e.g. f (Proxy :: Proxy a) = a
  deriving (Generic)

teCategory :: TermLevelUseErr -> String
teCategory ClassTE = "class"
teCategory TyConTE = "type constructor"
teCategory TyVarTE = "type variable"

data TermLevelUseCtxt
  = TermLevelUseGRE !GlobalRdrElt
  | TermLevelUseTyVar
  deriving (Generic)

pprTermLevelUseCtxt :: Name -> TermLevelUseCtxt -> SDoc
pprTermLevelUseCtxt nm = \case
  TermLevelUseGRE gre -> pprNameProvenance gre
  TermLevelUseTyVar -> text "bound at" <+> ppr (nameSrcLoc nm)


{- Note [Type variable scoping errors during typechecking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the scoping of the type variable `a` in the following
term-level example:

  -- f :: [forall b . Either b ()]
  f = [Right @a @() () :: forall a. Either a ()]

Here `@a` in the type application and `a` in the type signature refer to
the same type variable. Indeed, this term elaborates to the following Core:

  f = [(\@a -> Right @a @() ()) :: forall a . Either a ()]

But how does this work with types? Suppose we have:

  type F = '[Right @a @() () :: forall a. Either a ()]

To be consistent with the term-level language, we would have to elaborate
this using a big lambda:

  type F = '[(/\ a . Right @a @() ()) :: forall a. Either a ()]

Core has no such construct, so this is not a valid type.

Conclusion: Even with -XExtendedForAllScope, the forall'd variables of a
kind signature on a type cannot scope over the type.

In implementation terms, to get a helpful error message we do this:

* The renamer treats the type variable as bound by the forall
  (so it doesn't just say "out of scope"); see the `HsKindSig` case of GHC.Rename.HsType.rnHsTyKi.

* The typechecker adds the forall-bound type variables to the type environent,
  but bound to `APromotionErr TypeVariablePE`; see the call to `tcAddKindSigPlaceholders`
  in the `HsKindSig` case of `GHC.Tc.Gen.HsType.tc_infer_hs_type`.

* The occurrence site of a type variable then complains when it finds `APromotionErr`;
  see `GHC.Tc.Gen.HsType.tcTyVar`.
-}
