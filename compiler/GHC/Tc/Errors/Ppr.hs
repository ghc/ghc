{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage
{-# LANGUAGE InstanceSigs #-}

module GHC.Tc.Errors.Ppr
  ( pprTypeDoesNotHaveFixedRuntimeRep
  , pprScopeError
  , pprErrCtxtMsg
  --
  , tidySkolemInfo
  , tidySkolemInfoAnon
  --
  , pprHsDocContext
  , inHsDocContext
  , TcRnMessageOpts(..)
  , pprTyThingUsedWrong
  , pprUntouchableVariable

  --
  , mismatchMsg_ExpectedActuals

  -- | Useful when overriding message printing.
  , messageWithInfoDiagnosticMessage
  , messageWithHsDocContext
  )
  where

import GHC.Prelude

import qualified GHC.Boot.TH.Syntax as TH
-- In stage1: import "ghc-boot-th-next" qualified GHC.Boot.TH.Syntax as TH
-- In stage2: import "ghc-boot-th"      qualified GHC.Boot.TH.Syntax as TH
--            which is a rexport of
--            import "ghc-internal"     qualified GHC.Internal.TH.Syntax as TH
import qualified GHC.Boot.TH.Ppr as TH

import GHC.Builtin.Names
import GHC.Builtin.Types ( boxedRepDataConTyCon, tYPETyCon, pretendNameIsInScope )

import GHC.Types.Name.Reader
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Warnings

import GHC.Core.Coercion
import GHC.Core.Unify     ( tcMatchTys )
import GHC.Core.TyCon
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.Coercion.Axiom (CoAxBranch, coAxiomTyCon, coAxiomSingleBranch)
import GHC.Core.ConLike
import GHC.Core.FamInstEnv ( FamInst(..), famInstAxiom, pprFamInst )
import GHC.Core.InstEnv
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCo.Ppr (pprWithInvisibleBitsWhen, pprSourceTyCon,
                          pprTyVars, pprWithTYPE, pprTyVar, pprTidiedType, pprForAll)
import GHC.Core.PatSyn ( patSynName, pprPatSynType )
import GHC.Core.TyCo.Tidy
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.FVs( orphNamesOfTypes )
import GHC.CoreToIface

import GHC.Driver.Flags
import GHC.Driver.Backend
import GHC.Hs hiding (HoleError)

import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Types.BasicTypes
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Origin hiding ( Position(..) )
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Rank (Rank(..))
import GHC.Tc.Types.TH
import GHC.Tc.Utils.TcType

import GHC.Types.DefaultEnv (ClassDefaults(ClassDefaults, cd_types, cd_module))
import GHC.Types.Error
import GHC.Types.Error.Codes
import GHC.Types.Hint
import GHC.Types.Hint.Ppr ( pprSigLike ) -- & Outputable GhcHint
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Id.Info ( RecSelParent(..) )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.TyThing.Ppr ( pprTyThingInContext )
import GHC.Types.Unique.Set ( nonDetEltsUniqSet )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Fixity (defaultFixity)

import GHC.Iface.Errors.Types
import GHC.Iface.Errors.Ppr
import GHC.Iface.Syntax

import GHC.Unit.State
import GHC.Unit.Module

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.List.SetOps ( nubOrdBy )
import GHC.Data.Maybe
import GHC.Data.Pair
import GHC.Settings.Constants (mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.BooleanFormula (pprBooleanFormulaNice)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Foldable ( fold )
import Data.Function (on)
import Data.List ( groupBy, sortBy, tails
                 , partition, unfoldr )
import Data.Ord ( comparing )
import Data.Bifunctor
import GHC.Tc.Errors.Types.PromotionErr (pprTermLevelUseCtxt)


defaultTcRnMessageOpts :: TcRnMessageOpts
defaultTcRnMessageOpts = TcRnMessageOpts { tcOptsShowContext = True
                                         , tcOptsIfaceOpts = defaultDiagnosticOpts @IfaceMessage }

instance HasDefaultDiagnosticOpts TcRnMessageOpts where
  defaultOpts = defaultTcRnMessageOpts

instance Diagnostic TcRnMessage where
  type DiagnosticOpts TcRnMessage = TcRnMessageOpts
  diagnosticMessage opts = \case
    TcRnUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m
    TcRnMessageWithInfo unit_state msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed err_info msg
             -> messageWithInfoDiagnosticMessage unit_state err_info
                  (tcOptsShowContext opts)
                  (diagnosticMessage opts msg)
    TcRnWithHsDocContext ctxt msg
      -> messageWithHsDocContext opts ctxt (diagnosticMessage opts msg)
    TcRnSolverReport msg _reason
      -> mkSimpleDecorated $ pprSolverReportWithCtxt msg
    TcRnSolverDepthError ty depth -> mkSimpleDecorated msg
      where
        msg =
          vcat [ text "Reduction stack overflow; size =" <+> ppr depth
               , hang (text "When simplifying the following type:")
                    2 (ppr ty) ]
    TcRnRedundantConstraints redundants (info, show_info)
      -> mkSimpleDecorated $
         text "Redundant constraint" <> plural redundants <> colon
           <+> pprEvVarTheta redundants
         $$ if show_info then text "In" <+> ppr info else empty
    TcRnInaccessibleCode implic contra
      -> mkSimpleDecorated $
         hang (text "Inaccessible code in")
           2 (ppr (ic_info implic))
         $$ pprSolverReportWithCtxt contra
    TcRnInaccessibleCoAxBranch fam_tc cur_branch
      -> mkSimpleDecorated $
          text "Type family instance equation is overlapped:" $$
          nest 2 (pprCoAxBranchUser fam_tc cur_branch)
    TcRnTypeDoesNotHaveFixedRuntimeRep ty prov err_ctxt
      -> mkDecorated $
          (pprTypeDoesNotHaveFixedRuntimeRep ty prov)
          : map pprErrCtxtMsg err_ctxt
    TcRnImplicitLift id_or_name err_ctxt
      -> mkDecorated $
           ( text "The variable" <+> quotes (ppr id_or_name) <+>
             text "is implicitly lifted in the TH quotation"
           ) : map pprErrCtxtMsg err_ctxt
    TcRnUnusedPatternBinds bind
      -> mkDecorated [hang (text "This pattern-binding binds no variables:") 2 (ppr bind)]
    TcRnDodgyImports (DodgyImportsEmptyParent gre)
      -> mkDecorated [dodgy_msg (text "import") gre (dodgy_msg_insert gre)]
    TcRnDodgyImports (DodgyImportsHiding reason)
      -> mkSimpleDecorated $
         pprImportLookup reason
    TcRnDodgyExports gre
      -> mkDecorated [dodgy_msg (text "export") gre (dodgy_msg_insert gre)]
    TcRnMissingImportList ie
      -> mkDecorated [ text "The import item" <+> quotes (ppr ie) <+>
                       text "does not have an explicit import list"
                     ]
    TcRnUnsafeDueToPlugin
      -> mkDecorated [text "Use of plugins makes the module unsafe"]
    TcRnModMissingRealSrcSpan mod
      -> mkDecorated [text "Module does not have a RealSrcSpan:" <+> ppr mod]
    TcRnIdNotExportedFromModuleSig name mod
      -> mkDecorated [ text "The identifier" <+> ppr (occName name) <+>
                       text "does not exist in the signature for" <+> ppr mod
                     ]
    TcRnIdNotExportedFromLocalSig name
      -> mkDecorated [ text "The identifier" <+> ppr (occName name) <+>
                       text "does not exist in the local signature."
                     ]
    TcRnShadowedName occ provenance
      -> let shadowed_locs = case provenance of
               ShadowedNameProvenanceLocal n     -> [text "bound at" <+> ppr n]
               ShadowedNameProvenanceGlobal gres -> map pprNameProvenance gres
         in mkSimpleDecorated $
            sep [text "This binding for" <+> quotes (ppr occ)
             <+> text "shadows the existing binding" <> plural shadowed_locs,
                   nest 2 (vcat shadowed_locs)]
    TcRnInvalidWarningCategory cat
      -> mkSimpleDecorated $
           vcat [text "Warning category" <+> quotes (ppr cat) <+> text "is not valid",
                 text "(user-defined category names must begin with" <+> quotes (text "x-"),
                 text "and contain only letters, numbers, apostrophes and dashes)" ]
    TcRnDuplicateWarningDecls d rdr_name
      -> mkSimpleDecorated $
           vcat [text "Multiple warning declarations for" <+> quotes (ppr rdr_name),
                 text "also at " <+> ppr (getLocA d)]
    TcRnSimplifierTooManyIterations simples limit wc
      -> mkSimpleDecorated $
           hang (text "solveWanteds: too many iterations"
                   <+> parens (text "limit =" <+> ppr limit))
                2 (vcat [ text "Unsolved:" <+> ppr wc
                        , text "Simples:"  <+> ppr simples
                        ])
    TcRnIllegalPatSynDecl rdrname
      -> mkSimpleDecorated $
           hang (text "Illegal pattern synonym declaration for" <+> quotes (ppr rdrname))
              2 (text "Pattern synonym declarations are only valid at top level")
    TcRnLinearPatSyn ty
      -> mkSimpleDecorated $
           hang (text "Pattern synonyms do not support linear fields (GHC #18806):") 2 (ppr ty)
    TcRnEmptyRecordUpdate
      -> mkSimpleDecorated $ text "Empty record update"
    TcRnIllegalFieldPunning fld
      -> mkSimpleDecorated $ text "Illegal use of punning for field" <+> quotes (ppr fld)
    TcRnIllegalWildcardsInRecord fld_part
      -> mkSimpleDecorated $ text "Illegal `..' in record" <+> pprRecordFieldPart fld_part
    TcRnIllegalWildcardInType mb_name bad
      -> mkSimpleDecorated $ case bad of
          WildcardNotLastInConstraint ->
            hang notAllowed 2 constraint_hint_msg
          ExtraConstraintWildcardNotAllowed allow_sole ->
            case allow_sole of
              SoleExtraConstraintWildcardNotAllowed ->
                notAllowed
              SoleExtraConstraintWildcardAllowed ->
                hang notAllowed 2 sole_msg
          WildcardsNotAllowedAtAll ->
            notAllowed
          WildcardBndrInForallTelescope ->
            notAllowed
          WildcardBndrInTyFamResultVar ->
            notAllowed
      where
        notAllowed, what, wildcard, how :: SDoc
        notAllowed = what <+> quotes wildcard <+> how
        wildcard = case mb_name of
          Nothing   -> pprAnonWildCard
          Just name -> ppr name
        what
          | Just _ <- mb_name
          = text "Named wildcard"
          | ExtraConstraintWildcardNotAllowed {} <- bad
          = text "Extra-constraint wildcard"
          | WildcardBndrInForallTelescope {} <- bad
          = text "Wildcard binder"
          | WildcardBndrInTyFamResultVar {} <- bad
          = text "Wildcard binder"
          | otherwise
          = text "Wildcard"
        how = case bad of
          WildcardNotLastInConstraint
            -> text "not allowed in a constraint"
          WildcardBndrInForallTelescope
            -> text "not allowed in a forall telescope"
          WildcardBndrInTyFamResultVar
            -> text "not allowed in a type family result"
          _ -> text "not allowed"
        constraint_hint_msg :: SDoc
        constraint_hint_msg
          | Just _ <- mb_name
          = vcat [ text "Extra-constraint wildcards must be anonymous"
                 , nest 2 (text "e.g  f :: (Eq a, _) => blah") ]
          | otherwise
          = vcat [ text "except as the last top-level constraint of a type signature"
                 , nest 2 (text "e.g  f :: (Eq a, _) => blah") ]
        sole_msg :: SDoc
        sole_msg =
          vcat [ text "except as the sole constraint"
               , nest 2 (text "e.g., deriving instance _ => Eq (Foo a)") ]
    TcRnIllegalNamedWildcardInTypeArgument rdr
      -> mkSimpleDecorated $
           hang (text "Illegal named wildcard in a required type argument:")
                2 (quotes (ppr rdr))
    TcRnIllegalImplicitTyVarInTypeArgument rdr
      -> mkSimpleDecorated $
            hang (text "Illegal implicitly quantified type variable in a required type argument:")
                2 (quotes (ppr rdr))
    TcRnDuplicateFieldName fld_part dups
      -> mkSimpleDecorated $
           hsep [ text "Duplicate field name"
                , quotes (ppr (rdrNameOcc $ NE.head dups))
                , text "in record", pprRecordFieldPart fld_part ]
    TcRnIllegalViewPattern pat
      -> mkSimpleDecorated $ vcat [text "Illegal view pattern: " <+> ppr pat]
    TcRnCharLiteralOutOfRange c
      -> mkSimpleDecorated $ text "character literal out of range: '\\" <> char c  <> char '\''
    TcRnIllegalWildcardsInConstructor con
      -> mkSimpleDecorated $
           vcat [ text "Illegal `{..}' notation for constructor" <+> quotes (ppr con)
                , nest 2 (text "Record wildcards may not be used for constructors with unlabelled fields.")
                , nest 2 (text "Possible fix: Remove the `{..}' and add a match for each field of the constructor.")
                ]
    TcRnIgnoringAnnotations anns
      -> mkSimpleDecorated $
           text "Ignoring ANN annotation" <> plural anns <> comma
           <+> text "because this is a stage-1 compiler without -fexternal-interpreter or doesn't support GHCi"
    TcRnAnnotationInSafeHaskell
      -> mkSimpleDecorated $
           vcat [ text "Annotations are not compatible with Safe Haskell."
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/10826" ]
    TcRnInvalidTypeApplication fun_ty hs_ty
      -> mkSimpleDecorated $
           text "Cannot apply expression of type" <+> quotes (ppr fun_ty) $$
           text "to a visible type argument" <+> quotes (ppr hs_ty)
    TcRnTagToEnumMissingValArg
      -> mkSimpleDecorated $
           text "tagToEnum# must appear applied to one value argument"
    TcRnTagToEnumUnspecifiedResTy ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (vcat [ text "Specify the type by giving a type signature"
                      , text "e.g. (tagToEnum# x) :: Bool" ])
    TcRnTagToEnumResTyNotAnEnum ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (text "Result type must be an enumeration type")
    TcRnTagToEnumResTyTypeData ty
      -> mkSimpleDecorated $
           hang (text "Bad call to tagToEnum# at type" <+> ppr ty)
              2 (text "Result type cannot be headed by a `type data` type")
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> mkSimpleDecorated $
           text "Predicate type of `ifThenElse' depends on result type"
    TcRnIllegalHsBootOrSigDecl boot_or_sig decls
      -> mkSimpleDecorated $
           text "Illegal" <+> what <+> text "in" <+> whr <> dot
        where
          what = case decls of
            BootBindsPs      {} -> text "binding"
            BootBindsRn      {} -> text "binding"
            BootInstanceSigs {} -> text "instance body"
            BootFamInst      {} -> text "family instance"
            BootSpliceDecls  {} -> text "splice"
            BootForeignDecls {} -> text "foreign declaration"
            BootDefaultDecls {} -> text "default declaration"
            BootRuleDecls    {} -> text "RULE pragma"
          whr = case boot_or_sig of
            HsBoot -> text "an hs-boot file"
            Hsig   -> text "a backpack signature file"
    TcRnBootMismatch boot_or_sig err ->
      mkSimpleDecorated $ pprBootMismatch boot_or_sig err
    TcRnRecursivePatternSynonym binds
      -> mkSimpleDecorated $
            hang (text "Recursive pattern synonym definition with following bindings:")
               2 (vcat $ map pprLBind binds)
          where
            pprLoc loc = parens (text "defined at" <+> ppr loc)
            pprLBind :: CollectPass GhcRn => GenLocated (EpAnn a) (HsBindLR GhcRn idR) -> SDoc
            pprLBind (L loc bind) = pprWithCommas ppr (collectHsBindBinders CollNoDictBinders bind)
                                        <+> pprLoc (locA loc)
    TcRnPartialTypeSigTyVarMismatch n1 n2 fn_name hs_ty
      -> mkSimpleDecorated $
           hang (text "Couldn't match" <+> quotes (ppr n1)
                   <+> text "with" <+> quotes (ppr n2))
                2 (hang (text "both bound by the partial type signature:")
                        2 (ppr fn_name <+> dcolon <+> ppr hs_ty))
    TcRnPartialTypeSigBadQuantifier n fn_name m_unif_ty hs_ty
      -> mkSimpleDecorated $
           hang (text "Can't quantify over" <+> quotes (ppr n))
                2 (vcat [ hang (text "bound by the partial type signature:")
                             2 (ppr fn_name <+> dcolon <+> ppr hs_ty)
                        , extra ])
      where
        extra | Just rhs_ty <- m_unif_ty
              = sep [ quotes (ppr n), text "should really be", quotes (ppr rhs_ty) ]
              | otherwise
              = empty
    TcRnMissingSignature what _ ->
      mkSimpleDecorated $
      case what of
        MissingPatSynSig p ->
          hang (text "Pattern synonym with no type signature:")
            2 (text "pattern" <+> pprPrefixName (patSynName p) <+> dcolon <+> pprPatSynType p)
        MissingTopLevelBindingSig name ty ->
          hang (text "Top-level binding with no type signature:")
            2 (pprPrefixName name <+> dcolon <+> pprSigmaType ty)
        MissingTyConKindSig tc cusks_enabled ->
          hang msg
            2 (text "type" <+> pprPrefixName (tyConName tc) <+> dcolon <+> pprKind (tyConKind tc))
          where
            msg | cusks_enabled
                = text "Top-level type constructor with no standalone kind signature or CUSK:"
                | otherwise
                = text "Top-level type constructor with no standalone kind signature:"

    TcRnPolymorphicBinderMissingSig n ty
      -> mkSimpleDecorated $
           sep [ text "Polymorphic local binding with no type signature:"
               , nest 2 $ pprPrefixName n <+> dcolon <+> ppr ty ]
    TcRnOverloadedSig sig
      -> mkSimpleDecorated $
           hang (text "Overloaded signature conflicts with monomorphism restriction")
              2 (ppr sig)
    TcRnTupleConstraintInst _
      -> mkSimpleDecorated $ text "You can't specify an instance for a tuple constraint"
    TcRnUserTypeError ty
      -> mkSimpleDecorated (pprUserTypeErrorTy ty)
    TcRnConstraintInKind ty
      -> mkSimpleDecorated $
           text "Illegal constraint in a kind:" <+> pprType ty
    TcRnUnboxedTupleOrSumTypeFuncArg tuple_or_sum ty
      -> mkSimpleDecorated $
           sep [ text "Illegal unboxed" <+> what <+> text "type as function argument:"
               , pprType ty ]
        where
          what = case tuple_or_sum of
            UnboxedTupleType -> text "tuple"
            UnboxedSumType   -> text "sum"
    TcRnLinearFuncInKind ty
      -> mkSimpleDecorated $
           text "Illegal linear function in a kind:" <+> pprType ty
    TcRnForAllEscapeError ty kind
      -> mkSimpleDecorated $ vcat
           [ hang (text "Quantified type's kind mentions quantified type variable")
                2 (text "type:" <+> quotes (ppr ty))
           , hang (text "where the body of the forall has this kind:")
                2 (quotes (pprKind kind)) ]
    TcRnSimplifiableConstraint pred what
      -> mkSimpleDecorated $ vcat
           [ hang (text "The constraint" <+> quotes (pprType pred) <+> text "matches")
                2 (ppr what)
           , hang (text "This makes type inference for inner bindings fragile;")
                2 (text "either use MonoLocalBinds, or simplify it using the instance") ]
    TcRnArityMismatch thing thing_arity nb_args
      -> mkSimpleDecorated $
           hsep [ text "The" <+> what, quotes (ppr $ getName thing), text "should have"
                , n_arguments <> comma, text "but has been given"
                , if nb_args == 0 then text "none" else int nb_args
                ]
          where
            what = case thing of
              ATyCon tc -> ppr (tyConFlavour tc)
              _         -> text (tyThingCategory thing)
            n_arguments | thing_arity == 0 = text "no arguments"
                        | thing_arity == 1 = text "1 argument"
                        | True          = hsep [int thing_arity, text "arguments"]
    TcRnIllegalInstance reason ->
      mkSimpleDecorated $ pprIllegalInstance reason
    TcRnVDQInTermType mb_ty
      -> mkSimpleDecorated $
             case mb_ty of
               Nothing -> main_msg
               Just ty -> hang (main_msg <> char ':') 2 (pprType ty)
      where
        main_msg =
          text "Illegal visible, dependent quantification" <+>
          text "in the type of a term"
    TcRnBadQuantPredHead ty
      -> mkSimpleDecorated $
           hang (text "Quantified predicate must have a class or type variable head:")
              2 (pprType ty)
    TcRnIllegalTupleConstraint ty
      -> mkSimpleDecorated $
           text "Illegal tuple constraint:" <+> pprType ty
    TcRnNonTypeVarArgInConstraint ty
      -> mkSimpleDecorated $
           hang (text "Non type-variable argument")
              2 (text "in the constraint:" <+> pprType ty)
    TcRnIllegalImplicitParam ty
      -> mkSimpleDecorated $
           text "Illegal implicit parameter" <+> quotes (pprType ty)
    TcRnIllegalConstraintSynonymOfKind kind
      -> mkSimpleDecorated $
           text "Illegal constraint synonym of kind:" <+> quotes (pprKind kind)
    TcRnOversaturatedVisibleKindArg ty
      -> mkSimpleDecorated $
           text "Illegal oversaturated visible kind argument:" <+>
           quotes (char '@' <> pprParendType ty)
    TcRnForAllRankErr rank ty
      -> let herald = case tcSplitForAllTyVars ty of
               ([], _) -> text "Illegal qualified type:"
               _       -> text "Illegal polymorphic type:"
             extra = case rank of
               MonoTypeConstraint -> text "A constraint must be a monotype"
               _                  -> empty
         in mkSimpleDecorated $ vcat [hang herald 2 (pprType ty), extra]
    TcRnMonomorphicBindings bindings
      -> let pp_bndrs = pprBindings bindings
         in mkSimpleDecorated $
              sep [ text "The Monomorphism Restriction applies to the binding"
                  <> plural bindings
                  , text "for" <+> pp_bndrs ]
    TcRnOrphanInstance (Left cls_inst)
      -> mkSimpleDecorated $
           hang (text "Orphan class instance:")
              2 (pprInstanceHdr cls_inst)
    TcRnOrphanInstance (Right fam_inst)
      -> mkSimpleDecorated $
           hang (text "Orphan family instance:")
              2 (pprFamInst fam_inst)
    TcRnFunDepConflict unit_state sorted
      -> let herald = text "Functional dependencies conflict between instance declarations:"
         in mkSimpleDecorated $
              pprWithUnitState unit_state $ (hang herald 2 (pprInstances $ NE.toList sorted))
    TcRnDupInstanceDecls unit_state sorted
      -> let herald = text "Duplicate instance declarations:"
         in mkSimpleDecorated $
              pprWithUnitState unit_state $ (hang herald 2 (pprInstances $ NE.toList sorted))
    TcRnConflictingFamInstDecls sortedNE
      -> let sorted = NE.toList sortedNE
         in mkSimpleDecorated $
              hang (text "Conflicting family instance declarations:")
                 2 (vcat [ pprCoAxBranchUser (coAxiomTyCon ax) (coAxiomSingleBranch ax)
                         | fi <- sorted
                         , let ax = famInstAxiom fi ])
    TcRnFamInstNotInjective rea fam_tc (eqn1 NE.:| rest_eqns)
      -> let (herald, show_kinds) = case rea of
               InjErrRhsBareTyVar tys ->
                 (injectivityErrorHerald $$
                  text "RHS of injective type family equation is a bare" <+>
                  text "type variable" $$
                  text "but these LHS type and kind patterns are not bare" <+>
                  text "variables:" <+> pprQuotedList tys, False)
               InjErrRhsCannotBeATypeFam ->
                 (injectivityErrorHerald $$
                   text "RHS of injective type family equation cannot" <+>
                   text "be a type family:", False)
               InjErrRhsOverlap ->
                  (text "Type family equation right-hand sides overlap; this violates" $$
                   text "the family's injectivity annotation:", False)
               InjErrCannotInferFromRhs tvs has_kinds _ ->
                 let show_kinds = has_kinds == YesHasKinds
                     what = if show_kinds then text "Type/kind" else text "Type"
                     body = sep [ what <+> text "variable" <>
                                  pluralVarSet tvs <+> pprVarSet tvs (pprQuotedList . scopedSort)
                                , text "cannot be inferred from the right-hand side." ]
                     in (injectivityErrorHerald $$ body $$ text "In the type family equation:", show_kinds)

         in mkSimpleDecorated $ pprWithInvisibleBitsWhen show_kinds $
              hang herald
                2 (vcat (map (pprCoAxBranchUser fam_tc) (eqn1 : rest_eqns)))
    TcRnBangOnUnliftedType ty
      -> mkSimpleDecorated $
           text "Strictness flag has no effect on unlifted type" <+> quotes (ppr ty)
    TcRnLazyBangOnUnliftedType ty
      -> mkSimpleDecorated $
           text "Lazy flag has no effect on unlifted type" <+> quotes (ppr ty)
    TcRnMultipleDefaultDeclarations cls dup_things
      -> mkSimpleDecorated $
           hang (text "Multiple default declarations for class" <+> quotes (ppr cls))
              2 (vcat (map pp dup_things))
         where
           pp :: LDefaultDecl GhcRn -> SDoc
           pp (L locn DefaultDecl {})
             = text "here was another default declaration" <+> ppr (locA locn)
    TcRnBadDefaultType ty deflt_clss
      -> mkSimpleDecorated $
           hang (text "The default type" <+> quotes (ppr ty) <+> text "is not an instance of")
              2 (foldr1 (\a b -> a <+> text "or" <+> b) (NE.map (quotes. ppr) deflt_clss))
    TcRnPatSynBundledWithNonDataCon
      -> mkSimpleDecorated $
           text "Pattern synonyms can be bundled only with datatypes."
    TcRnPatSynBundledWithWrongType expected_res_ty res_ty
      -> mkSimpleDecorated $
           text "Pattern synonyms can only be bundled with matching type constructors"
               $$ text "Couldn't match expected type of"
               <+> quotes (ppr expected_res_ty)
               <+> text "with actual type of"
               <+> quotes (ppr res_ty)
    TcRnDupeModuleExport mod
      -> mkSimpleDecorated $
           hsep [ text "Duplicate"
                , quotes (text "Module" <+> ppr mod)
                , text "in export list" ]
    TcRnExportedModNotImported mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "is not imported"
    TcRnNullExportedModule mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "exports nothing"
    TcRnMissingExportList mod
      -> mkSimpleDecorated
       $ formatExportItemError
           (text "module" <+> ppr mod)
           "is missing an export list"
    TcRnExportHiddenComponents export_item
      -> mkSimpleDecorated
       $ formatExportItemError
           (ppr export_item)
           "attempts to export constructors or class methods that are not visible here"
    TcRnExportHiddenDefault export_item
      -> mkSimpleDecorated
       $ formatExportItemError
           (ppr export_item)
           "attempts to export a default class declaration that is not visible here"
    TcRnDuplicateExport gre ie1 ie2
      -> mkSimpleDecorated $
           hsep [ quotes (ppr $ greName gre)
                , text "is exported by", quotes (ppr ie1)
                , text "and",            quotes (ppr ie2) ]
    TcRnDuplicateNamedDefaultExport nm ie1 ie2
      -> mkSimpleDecorated $
           hsep [ text "The named default declaration for" <+> quotes (ppr nm)
                , text "is exported by", quotes (ppr ie1)
                , text "and",            quotes (ppr ie2) ]
    TcRnExportedParentChildMismatch parent_name ty_thing child parent_names
      -> mkSimpleDecorated $
           text "The type constructor" <+> quotes (ppr parent_name)
                 <+> text "is not the parent of the" <+> text what_is
                 <+> quotes thing <> char '.'
                 $$ text (capitalise what_is)
                    <> text "s can only be exported with their parent type constructor."
                 $$ (case parents of
                       [] -> empty
                       [_] -> text "Parent:"
                       _  -> text "Parents:") <+> fsep (punctuate comma parents)
      where
        pp_category :: TyThing -> String
        pp_category (AnId i)
          | isRecordSelector i = "record selector"
        pp_category i = tyThingCategory i
        what_is = pp_category ty_thing
        thing = ppr $ nameOccName child
        parents = map ppr parent_names
    TcRnConflictingExports occ child_gre1 ie1 child_gre2 ie2
      -> mkSimpleDecorated $
           vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
                , ppr_export child_gre1 ie1
                , ppr_export child_gre2 ie2
                ]
      where
        ppr_export gre ie =
          nest 3 $
            hang (quotes (ppr ie) <+> text "exports" <+> quotes (ppr $ greName gre))
               2 (pprNameProvenance gre)
    TcRnDuplicateFieldExport (gre, ie1) gres_ies ->
      mkSimpleDecorated $
           vcat ( hsep [ text "Duplicate record field"
                       , quotes (ppr $ greOccName gre)
                       , text "in export list" <> colon ]
                : map ppr_export ((gre,ie1) : NE.toList gres_ies)
                )
      where
        ppr_export (gre,ie) =
          nest 3 $
            hang (sep [ quotes (ppr ie) <+> text "exports the field" <+> quotes (ppr $ greName gre)
                       , text "belonging to the constructor" <> plural fld_cons <+> pprQuotedList fld_cons ])
               2 (pprNameProvenance gre)
          where
            fld_cons :: [ConLikeName]
            fld_cons = nonDetEltsUniqSet $ recFieldCons $ fieldGREInfo gre
    TcRnAmbiguousFieldInUpdate (gre1, gre2, gres)
      -> mkSimpleDecorated $
          vcat [ text "Ambiguous record field" <+> fld <> dot
               , hang (text "It could refer to any of the following:")
                  2 $ vcat (map pprSugg (gre1 : gre2 : gres))
               ]
        where
          fld = quotes $ ppr (occNameFS $ greOccName gre1)
          pprSugg gre = vcat [ bullet <+> pprGRE gre <> comma
                             , nest 2 (pprNameProvenance gre) ]
          pprGRE gre = case greInfo gre of
            IAmRecField {}
              | ParentIs { par_is = parent } <- greParent gre
              -> text "record field" <+> fld <+> text "of" <+> quotes (ppr parent)
            _ -> text "variable" <+> fld
    TcRnAmbiguousRecordUpdate _rupd tc
      -> mkSimpleDecorated $
          vcat [ text "Ambiguous record update with parent" <+> what <> dot
               , hsep [ text "This type-directed disambiguation mechanism"
                      , text "will not be supported by -XDuplicateRecordFields in future releases of GHC." ]
               , text "Consider disambiguating using module qualification instead."
               ]
        where
          what :: SDoc
          what = text "type constructor" <+> quotes (ppr $ RecSelData tc)
    TcRnMissingFields con fields
      -> mkSimpleDecorated $ vcat [header, nest 2 rest]
         where
           rest | null fields = empty
                | otherwise   = vcat (fmap pprField fields)
           header = text "Fields of" <+> quotes (ppr con) <+>
                    text "not initialised" <>
                    if null fields then empty else colon
    TcRnFieldUpdateInvalidType prs
      -> mkSimpleDecorated $
           hang (text "Record update for insufficiently polymorphic field"
                   <> plural prs <> colon)
              2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])
    TcRnMissingStrictFields con fields
      -> mkSimpleDecorated $ vcat [header, nest 2 rest]
         where
           rest | null fields = empty  -- Happens for non-record constructors
                                       -- with strict fields
                | otherwise   = vcat (fmap pprField fields)

           header = text "Constructor" <+> quotes (ppr con) <+>
                    text "does not have the required strict field(s)" <>
                    if null fields then empty else colon
    TcRnBadRecordUpdate upd_flds reason
      -> case reason of
          NoConstructorHasAllFields { conflictingFields = conflicts }
            | [fld] <- conflicts
            -> mkSimpleDecorated $
                vcat [ header
                     , text "No constructor in scope has the field" <+> quotes (ppr fld) ]
            | otherwise
            ->
              mkSimpleDecorated $
                vcat [ header
                     , hang (text "No constructor in scope has all of the following fields:")
                        2 (pprQuotedList conflicts) ]
            where
              header :: SDoc
              header = text "Invalid record update."
          MultiplePossibleParents (par1, par2, pars) ->
            mkSimpleDecorated $
              vcat [ hang (text "Ambiguous record update with field" <> plural upd_flds)
                       2 ppr_flds
                   , hang (thisOrThese upd_flds <+> text "field" <> plural upd_flds <+> what_parent)
                       2 (quotedListWithAnd (map ppr (par1:par2:pars))) ]
            where
              ppr_flds, what_parent, which :: SDoc
              ppr_flds = quotedListWithAnd $ map ppr upd_flds
              what_parent = case par1 of
                RecSelData   {} -> text "appear" <> singular upd_flds
                                <+> text "in" <+> which <+> text "datatypes"
                RecSelPatSyn {} -> isOrAre upd_flds <+> text "associated with"
                                <+> which <+> text "pattern synonyms"
              which = case pars of
                [] -> text "both"
                _  -> text "all of the"
          InvalidTyConParent tc pars ->
            mkSimpleDecorated $
              vcat [ hang (text "No data constructor of" <+> what $$ text "has all of the fields:")
                      2 (pprQuotedList upd_flds)
                   , pat_syn_msg ]
            where
              what = text "type constructor" <+> quotes (ppr (RecSelData tc))
              pat_syn_msg
                | any (\case { RecSelPatSyn {} -> True; _ -> False}) pars
                = note "Type-directed disambiguation is not supported for pattern synonym record fields"
                | otherwise
                = empty
    TcRnStaticFormNotClosed name reason
      -> mkSimpleDecorated $
           quotes (ppr name)
             <+> text "is used in a static form but it is not closed"
             <+> text "because it"
             $$ sep (causes reason)
         where
          causes :: NotClosedReason -> [SDoc]
          causes NotLetBoundReason = [text "is not let-bound."]
          causes (NotTypeClosed vs) =
            [ text "has a non-closed type because it contains the"
            , text "type variables:" <+>
              pprVarSet vs (hsep . punctuate comma . map (quotes . ppr))
            ]
          causes (NotClosed n reason) =
            let msg = text "uses" <+> quotes (ppr n) <+> text "which"
             in case reason of
                  NotClosed _ _ -> msg : causes reason
                  _   -> let (xs0, xs1) = splitAt 1 $ causes reason
                          in fmap (msg <+>) xs0 ++ xs1
    TcRnUselessTypeable
      -> mkSimpleDecorated $
           text "Deriving" <+> quotes (ppr typeableClassName) <+>
           text "has no effect: all types now auto-derive Typeable"
    TcRnDerivingDefaults cls
      -> mkSimpleDecorated $ sep
                     [ text "Both DeriveAnyClass and"
                       <+> text "GeneralizedNewtypeDeriving are enabled"
                     , text "Defaulting to the DeriveAnyClass strategy"
                       <+> text "for instantiating" <+> ppr cls
                     ]
    TcRnNonUnaryTypeclassConstraint ctxt ct
      -> mkSimpleDecorated $
           quotes (ppr ct)
           <+> text "is not a unary constraint, as expected by"
           <+> pprUserTypeCtxt ctxt
    TcRnPartialTypeSignatures _ theta
      -> mkSimpleDecorated $
           text "Found type wildcard" <+> quotes (char '_')
                       <+> text "standing for" <+> quotes (pprTheta theta)
    TcRnCannotDeriveInstance cls cls_tys mb_strat newtype_deriving reason
      -> mkSimpleDecorated $
           derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving True reason
    TcRnLookupInstance cls tys reason
      -> mkSimpleDecorated $
          text "Couldn't match instance:" <+>
           lookupInstanceErrDiagnosticMessage cls tys reason
    TcRnLazyGADTPattern
      -> mkSimpleDecorated $
           hang (text "An existential or GADT data constructor cannot be used")
              2 (text "inside a lazy (~) pattern")
    TcRnArrowProcGADTPattern
      -> mkSimpleDecorated $
           text "Proc patterns cannot use existential or GADT data constructors"
    TcRnTypeEqualityOutOfScope
      -> mkDecorated
           [ text "The" <+> quotes (text "~") <+> text "operator is out of scope." $$
             text "Assuming it to stand for an equality constraint."
           , note $ quotes "~" <+> "used to be built-in syntax but now is a regular type operator" $$
                      "exported from Data.Type.Equality and Prelude." $$
                      "If you are using a custom Prelude, consider re-exporting it"
           , text "This will become an error in a future GHC release." ]
    TcRnTypeEqualityRequiresOperators
      -> mkSimpleDecorated $
            fsep [ text "The use of" <+> quotes (text "~")
                                     <+> text "without TypeOperators",
                   text "will become an error in a future GHC release." ]
    TcRnIllegalTypeOperator overall_ty op
      -> mkSimpleDecorated $
           text "Illegal operator" <+> quotes (ppr op) <+>
           text "in type" <+> quotes (ppr overall_ty)
    TcRnIllegalTypeOperatorDecl name
      -> mkSimpleDecorated $
        text "Illegal declaration of a type or class operator" <+> quotes (ppr name)
    TcRnGADTMonoLocalBinds
      -> mkSimpleDecorated $
            fsep [ text "Pattern matching on GADTs without MonoLocalBinds"
                 , text "is fragile." ]
    TcRnIncorrectNameSpace name _in_th_tick
      -> mkSimpleDecorated $
           text "The" <+> what <+> text "does not live in" <+> other_ns
        where
          -- the other (opposite) namespace
          other_ns | isValNameSpace ns = text "the type-level namespace"
                   | otherwise         = text "the term-level namespace"
          ns = nameNameSpace name
          what = pprNameSpace ns <+> quotes (ppr name)
    TcRnNotInScope err name
      -> mkSimpleDecorated $ pprScopeError name err
    TcRnTermNameInType name
      -> mkSimpleDecorated $
           quotes (ppr name) <+>
             (text "is a term-level binding") $+$
             (text " and can not be used at the type level.")
    TcRnUntickedPromotedThing thing
      -> mkSimpleDecorated $
         text "Unticked promoted" <+> what
           where
             what :: SDoc
             what = case thing of
               UntickedExplicitList -> text "list" <> dot
               UntickedConstructor fixity nm ->
                 let con      = pprUntickedConstructor fixity nm
                     bare_sym = isBareSymbol fixity nm
                 in text "constructor:" <+> con <> if bare_sym then empty else dot
    TcRnIllegalBuiltinSyntax sig rdr_name
      -> mkSimpleDecorated $
           hsep [text "Illegal", pprSigLike sig, text "of built-in syntax:", ppr rdr_name]
    TcRnWarnDefaulting tidy_wanteds tidy_tv default_ty
      -> mkSimpleDecorated $
           hang (hsep $ [ text "Defaulting" ]
                     ++
                     (case tidy_tv of
                         Nothing -> []
                         Just tv -> [text "the type variable"
                                    , quotes (ppr tv)])
                     ++
                     [ text "to type"
                     , quotes (ppr default_ty)
                     , text "in the following constraint" <> plural tidy_wanteds ])
             2
             (pprWithArising tidy_wanteds)
    TcRnWarnClashingDefaultImports cls Nothing imports
      -> mkSimpleDecorated $
           hang (text "Clashing imported defaults for class" <+> quotes (ppr cls) <> colon)
              2 (vcat $ defaultTypesAndImport <$> NE.toList imports)
    TcRnWarnClashingDefaultImports cls (Just local) imports
      -> mkSimpleDecorated $
           sep [ hang (text "Imported defaults for class" <+> quotes (ppr cls) <> colon)
                    2 (vcat $ defaultTypesAndImport <$> NE.toList imports)
               , hang (text "are not subsumed by the local `default` declaration")
                    2 (parens $ pprWithCommas ppr local)
               ]

    TcRnForeignImportPrimExtNotSet _decl
      -> mkSimpleDecorated $
           text "`foreign import prim' requires GHCForeignImportPrim."

    TcRnForeignImportPrimSafeAnn _decl
      -> mkSimpleDecorated $
           text "The safe/unsafe annotation should not be used with `foreign import prim'."

    TcRnForeignFunctionImportAsValue _decl
      -> mkSimpleDecorated $
           text "`value' imports cannot have function types"

    TcRnFunPtrImportWithoutAmpersand _decl
      -> mkSimpleDecorated $
           text "possible missing & in foreign import of FunPtr"

    TcRnIllegalForeignDeclBackend _decl _backend expectedBknds
      -> mkSimpleDecorated $
         fsep (text "Illegal foreign declaration: requires one of these back ends:" :
               commafyWith (text "or") (map (text . backendDescription) expectedBknds))

    TcRnUnsupportedCallConv _decl unsupportedCC
      -> mkSimpleDecorated $
           case unsupportedCC of
             StdCallConvUnsupported ->
               text "the 'stdcall' calling convention is unsupported on this platform,"
               $$ text "treating as ccall"
             PrimCallConvUnsupported ->
               text "The `prim' calling convention can only be used with `foreign import'"
             JavaScriptCallConvUnsupported ->
               text "The `javascript' calling convention is unsupported on this platform"

    TcRnIllegalForeignType mArgOrResult reason
      -> mkSimpleDecorated $ hang msg 2 extra
      where
        arg_or_res = case mArgOrResult of
          Nothing -> empty
          Just Arg -> text "argument"
          Just Result -> text "result"
        msg = hsep [ text "Unacceptable", arg_or_res
                   , text "type in foreign declaration:"]
        extra =
          case reason of
            TypeCannotBeMarshaled ty why ->
              let innerMsg = quotes (ppr ty) <+> text "cannot be marshalled in a foreign call"
               in case why of
                NotADataType ->
                  quotes (ppr ty) <+> text "is not a data type"
                NewtypeDataConNotInScope _ [] ->
                  hang innerMsg 2 $ text "because its data constructor is not in scope"
                NewtypeDataConNotInScope tc _ ->
                  hang innerMsg 2 $
                    text "because the data constructor for"
                    <+> quotes (ppr tc) <+> text "is not in scope"
                UnliftedFFITypesNeeded ->
                  innerMsg $$ text "UnliftedFFITypes is required to marshal unlifted types"
                NotABoxedMarshalableTyCon -> innerMsg
                ForeignLabelNotAPtr ->
                  innerMsg $$ text "A foreign-imported address (via &foo) must have type (Ptr a) or (FunPtr a)"
                NotSimpleUnliftedType ->
                  innerMsg $$ text "foreign import prim only accepts simple unlifted types"
                NotBoxedKindAny ->
                  text "Expected kind" <+> quotes (text "Type") <+> text "or" <+> quotes (text "UnliftedType") <> comma $$
                  text "but" <+> quotes (ppr ty) <+> text "has kind" <+> quotes (ppr (typeKind ty))
            ForeignDynNotPtr expected ty ->
              vcat [ text "Expected: Ptr/FunPtr" <+> pprParendType expected <> comma, text "  Actual:" <+> ppr ty ]
            SafeHaskellMustBeInIO ->
              text "Safe Haskell is on, all FFI imports must be in the IO monad"
            IOResultExpected ->
              text "IO result type expected"
            UnexpectedNestedForall ->
              text "Unexpected nested forall"
            LinearTypesNotAllowed ->
              text "Linear types are not supported in FFI declarations, see #18472"
            OneArgExpected ->
              text "One argument expected"
            AtLeastOneArgExpected ->
              text "At least one argument expected"
    TcRnInvalidCIdentifier target
      -> mkSimpleDecorated $
           sep [quotes (ppr target) <+> text "is not a valid C identifier"]
    TcRnExpectedValueId thing
      -> mkSimpleDecorated $
           ppr thing <+> text "used where a value identifier was expected"
    TcRnRecSelectorEscapedTyVar lbl
      -> mkSimpleDecorated $
           text "Cannot use record selector" <+> quotes (ppr lbl) <+>
           text "as a function due to escaped type variables"
    TcRnPatSynNotBidirectional name
      -> mkSimpleDecorated $
           text "non-bidirectional pattern synonym"
           <+> quotes (ppr name) <+> text "used in an expression"
    TcRnIllegalDerivingItem hs_ty
      -> mkSimpleDecorated $
           text "Illegal deriving item" <+> quotes (ppr hs_ty)
    TcRnIllegalDefaultClass nm
      -> mkSimpleDecorated $
           text "Illegal named default declaration for non-class" <+> quotes (ppr nm)
    TcRnIllegalNamedDefault hs_decl
      -> mkSimpleDecorated $ text "Illegal named default declaration" <+> quotes (ppr hs_decl)
    TcRnUnexpectedAnnotation ty bang
      -> mkSimpleDecorated $
           let err = case bang of
                 HsSrcBang _ SrcUnpack   _       -> "UNPACK"
                 HsSrcBang _ SrcNoUnpack _       -> "NOUNPACK"
                 HsSrcBang _ NoSrcUnpack SrcLazy -> "laziness (~)"
                 HsSrcBang _ _           _       -> "strictness (!)"
            in text "Unexpected" <+> text err <+> text "annotation:" <+> ppr ty $$
               text err <+> text "annotation can only appear on the arguments of a data constructor type"
    TcRnIllegalRecordSyntax ty
      -> mkSimpleDecorated $
           text "Record syntax is illegal here:" <+> ppr ty

    TcRnInvalidVisibleKindArgument arg ty
      -> mkSimpleDecorated $
           text "Cannot apply function of kind" <+> quotes (ppr ty)
             $$ text "to visible kind argument" <+> quotes (ppr arg)
    TcRnTooManyBinders ki bndrs
      -> mkSimpleDecorated $
           hang (text "Not a function kind:")
              4 (ppr ki) $$
           hang (text "but extra binders found:")
              4 (fsep (map ppr bndrs))
    TcRnDifferentNamesForTyVar n1 n2
      -> mkSimpleDecorated $
           hang (text "Different names for the same type variable:") 2 info
         where
           info | nameOccName n1 /= nameOccName n2
                = quotes (ppr n1) <+> text "and" <+> quotes (ppr n2)
                | otherwise -- Same OccNames! See C2 in
                            -- Note [Swizzling the tyvars before generaliseTcTyCon]
                = vcat [ quotes (ppr n1) <+> text "bound at" <+> ppr (getSrcLoc n1)
                       , quotes (ppr n2) <+> text "bound at" <+> ppr (getSrcLoc n2) ]

    TcRnDisconnectedTyVar n
      -> mkSimpleDecorated $
           hang (text "Scoped type variable only appears non-injectively in declaration header:")
              2 (quotes (ppr n) <+> text "bound at" <+> ppr (getSrcLoc n))

    TcRnInvalidReturnKind data_sort allowed_kind kind _suggested_ext
      -> mkSimpleDecorated $
           sep [ ppDataSort data_sort <+>
                 text "has non-" <>
                 allowed_kind_tycon
               , (if is_data_family then text "and non-variable" else empty) <+>
                 text "return kind" <+> quotes (ppr kind)
               ]
         where
          is_data_family =
            case data_sort of
              DataDeclSort{}     -> False
              DataInstanceSort{} -> False
              DataFamilySort     -> True
          allowed_kind_tycon =
            case allowed_kind of
              AnyTYPEKind  -> ppr tYPETyCon
              AnyBoxedKind -> ppr boxedRepDataConTyCon
              LiftedKind   -> ppr liftedTypeKind
    TcRnClassKindNotConstraint _kind
      -> mkSimpleDecorated $
           text "Kind signature on a class must end with" <+> ppr constraintKind $$
           text "unobscured by type families"
    TcRnUnpromotableThing name err
      -> mkSimpleDecorated $
           (hang (pprPECategory err <+> quotes (ppr name) <+> text "cannot be used here")
                        2 (parens reason))
        where
          reason = case err of
                     ConstrainedDataConPE theta
                                    -> text "it has an unpromotable context"
                                       <+> quotes (pprTheta theta)

                     FamDataConPE   -> text "it comes from a data family instance"
                     PatSynPE       -> text "pattern synonyms cannot be promoted"
                     RecDataConPE   -> same_rec_group_msg
                     ClassPE        -> same_rec_group_msg
                     TyConPE        -> same_rec_group_msg
                     TermVariablePE -> text "term variables cannot be promoted"
                     TypeVariablePE -> text "type variables bound in a kind signature cannot be used in the type"
          same_rec_group_msg = text "it is defined and used in the same recursive group"
    TcRnIllegalTermLevelUse rdr name err
      -> mkSimpleDecorated $
            text "Illegal term-level use of the" <+>
              text (teCategory err) <+> quotes (ppr qnm)
          where
            qnm = WithUserRdr rdr name
    TcRnMatchesHaveDiffNumArgs argsContext (MatchArgMatches match1 bad_matches)
      -> mkSimpleDecorated $
           (vcat [ pprMatchContextNouns argsContext <+>
                   text "have different numbers of arguments"
                 , nest 2 (ppr (getLocA match1))
                 , nest 2 (ppr (getLocA (NE.head bad_matches)))])
    TcRnCannotBindScopedTyVarInPatSig sig_tvs
      -> mkSimpleDecorated $
           hang (text "You cannot bind scoped type variable"
                  <> plural (NE.toList sig_tvs)
                 <+> pprQuotedList (map fst $ NE.toList sig_tvs))
              2 (text "in a pattern binding signature")
    TcRnCannotBindTyVarsInPatBind _offenders
      -> mkSimpleDecorated $
           text "Binding type variables is not allowed in pattern bindings"
    TcRnTooManyTyArgsInConPattern con_like expected_number actual_number
      -> mkSimpleDecorated $
           text "Too many type arguments in constructor pattern for" <+> quotes (ppr con_like) $$
           text "Expected no more than" <+> ppr expected_number <> semi <+> text "got" <+> ppr actual_number
    TcRnMultipleInlinePragmas poly_id fst_inl_prag inl_prags
      -> mkSimpleDecorated $
           hang (text "Multiple INLINE pragmas for" <+> ppr poly_id)
             2 (vcat (text "Ignoring all but the first"
                      : map pp_inl (fst_inl_prag : NE.toList inl_prags)))
         where
           pp_inl (L loc prag) = ppr prag <+> parens (ppr loc)
    TcRnUnexpectedPragmas poly_id bad_sigs
      -> mkSimpleDecorated $
           hang (text "Discarding unexpected pragmas for" <+> ppr poly_id)
              2 (vcat (map (ppr . getLoc) $ NE.toList bad_sigs))
    TcRnNonOverloadedSpecialisePragma fun_name
       -> mkSimpleDecorated $
            text "SPECIALISE pragma for non-overloaded function"
              <+> quotes (ppr fun_name)
    TcRnSpecialiseNotVisible name
      -> mkSimpleDecorated $
         text "You cannot SPECIALISE" <+> quotes (ppr name)
           <+> text "because its definition is not visible in this module"
    TcRnPragmaWarning
      { pragma_warning_info = PragmaWarningInstance{pwarn_dfunid, pwarn_ctorig}
      , pragma_warning_msg }
      -> mkSimpleDecorated $
        sep [ hang (text "In the use of")
                 2 (pprDFunId pwarn_dfunid)
            , ppr pwarn_ctorig
            , pprWarningTxtForMsg pragma_warning_msg
         ]
    TcRnPragmaWarning
      { pragma_warning_info = PragmaWarningDefault{pwarn_class, pwarn_impmod}
      , pragma_warning_msg }
      -> mkSimpleDecorated $
        sep [ sep [ text "In the use of class"
                    <+> ppr pwarn_class
                    <+> text "defaults imported from"
                    <+> ppr pwarn_impmod <> colon ]
            , pprWarningTxtForMsg pragma_warning_msg
         ]
    TcRnPragmaWarning {pragma_warning_info, pragma_warning_msg}
      -> mkSimpleDecorated $
        sep [ sep [ text "In the use of"
                <+> pprNonVarNameSpace (occNameSpace occ_name)
                <+> quotes (ppr occ_name)
                , parens imp_msg <> colon ]
          , pprWarningTxtForMsg pragma_warning_msg ]
          where
            occ_name = pwarn_occname pragma_warning_info
            imp_mod = pwarn_impmod pragma_warning_info
            imp_msg  = text "imported from" <+> ppr imp_mod <> extra
            extra | PragmaWarningName {pwarn_declmod = decl_mod} <- pragma_warning_info
                  , imp_mod /= decl_mod = text ", but defined in" <+> ppr decl_mod
                  | otherwise = empty
    TcRnDifferentExportWarnings name locs
      -> mkSimpleDecorated $ vcat [quotes (ppr name) <+> text "exported with different error messages",
                                   text "at" <+> vcat (map ppr $ sortBy leftmost_smallest $ NE.toList locs)]
    TcRnIncompleteExportWarnings name locs
      -> mkSimpleDecorated $ vcat [quotes (ppr name) <+> text "will not have its export warned about",
                                   text "missing export warning at" <+> vcat (map ppr $ sortBy leftmost_smallest $ NE.toList locs)]
    TcRnIllegalHsigDefaultMethods name meths
      -> mkSimpleDecorated $
        text "Illegal default method" <> plural (NE.toList meths) <+> text "in class definition of" <+> ppr name <+> text "in hsig file"
    TcRnHsigFixityMismatch real_thing real_fixity sig_fixity
      ->
      let ppr_fix f = ppr f <+> if f == defaultFixity then parens (text "default") else empty
      in mkSimpleDecorated $
        vcat [ppr real_thing <+> text "has conflicting fixities in the module",
              text "and its hsig file",
              text "Main module:" <+> ppr_fix real_fixity,
              text "Hsig file:" <+> ppr_fix sig_fixity]
    TcRnHsigShapeMismatch (HsigShapeSortMismatch info1 info2)
      -> mkSimpleDecorated $
            text "While merging export lists, could not combine"
            <+> ppr info1 <+> text "with" <+> ppr info2
            <+> parens (text "one is a type, the other is a plain identifier")
    TcRnHsigShapeMismatch (HsigShapeNotUnifiable name1 name2 notHere)
      ->
      let extra = if notHere
                  then text "Neither name variable originates from the current signature."
                  else empty
      in mkSimpleDecorated $
        text "While merging export lists, could not unify"
        <+> ppr name1 <+> text "with" <+> ppr name2 $$ extra
    TcRnHsigMissingModuleExport occ unit_state impl_mod
      -> mkSimpleDecorated $
            quotes (ppr occ)
        <+> text "is exported by the hsig file, but not exported by the implementing module"
        <+> quotes (pprWithUnitState unit_state $ ppr impl_mod)
    TcRnBadGenericMethod clas op
      -> mkSimpleDecorated $
        hsep [text "Class", quotes (ppr clas),
          text "has a generic-default signature without a binding", quotes (ppr op)]
    TcRnWarningMinimalDefIncomplete mindef
      -> mkSimpleDecorated $
        vcat [ text "The MINIMAL pragma does not require:"
          , nest 2 (pprBooleanFormulaNice mindef)
          , text "but there is no default implementation." ]
    TcRnDefaultMethodForPragmaLacksBinding sel_id prag
      -> mkSimpleDecorated $
        text "The" <+> hsSigDoc prag <+> text "for default method"
          <+> quotes (ppr sel_id)
          <+> text "lacks an accompanying binding"
    TcRnIgnoreSpecialisePragmaOnDefMethod sel_name
      -> mkSimpleDecorated $
        text "Ignoring SPECIALISE pragmas on default method"
          <+> quotes (ppr sel_name)
    TcRnBadMethodErr{badMethodErrClassName, badMethodErrMethodName}
      -> mkSimpleDecorated $
        hsep [text "Class", quotes (ppr badMethodErrClassName),
          text "does not have a method", quotes (ppr badMethodErrMethodName)]
    TcRnIllegalTypeData
      -> mkSimpleDecorated $
        text "Illegal type-level data declaration"
    TcRnTypeDataForbids feature
      -> mkSimpleDecorated $
        ppr feature <+> text "are not allowed in type data declarations."

    TcRnIllegalNewtype con show_linear_types reason
      -> mkSimpleDecorated $
        vcat [msg, additional]
        where
          (msg,additional) =
            case reason of
              DoesNotHaveSingleField n_flds ->
                (sep [
                  text "A newtype constructor must have exactly one field",
                  nest 2 $
                    text "but" <+> quotes (ppr con) <+> text "has" <+> speakN n_flds
                ],
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              IsNonLinear ->
                (text "A newtype constructor must be linear",
                ppr con <+> dcolon <+> ppr (dataConDisplayType True con))
              IsGADT ->
                (text "A newtype must not be a GADT",
                ppr con <+> dcolon <+> pprWithInvisibleBitsWhen sneaky_eq_spec
                                       (ppr $ dataConDisplayType show_linear_types con))
              HasConstructorContext ->
                (text "A newtype constructor must not have a context in its type",
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              HasExistentialTyVar ->
                (text "A newtype constructor must not have existential type variables",
                ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con))
              HasStrictnessAnnotation ->
                (text "A newtype constructor must not have a strictness annotation", empty)

          -- Is the data con a "covert" GADT?  See Note [isCovertGadtDataCon]
          -- in GHC.Core.DataCon
          sneaky_eq_spec = isCovertGadtDataCon con
    TcRnOrPatBindsVariables bndrs -> mkSimpleDecorated $
      text "An or-pattern may not bind term or type variables such as"
        <+> quotedListWithOr (map ppr (NE.toList bndrs))
    TcRnUnsatisfiedMinimalDef mindef
      -> mkSimpleDecorated $
        vcat [text "No explicit implementation for"
              ,nest 2 $ pprBooleanFormulaNice mindef
             ]
    TcRnMisplacedInstSig name hs_ty
      -> mkSimpleDecorated $
        vcat [ hang (text "Illegal type signature in instance declaration:")
                  2 (hang (pprPrefixName name)
                        2 (dcolon <+> ppr hs_ty))
             ]
    TcRnNoRebindableSyntaxRecordDot -> mkSimpleDecorated $
      text "RebindableSyntax is required if OverloadedRecordUpdate is enabled."
    TcRnNoFieldPunsRecordDot -> mkSimpleDecorated $
      text "For this to work enable NamedFieldPuns"
    TcRnIllegalStaticExpression e -> mkSimpleDecorated $
        text "Illegal static expression:" <+> ppr e
    TcRnListComprehensionDuplicateBinding n -> mkSimpleDecorated $
        (text "Duplicate binding in parallel list comprehension for:"
          <+> quotes (ppr n))
    TcRnEmptyStmtsGroup cause -> mkSimpleDecorated  $ case cause of
      EmptyStmtsGroupInParallelComp ->
        text "Empty statement group in parallel comprehension"
      EmptyStmtsGroupInTransformListComp ->
        text "Empty statement group preceding 'group' or 'then'"
      EmptyStmtsGroupInDoNotation ctxt ->
        text "Empty" <+> pprHsDoFlavour ctxt
      EmptyStmtsGroupInArrowNotation ->
        text "Empty 'do' block in an arrow command"
    TcRnLastStmtNotExpr ctxt (UnexpectedStatement stmt) ->
      mkSimpleDecorated $ hang last_error 2 (ppr stmt)
      where
        last_error =
          text "The last statement in" <+> pprAStmtContext ctxt
          <+> text "must be an expression"
    TcRnUnexpectedStatementInContext ctxt (UnexpectedStatement stmt) _ -> mkSimpleDecorated $
       sep [ text "Unexpected" <+> pprStmtCat stmt <+> text "statement"
                       , text "in" <+> pprAStmtContext ctxt ]
    TcRnIllegalTupleSection -> mkSimpleDecorated $
      text "Illegal tuple section"
    TcRnIllegalImplicitParameterBindings eBinds -> mkSimpleDecorated $
        either msg msg eBinds
      where
        msg binds = hang
          (text "Implicit-parameter bindings illegal in an mdo expression")
          2 (ppr binds)
    TcRnSectionWithoutParentheses expr -> mkSimpleDecorated $
      hang (text "A section must be enclosed in parentheses")
         2 (text "thus:" <+> (parens (ppr expr)))
    TcRnMissingRoleAnnotation name roles -> mkSimpleDecorated $
      hang (text "Missing role annotation" <> colon)
         2 (text "type role" <+> ppr name <+> hsep (map ppr roles))

    TcRnIllformedTypePattern p
      -> mkSimpleDecorated $
          hang (text "Ill-formed type pattern:") 2 (ppr p)
    TcRnIllegalTypePattern
      -> mkSimpleDecorated $
          text "Illegal type pattern." $$
          text "A type pattern must be checked against a visible forall."
    TcRnIllformedTypeArgument e
      -> mkSimpleDecorated $
          hang (text "Ill-formed type argument:") 2 (ppr e)
    TcRnIllegalTypeExpr syntax -> mkSimpleDecorated $
      vcat [ text "Illegal" <+> pprTypeSyntaxName syntax
           , text "Type syntax may only be used in a required type argument,"
           , text "i.e. to instantiate a visible forall." ]

    TcRnCapturedTermName tv_name shadowed_term_names
      -> mkSimpleDecorated $
        text "The type variable" <+> quotes (ppr tv_name) <+>
          text "is implicitly quantified," $+$
          text "even though another variable of the same name is in scope:" $+$
          nest 2 var_names $+$
          text "This is not compatible with the RequiredTypeArguments extension."
        where
          var_names = case shadowed_term_names of
              Left gbl_names -> vcat (map (\name -> quotes (ppr $ greName name) <+> pprNameProvenance name) gbl_names)
              Right lcl_name -> quotes (ppr lcl_name) <+> text "defined at"
                <+> ppr (nameSrcLoc lcl_name)
    TcRnBindingOfExistingName name -> mkSimpleDecorated $
      text "Illegal binding of an existing name:" <+> ppr name
    TcRnMultipleFixityDecls loc rdr_name -> mkSimpleDecorated $
      vcat [text "Multiple fixity declarations for" <+> quotes (ppr rdr_name),
            text "also at " <+> ppr loc]
    TcRnIllegalPatternSynonymDecl -> mkSimpleDecorated $
      text "Illegal pattern synonym declaration"
    TcRnIllegalClassBinding dsort bind -> mkSimpleDecorated $
      vcat [ what <+> text "not allowed in" <+> decl_sort
              , nest 2 (ppr bind) ]
      where
        decl_sort = case dsort of
          ClassDeclSort -> text "class declaration:"
          InstanceDeclSort -> text "instance declaration:"
        what = case bind of
                  PatBind {}    -> text "Pattern bindings (except simple variables)"
                  PatSynBind {} -> text "Pattern synonyms"
                                   -- Associated pattern synonyms are not implemented yet
                  _ -> pprPanic "rnMethodBind" (ppr bind)

    TcRnOrphanCompletePragma -> mkSimpleDecorated $
      text "Orphan COMPLETE pragmas not supported" $$
      text "A COMPLETE pragma must mention at least one data constructor" $$
      text "or pattern synonym defined in the same module."
    TcRnEmptyCase ctxt reason -> mkSimpleDecorated $
      case reason of
        EmptyCaseWithoutFlag ->
          text "Empty list of alternatives in" <+> pp_ctxt
        EmptyCaseDisallowedCtxt ->
          text "Empty list of alternatives is not allowed in" <+> pp_ctxt
        EmptyCaseForall tvb ->
          vcat [ text "Empty list of alternatives in" <+> pp_ctxt
               , hang (text "checked against a forall-type:")
                      2 (pprForAll [tvb] <+> text "...")
               ]
        where
          pp_ctxt = case ctxt of
            CaseAlt                                -> text "case expression"
            LamAlt LamCase                         -> text "\\case expression"
            LamAlt LamCases                        -> text "\\cases expression"
            ArrowMatchCtxt (ArrowLamAlt LamSingle) -> text "kappa abstraction"
            ArrowMatchCtxt (ArrowLamAlt LamCase)   -> text "\\case command"
            ArrowMatchCtxt (ArrowLamAlt LamCases)  -> text "\\cases command"
            ArrowMatchCtxt ArrowCaseAlt            -> text "case command"
            ctxt                                   -> text "(unexpected)" <+> pprMatchContextNoun ctxt
    TcRnNonStdGuards (NonStandardGuards guards) -> mkSimpleDecorated $
      text "accepting non-standard pattern guards" $$
      nest 4 (interpp'SP guards)
    TcRnDuplicateSigDecl pairs@((L _ name, sig) :| _) -> mkSimpleDecorated $
      vcat [ text "Duplicate" <+> what_it_is
            <> text "s for" <+> quotes (ppr name)
          , text "at" <+> vcat (map ppr $ sortBy leftmost_smallest
                                        $ map (getLocA . fst)
                                        $ NE.toList pairs)
          ]
      where
        what_it_is = hsSigDoc sig
    TcRnMisplacedSigDecl sig -> mkSimpleDecorated $
      sep [text "Misplaced" <+> hsSigDoc sig <> colon, ppr sig]
    TcRnUnexpectedDefaultSig sig -> mkSimpleDecorated $
      hang (text "Unexpected default signature:")
         2 (ppr sig)
    TcRnDuplicateMinimalSig sig1 sig2 otherSigs -> mkSimpleDecorated $
      vcat [ text "Multiple minimal complete definitions"
           , text "at" <+> vcat (map ppr $ sortBy leftmost_smallest $ map getLocA sigs)
           , text "Combine alternative minimal complete definitions with `|'" ]
      where
        sigs = sig1 : sig2 : otherSigs
    TcRnSpecSigShape spec_e -> mkSimpleDecorated $
      hang (text "Illegal form of SPECIALISE pragma:")
         2 (ppr spec_e)
    TcRnUnexpectedStandaloneDerivingDecl -> mkSimpleDecorated $
      text "Illegal standalone deriving declaration"
    TcRnUnusedVariableInRuleDecl name var -> mkSimpleDecorated $
      sep [text "Rule" <+> doubleQuotes (ftext name) <> colon,
          text "Forall'd variable" <+> quotes (ppr var) <+>
                  text "does not appear on left hand side"]
    TcRnUnexpectedStandaloneKindSig -> mkSimpleDecorated $
      text "Illegal standalone kind signature"
    TcRnIllegalRuleLhs errReason name lhs bad_e -> mkSimpleDecorated $
      sep [text "Rule" <+> pprRuleName name <> colon,
           nest 2 (vcat [err,
                         text "in left-hand side:" <+> ppr lhs])]
      $$
      text "LHS must be of form (f e1 .. en) where f is not forall'd"
      where
        err = case errReason of
          UnboundVariable uv nis -> pprScopeError uv nis
          IllegalExpression -> text "Illegal expression:" <+> ppr bad_e
    TcRnRuleLhsEqualities ruleName _lhs cts -> mkSimpleDecorated $
      text "Discarding RULE" <+> doubleQuotes (ftext ruleName) <> dot
      $$
      hang
        (sep [ text "The LHS of this rule gave rise to equality constraints"
             , text "that GHC was unable to quantify over:" ]
        )
        4 (pprWithArising $ NE.toList cts)
      $$
      text "NB: this warning will become an error starting from GHC 9.18"
    TcRnDuplicateRoleAnnot list -> mkSimpleDecorated $
      hang (text "Duplicate role annotations for" <+>
            quotes (ppr $ roleAnnotDeclName first_decl) <> colon)
        2 (vcat $ map pp_role_annot $ NE.toList sorted_list)
      where
        sorted_list = NE.sortBy cmp_loc list
        ((L _ first_decl) :| _) = sorted_list

        pp_role_annot (L loc decl) = hang (ppr decl)
                                        4 (text "-- written at" <+> ppr (locA loc))

        cmp_loc = leftmost_smallest `on` getLocA
    TcRnDuplicateKindSig list -> mkSimpleDecorated $
      hang (text "Duplicate standalone kind signatures for" <+>
            quotes (ppr $ standaloneKindSigName first_decl) <> colon)
        2 (vcat $ map pp_kisig $ NE.toList sorted_list)
      where
        sorted_list = NE.sortBy cmp_loc list
        ((L _ first_decl) :| _) = sorted_list

        pp_kisig (L loc decl) =
          hang (ppr decl) 4 (text "-- written at" <+> ppr (locA loc))

        cmp_loc = leftmost_smallest `on` getLocA
    TcRnIllegalDerivStrategy ds -> mkSimpleDecorated $
      text "Illegal deriving strategy" <> colon <+> derivStrategyName ds
    TcRnIllegalMultipleDerivClauses -> mkSimpleDecorated $
      text "Illegal use of multiple, consecutive deriving clauses"
    TcRnNoDerivStratSpecified{} -> mkSimpleDecorated $ text
      "No deriving strategy specified. Did you want stock, newtype, or anyclass?"
    TcRnStupidThetaInGadt{} -> mkSimpleDecorated $
      vcat [text "No context is allowed on a GADT-style data declaration",
            text "(You can put a context on each constructor, though.)"]
    TcRnShadowedTyVarNameInFamResult resName -> mkSimpleDecorated $
       hsep [ text "Type variable", quotes (ppr resName) <> comma
            , text "naming a type family result,"
            ] $$
      text "shadows an already bound type variable"
    TcRnIncorrectTyVarOnLhsOfInjCond resName injFrom -> mkSimpleDecorated $
        vcat [ text $ "Incorrect type variable on the LHS of "
                   ++ "injectivity condition"
      , nest 5
      ( vcat [ text "Expected :" <+> ppr resName
             , text "Actual   :" <+> ppr injFrom ])]
    TcRnUnknownTyVarsOnRhsOfInjCond errorVars -> mkSimpleDecorated $
      hsep [ text "Unknown type variable" <> plural errorVars
           , text "on the RHS of injectivity condition:"
           , interpp'SP errorVars ]
    TcRnBadlyStaged reason bind_lvl use_lvl
      -> mkSimpleDecorated $
         vcat $
         [ text "Stage error:" <+> pprStageCheckReason reason <+>
           hsep [text "is bound at stage" <+> ppr bind_lvl,
                 text "but used at stage" <+> ppr use_lvl]
         ] ++
         [ hsep [ text "Hint: quoting" <+> thBrackets (ppUnless (isValName n) "t") (ppr n)
                , text "or an enclosing expression would allow the quotation to be used in an earlier stage"
                ]
         | StageCheckSplice n <- [reason]
         ]
    TcRnBadlyStagedType name bind_lvl use_lvl
      -> mkSimpleDecorated $
         text "Badly staged type:" <+> ppr name <+>
         hsep [text "is bound at stage" <+> ppr bind_lvl,
               text "but used at stage" <+> ppr use_lvl]
    TcRnStageRestriction reason
      -> mkSimpleDecorated $
         sep [ text "GHC stage restriction:"
             , nest 2 (vcat [ pprStageCheckReason reason <+>
                              text "is used in a top-level splice, quasi-quote, or annotation,"
                            , text "and must be imported, not defined locally"])]
    TcRnTyThingUsedWrong sort thing name
      -> mkSimpleDecorated $
         pprTyThingUsedWrong sort thing name
    TcRnCannotDefaultKindVar var knd ->
      mkSimpleDecorated $
      (vcat [ text "Cannot default kind variable" <+> quotes (ppr var)
            , text "of kind:" <+> ppr knd
            , text "Perhaps enable PolyKinds or add a kind signature" ])
    TcRnUninferrableTyVar tidied_tvs context ->
      mkSimpleDecorated $
      pprWithInvisibleBitsWhen True $
      vcat [ text "Uninferrable type variable"
              <> plural tidied_tvs
              <+> pprWithCommas pprTyVar tidied_tvs
              <+> text "in"
            , pprUninferrableTyVarCtx context ]
    TcRnSkolemEscape escapees tv orig_ty ->
      mkSimpleDecorated $
      pprWithInvisibleBitsWhen True $
      vcat [ sep [ text "Cannot generalise type; skolem" <> plural escapees
                , quotes $ pprTyVars escapees
                , text "would escape" <+> itsOrTheir escapees <+> text "scope"
                ]
          , sep [ text "if I tried to quantify"
                , pprTyVar tv
                , text "in this type:"
                ]
          , nest 2 (pprTidiedType orig_ty)
          , text "(Indeed, I sometimes struggle even printing this correctly,"
          , text " due to its ill-scoped nature.)"
          ]
    TcRnPatSynEscapedCoercion arg bad_co_ne -> mkSimpleDecorated $
      vcat [ text "Iceland Jack!  Iceland Jack! Stop torturing me!"
           , hang (text "Pattern-bound variable")
                2 (ppr arg <+> dcolon <+> ppr (idType arg))
           , nest 2 $
             hang (text "has a type that mentions pattern-bound coercion"
                   <> plural bad_co_list <> colon)
                2 (pprWithCommas ppr bad_co_list)
           , text "Hint: use -fprint-explicit-coercions to see the coercions"
           , text "Probable fix: add a pattern signature" ]
      where
        bad_co_list = NE.toList bad_co_ne
    TcRnPatSynExistentialInResult name pat_ty bad_tvs -> mkSimpleDecorated $
      hang (sep [ text "The result type of the signature for" <+> quotes (ppr name) <> comma
                , text "namely" <+> quotes (ppr pat_ty) ])
        2 (text "mentions existential type variable" <> plural bad_tvs
           <+> pprQuotedList bad_tvs)
    TcRnPatSynArityMismatch name decl_arity missing -> mkSimpleDecorated $
      hang (text "Pattern synonym" <+> quotes (ppr name) <+> text "has"
            <+> speakNOf decl_arity (text "argument"))
         2 (text "but its type signature has" <+> int missing <+> text "fewer arrows")
    TcRnPatSynInvalidRhs ps_name lpat _ reason -> mkSimpleDecorated $
      vcat [ hang (text "Invalid right-hand side of bidirectional pattern synonym"
                   <+> quotes (ppr ps_name) <> colon)
                2 (pprPatSynInvalidRhsReason reason)
           , text "RHS pattern:" <+> ppr lpat ]
    TcRnTyFamDepsDisabled -> mkSimpleDecorated $
      text "Illegal injectivity annotation"
    TcRnAbstractClosedTyFamDecl -> mkSimpleDecorated $
      text "You may define an abstract closed type family" $$
      text "only in a .hs-boot file"
    TcRnPartialFieldSelector fld -> mkSimpleDecorated $
      vcat [ sep [ text "Definition of partial record field" <> colon
                 , nest 2 $ quotes (ppr (occName fld)) ]
           , text "Record selection and update using this field will be partial." ]
    TcRnHasFieldResolvedIncomplete name cons maxCons -> mkSimpleDecorated $
      hang (text "Selecting the record field" <+> quotes (ppr name)
              <+> text "may fail for the following constructors:")
           2
           (hsep $ punctuate comma $
            map ppr (take maxCons cons) ++ [ text "..." | lengthExceeds cons maxCons ])
    TcRnBadFieldAnnotation n con reason -> mkSimpleDecorated $
      hang (pprBadFieldAnnotationReason reason)
         2 (text "on the" <+> speakNth n
            <+> text "argument of" <+> quotes (ppr con))
    TcRnSuperclassCycle (MkSuperclassCycle cls definite details) ->
      let herald | definite  = text "Superclass cycle for"
                 | otherwise = text "Potential superclass cycle for"
      in mkSimpleDecorated $
       vcat [ herald <+> quotes (ppr cls), nest 2 (vcat (pprSuperclassCycleDetail <$> details))]
    TcRnDefaultSigMismatch sel_id dm_ty -> mkSimpleDecorated $
      hang (text "The default type signature for"
            <+> ppr sel_id <> colon)
         2 (ppr dm_ty)
      $$ (text "does not match its corresponding"
          <+> text "non-default type signature")
    TcRnTyFamsDisabled reason -> mkSimpleDecorated $
      text "Illegal family" <+> text sort <+> text "for" <+> quotes name
      where
        (sort, name) = case reason of
          TyFamsDisabledFamily n -> ("declaration", ppr n)
          TyFamsDisabledInstance n -> ("instance", ppr n)
    TcRnBadTyConTelescope tc -> mkSimpleDecorated $
      vcat [ hang (text "The kind of" <+> quotes (ppr tc) <+> text "is ill-scoped")
                2 pp_tc_kind
           , extra
           , hang (text "Perhaps try this order instead:")
                2 (pprTyVars sorted_tvs) ]
      where
        pp_tc_kind = text "Inferred kind:" <+> ppr tc <+> dcolon <+> ppr_untidy (tyConKind tc)
        ppr_untidy ty = pprIfaceType (toIfaceType ty)
          -- We need ppr_untidy here because pprType will tidy the type, which
          -- will turn the bogus kind we are trying to report
          --     T :: forall (a::k) k (b::k) -> blah
          -- into a misleadingly sanitised version
          --     T :: forall (a::k) k1 (b::k1) -> blah

        tcbs = tyConBinders tc
        tvs  = binderVars tcbs
        sorted_tvs = scopedSort tvs

        inferred_tvs  = [ binderVar tcb
                        | tcb <- tcbs, Inferred == tyConBinderForAllTyFlag tcb ]
        specified_tvs = [ binderVar tcb
                        | tcb <- tcbs, Specified == tyConBinderForAllTyFlag tcb ]

        extra
          | null inferred_tvs && null specified_tvs
          = empty
          | null inferred_tvs
          = note $ "Specified variables" <+> pp_spec <+> "always come first"
          | null specified_tvs
          = note inf_always_first
          | otherwise
          = note $ inf_always_first $$
              "then specified variables" <+> pp_spec

        inf_always_first = "Inferred variables" <+> pp_inf $$ "always come first"

        pp_inf  = parens (text "namely:" <+> pprTyVars inferred_tvs)
        pp_spec = parens (text "namely:" <+> pprTyVars specified_tvs)
    TcRnTyFamResultDisabled tc_name tvb -> mkSimpleDecorated $
      text "Illegal result type variable" <+> ppr tvb <+> text "for" <+> quotes (ppr tc_name)
    TcRnRoleValidationFailed role reason -> mkSimpleDecorated $
      vcat [text "Internal error in role inference:",
            pprRoleValidationFailedReason role reason,
            text "Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug"]
    TcRnCommonFieldResultTypeMismatch con1 con2 field_name -> mkSimpleDecorated $
      vcat [sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
                 text "have a common field" <+> quotes (ppr field_name) <> comma],
            nest 2 $ text "but have different result types"]
    TcRnCommonFieldTypeMismatch con1 con2 field_name -> mkSimpleDecorated $
      sep [text "Constructors" <+> ppr con1 <+> text "and" <+> ppr con2,
           text "give different types for field", quotes (ppr field_name)]
    TcRnClassExtensionDisabled cls reason -> mkSimpleDecorated $
      pprDisabledClassExtension cls reason
    TcRnDataConParentTypeMismatch data_con res_ty_tmpl -> mkSimpleDecorated $
      hang (text "Data constructor" <+> quotes (ppr data_con) <+>
            text "returns type" <+> quotes (ppr actual_res_ty))
         2 (text "instead of an instance of its parent type" <+> quotes (ppr res_ty_tmpl))
      where
        actual_res_ty = dataConOrigResTy data_con
    TcRnGADTsDisabled tc_name -> mkSimpleDecorated $
      text "Illegal generalised algebraic data declaration for" <+> quotes (ppr tc_name)
    TcRnExistentialQuantificationDisabled con -> mkSimpleDecorated $
      sdocOption sdocLinearTypes (\show_linear_types ->
        hang (text "Data constructor" <+> quotes (ppr con) <+>
              text "has existential type variables, a context, or a specialised result type")
           2 (ppr con <+> dcolon <+> ppr (dataConDisplayType show_linear_types con)))
    TcRnGADTDataContext tc_name -> mkSimpleDecorated $
      text "A data type declared in GADT style cannot have a context:" <+> quotes (ppr tc_name)
    TcRnMultipleConForNewtype tycon n -> mkSimpleDecorated $
      sep [text "A newtype must have exactly one constructor,",
           nest 2 $ text "but" <+> quotes (ppr tycon) <+> text "has" <+> speakN n]
    TcRnKindSignaturesDisabled thing -> mkSimpleDecorated $
      text "Illegal kind signature" <+> quotes (either ppr with_sig thing)
      where
        with_sig (tc_name, ksig) = ppr tc_name <+> dcolon <+> ppr ksig
    TcRnEmptyDataDeclsDisabled tycon -> mkSimpleDecorated $
      quotes (ppr tycon) <+> text "has no constructors"
    TcRnRoleMismatch var annot inferred -> mkSimpleDecorated $
      hang (text "Role mismatch on variable" <+> ppr var <> colon)
         2 (sep [ text "Annotation says", ppr annot
                , text "but role", ppr inferred
                , text "is required" ])
    TcRnRoleCountMismatch tyvars d@(L _ (RoleAnnotDecl _ _ annots)) -> mkSimpleDecorated $
      hang (text "Wrong number of roles listed in role annotation;" $$
            text "Expected" <+> (ppr tyvars) <> comma <+>
            text "got" <+> (ppr $ length annots) <> colon)
         2 (ppr d)
    TcRnIllegalRoleAnnotation (RoleAnnotDecl _ tycon _) -> mkSimpleDecorated $
      (text "Illegal role annotation for" <+> ppr tycon <> char ';' $$
       text "they are allowed only for datatypes and classes.")
    TcRnRoleAnnotationsDisabled  tc -> mkSimpleDecorated $
      text "Illegal role annotation for" <+> ppr tc
    TcRnIncoherentRoles _ -> mkSimpleDecorated $
      (text "Roles other than" <+> quotes (text "nominal") <+>
      text "for class parameters can lead to incoherence.")
    TcRnUnexpectedKindVar tv_name
      -> mkSimpleDecorated $ text "Unexpected kind variable" <+> quotes (ppr tv_name)

    TcRnNegativeNumTypeLiteral tyLit
      -> mkSimpleDecorated $ text "Illegal literal in type (type literals must not be negative):" <+> ppr tyLit

    TcRnIllegalKind ty_thing _
      -> mkSimpleDecorated $ text "Illegal kind:" <+> (ppr ty_thing)

    TcRnPrecedenceParsingError op1 op2
      -> mkSimpleDecorated $
           hang (text "Precedence parsing error")
           4 (hsep [text "cannot mix", ppr_opfix op1, text "and",
           ppr_opfix op2,
           text "in the same infix expression"])

    TcRnSectionPrecedenceError op arg_op section
      -> mkSimpleDecorated $
           vcat [text "The operator" <+> ppr_opfix op <+> text "of a section",
             nest 4 (sep [text "must have lower precedence than that of the operand,",
                          nest 2 (text "namely" <+> ppr_opfix arg_op)]),
             nest 4 (text "in the section:" <+> quotes (ppr section))]

    TcRnUnexpectedPatSigType ty
      -> mkSimpleDecorated $
           hang (text "Illegal type signature:" <+> quotes (ppr ty))
              2 (text "Type signatures are only allowed in patterns with ScopedTypeVariables")

    TcRnIllegalKindSignature ty
      -> mkSimpleDecorated $ text "Illegal kind signature:" <+> quotes (ppr ty)

    TcRnUnusedQuantifiedTypeVar doc tyVar
      -> mkSimpleDecorated $
           vcat [ text "Unused quantified type variable" <+> quotes (ppr tyVar)
                , inHsDocContext doc ]

    TcRnDataKindsError typeOrKind thing
      -- See Note [Checking for DataKinds] (Wrinkle: Migration story for
      -- DataKinds typechecker errors) in GHC.Tc.Validity for why we give
      -- different diagnostic messages below.
      -> case thing of
           Left renamer_thing ->
             mkSimpleDecorated $
               text "Illegal" <+> ppr_level <> colon <+> quotes (ppr renamer_thing)
           Right typechecker_thing ->
             mkSimpleDecorated $ vcat
               [ text "An occurrence of" <+> quotes (ppr typechecker_thing) <+>
                 text "in a" <+> ppr_level <+> text "requires DataKinds."
               , text "Future versions of GHC will turn this warning into an error."
               ]
      where
        ppr_level = text $ levelString typeOrKind

    TcRnTypeSynonymCycle decl_or_tcs
      -> mkSimpleDecorated $
           sep [ text "Cycle in type synonym declarations:"
               , nest 2 (vcat (map ppr_decl decl_or_tcs)) ]
      where
        ppr_decl = \case
          Right (L loc decl) -> ppr (locA loc) <> colon <+> ppr decl
          Left tc ->
            let n = tyConName tc
            in ppr (getSrcSpan n) <> colon <+> ppr (tyConName tc)
                   <+> text "from external module"
    TcRnZonkerMessage err
      -> mkSimpleDecorated $ pprZonkerMessage err
    TcRnInterfaceError reason
      -> diagnosticMessage (tcOptsIfaceOpts opts) reason
    TcRnSelfImport imp_mod_name
      -> mkSimpleDecorated $
         text "A module cannot import itself:" <+> ppr imp_mod_name
    TcRnNoExplicitImportList mod
      -> mkSimpleDecorated $
         text "The module" <+> quotes (ppr mod) <+> text "does not have an explicit import list"
    TcRnSafeImportsDisabled _
      -> mkSimpleDecorated $
         text "safe import can't be used as Safe Haskell isn't on!"
    TcRnDeprecatedModule mod txt
      -> mkSimpleDecorated $
         sep [ text "Module" <+> quotes (ppr mod) <> text extra <> colon,
               nest 2 (vcat (map (ppr . hsDocString . unLoc) msg)) ]
         where
          (extra, msg) = case txt of
            WarningTxt _ _ msg -> ("", msg)
            DeprecatedTxt _ msg -> (" is deprecated", msg)
    TcRnRedundantSourceImport mod_name
      -> mkSimpleDecorated $
         text "Unnecessary {-# SOURCE #-} in the import of module" <+> quotes (ppr mod_name)
    TcRnImportLookup reason
      -> mkSimpleDecorated $
         pprImportLookup reason
    TcRnUnusedImport decl reason
      -> mkSimpleDecorated $
         pprUnusedImport decl reason
    TcRnDuplicateDecls name sorted_names
      -> mkSimpleDecorated $
         vcat [text "Multiple declarations of" <+>
               quotes (ppr name),
                -- NB. print the OccName, not the Name, because the
                -- latter might not be in scope in the RdrEnv and so will
                -- be printed qualified.
               text "Declared at:" <+>
               vcat (NE.toList $ ppr . nameSrcLoc <$> sorted_names)]
    TcRnPackageImportsDisabled
      -> mkSimpleDecorated $
         text "Package-qualified imports are not enabled"
    TcRnIllegalDataCon name
      -> mkSimpleDecorated $
         hsep [text "Illegal data constructor name", quotes (ppr name)]
    TcRnNestedForallsContexts entity
      -> mkSimpleDecorated $
         what <+> text "cannot contain nested"
         <+> quotes forAllLit <> text "s or contexts"
         where
           what = case entity of
             NFC_Specialize -> text "SPECIALISE instance type"
             NFC_ViaType -> quotes (text "via") <+> text "type"
             NFC_GadtConSig -> text "GADT constructor type signature"
             NFC_InstanceHead -> text "Instance head"
             NFC_StandaloneDerivedInstanceHead -> text "Standalone-derived instance head"
             NFC_DerivedClassType -> text "Derived class type"
    TcRnRedundantRecordWildcard
      -> mkSimpleDecorated $
         text "Record wildcard does not bind any new variables"
    TcRnUnusedRecordWildcard _
      -> mkSimpleDecorated $
         text "No variables bound in the record wildcard match are used"
    TcRnUnusedName name reason
      -> mkSimpleDecorated $
         pprUnusedName name reason
    TcRnQualifiedBinder rdr_name
      -> mkSimpleDecorated $
         text "Qualified name in binding position:" <+> ppr rdr_name
    TcRnTypeApplicationsDisabled ty ty_or_ki
      -> mkSimpleDecorated $
         text "Illegal visible" <+> what <+> text "application" <> colon
           <+> ppr arg
         where
           arg = char '@' <> ppr ty
           what = case ty_or_ki of
             TypeLevel -> text "type"
             KindLevel -> text "kind"
    TcRnInvalidRecordField con field
      -> mkSimpleDecorated $
         hsep [text "Constructor" <+> quotes (ppr con),
               text "does not have field", quotes (ppr field)]
    TcRnTupleTooLarge tup_size
      -> mkSimpleDecorated $
         sep [text "A" <+> int tup_size <> text "-tuple is too large for GHC",
              nest 2 (parens (text "max size is" <+> int mAX_TUPLE_SIZE)),
              nest 2 (text "Workaround: use nested tuples or define a data type")]
    TcRnCTupleTooLarge tup_size
      -> mkSimpleDecorated $
         hang (text "Constraint tuple arity too large:" <+> int tup_size
               <+> parens (text "max arity =" <+> int mAX_CTUPLE_SIZE))
            2 (text "Instead, use a nested tuple")
    TcRnIllegalInferredTyVars _
      -> mkSimpleDecorated $
         text "Inferred type variables are not allowed"
    TcRnAmbiguousName gre_env name gres
      -> mkSimpleDecorated $
         vcat [ text "Ambiguous occurrence" <+> quotes (ppr name) <> dot
              , text "It could refer to"
              , nest 3 (vcat msgs) ]
         where
           np1 NE.:| nps = gres
           msgs = punctuateFinal comma dot $
                    text "either" <+> ppr_gre np1
                 : [text "    or" <+> ppr_gre np | np <- nps]

           ppr_gre gre = pprAmbiguousGreName gre_env gre

    TcRnBindingNameConflict name locs
      -> mkSimpleDecorated $
         vcat [text "Conflicting definitions for" <+> quotes (ppr name),
               locations]
         where
           locations =
             text "Bound at:"
             <+> vcat (map ppr (sortBy leftmost_smallest (NE.toList locs)))
    TcRnNonCanonicalDefinition reason inst_ty
      -> mkSimpleDecorated $
         pprNonCanonicalDefinition inst_ty reason
    TcRnDefaultedExceptionContext ct_loc ->
      mkSimpleDecorated $ vcat [ header, warning, proposal ]
      where
        header, warning, proposal :: SDoc
        header
          = vcat [ text "Solving for an implicit ExceptionContext constraint"
                 , nest 2 $ pprCtOrigin (ctLocOrigin ct_loc) <> text "." ]
        warning
          = vcat [ text "Future versions of GHC will turn this warning into an error." ]
        proposal
          = vcat [ text "See GHC Proposal #330." ]
    TcRnImplicitImportOfPrelude
      -> mkSimpleDecorated $
         text "Module" <+> quotes (text "Prelude") <+> text "implicitly imported."
    TcRnMissingMain explicit_export_list main_mod main_occ
      -> mkSimpleDecorated $
         text "The" <+> ppMainFn main_occ
        <+> text "is not" <+> defOrExp <+> text "module"
        <+> quotes (ppr main_mod)
      where
        defOrExp :: SDoc
        defOrExp | explicit_export_list = text "exported by"
                 | otherwise            = text "defined in"
    TcRnGhciUnliftedBind id
      -> mkSimpleDecorated $
         sep [ text "GHCi can't bind a variable of unlifted type:"
             , nest 2 (pprPrefixOcc id <+> dcolon <+> ppr (idType id)) ]
    TcRnGhciMonadLookupFail ty lookups
      -> mkSimpleDecorated $
         hang (text "Can't find type" <+> pp_ty <> dot $$ ambig_msg)
           2 (text "When checking that" <+> pp_ty <>
              text "is a monad that can execute GHCi statements.")
      where
        pp_ty = quotes (text ty)
        ambig_msg = case lookups of
          Just (_:_:_) -> text "The type is ambiguous."
          _            -> empty
    TcRnIllegalQuasiQuotes -> mkSimpleDecorated $
      text "Quasi-quotes are not permitted without QuasiQuotes"
    TcRnTHError err -> pprTHError err
    TcRnPatersonCondFailure reason ctxt lhs rhs ->
      mkSimpleDecorated $ pprPatersonCondFailure reason ctxt lhs rhs
    TcRnIllegalInvisTyVarBndr bndr ->
      mkSimpleDecorated $
        hang (text "Illegal invisible type variable binder:")
           2 (ppr bndr)
    TcRnIllegalWildcardTyVarBndr bndr ->
      mkSimpleDecorated $
        hang (text "Illegal wildcard binder:")
           2 (ppr bndr)

    TcRnInvalidInvisTyVarBndr name hs_bndr ->
      mkSimpleDecorated $
        vcat [ hang (text "Invalid invisible type variable binder:")
                  2 (ppr hs_bndr)
             , text "There is no matching forall-bound variable"
             , text "in the standalone kind signature for" <+> quotes (ppr name) <> dot
             , note $ vcat [
                "Only" <+> quotes "forall a." <+> "-quantification matches invisible binders,",
                "whereas" <+> quotes "forall {a}." <+> "and" <+> quotes "forall a ->" <+> "do not"
             ]]

    TcRnInvisBndrWithoutSig _ hs_bndr ->
      mkSimpleDecorated $
        vcat [ hang (text "Invalid invisible type variable binder:")
                  2 (ppr hs_bndr)
             , text "Either a standalone kind signature (SAKS)"
             , text "or a complete user-supplied kind (CUSK, legacy feature)"
             , text "is required to use invisible binders." ]

    TcRnImplicitRhsQuantification kv -> mkSimpleDecorated $
      vcat [ text "The variable" <+> quotes (ppr kv) <+> text "occurs free on the RHS of the type declaration"
           , text "The next version of GHC will reject this program, no longer implicitly quantify over" <+> quotes (ppr kv)
           ]

    TcRnInvalidDefaultedTyVar wanteds proposal bad_tvs ->
      mkSimpleDecorated $
      pprWithInvisibleBitsWhen True $
      vcat [ text "Invalid defaulting proposal."
           , hang (text "The following type variable" <> plural (NE.toList bad_tvs) <+> text "cannot be defaulted, as" <+> why <> colon)
                2 (pprQuotedList (NE.toList bad_tvs))
           , hang (text "Defaulting proposal:")
                2 (ppr proposal)
           , hang (text "Wanted constraints:")
                2 (pprQuotedList (map ctPred wanteds))
           ]
        where
          why
            | _ :| [] <- bad_tvs
            = text "it is not an unfilled metavariable"
            | otherwise
            = text "they are not unfilled metavariables"

    TcRnNamespacedWarningPragmaWithoutFlag warning@(Warning (kw, _) _ txt) -> mkSimpleDecorated $
      vcat [ text "Illegal use of the" <+> quotes (ppr kw) <+> text "keyword:"
           , nest 2 (ppr warning)
           , text "in a" <+> pragma_type <+> text "pragma"
           ]
      where
        pragma_type = case txt of
          WarningTxt{} -> text "WARNING"
          DeprecatedTxt{} -> text "DEPRECATED"

    TcRnIllegalInvisibleTypePattern tp reason -> mkSimpleDecorated $
      vcat [ hang (text "Illegal invisible type pattern:")
                  2 (char '@' <> ppr tp)
           , case reason of
               InvisPatWithoutFlag -> empty
               InvisPatNoForall  -> text "An invisible type pattern must be checked against a forall."
               InvisPatMisplaced -> text "An invisible type pattern must occur in an argument position."
           ]

    TcRnNamespacedFixitySigWithoutFlag sig@(FixitySig kw _ _) -> mkSimpleDecorated $
      vcat [ text "Illegal use of the" <+> quotes (ppr kw) <+> text "keyword:"
           , nest 2 (ppr sig)
           , text "in a fixity signature"
           ]

    TcRnOutOfArityTyVar ts_name tv_name -> mkDecorated
      [ vcat [ text "The arity of" <+> quotes (ppr ts_name) <+> text "is insufficiently high to accommodate"
             , text "an implicit binding for the" <+> quotes (ppr tv_name) <+> text "type variable." ]
      , suggestion ]
      where
        suggestion =
          text "Use" <+> quotes at_bndr     <+> text "on the LHS" <+>
          text "or"  <+> quotes forall_bndr <+> text "on the RHS" <+>
          text "to bring it into scope."
        at_bndr     = char '@' <> ppr tv_name
        forall_bndr = text "forall" <+> ppr tv_name <> text "."

    TcRnUnexpectedTypeSyntaxInTerms syntax -> mkSimpleDecorated $
      text "Unexpected" <+> pprTypeSyntaxName syntax

  diagnosticReason :: TcRnMessage -> DiagnosticReason
  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed _ m -> diagnosticReason m
    TcRnWithHsDocContext _ msg
      -> diagnosticReason msg
    TcRnSolverReport _report reason
      -> reason -- Error, or a Warning if we are deferring type errors
    TcRnSolverDepthError {}
      -> ErrorWithoutFlag
    TcRnRedundantConstraints {}
      -> WarningWithFlag Opt_WarnRedundantConstraints
    TcRnInaccessibleCode {}
      -> WarningWithFlag Opt_WarnInaccessibleCode
    TcRnInaccessibleCoAxBranch {}
      -> WarningWithFlag Opt_WarnInaccessibleCode
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> ErrorWithoutFlag
    TcRnImplicitLift{}
      -> WarningWithFlag Opt_WarnImplicitLift
    TcRnUnusedPatternBinds{}
      -> WarningWithFlag Opt_WarnUnusedPatternBinds
    TcRnDodgyImports{}
      -> WarningWithFlag Opt_WarnDodgyImports
    TcRnDodgyExports{}
      -> WarningWithFlag Opt_WarnDodgyExports
    TcRnMissingImportList{}
      -> WarningWithFlag Opt_WarnMissingImportList
    TcRnUnsafeDueToPlugin{}
      -> WarningWithoutFlag
    TcRnModMissingRealSrcSpan{}
      -> ErrorWithoutFlag
    TcRnIdNotExportedFromModuleSig{}
      -> ErrorWithoutFlag
    TcRnIdNotExportedFromLocalSig{}
      -> ErrorWithoutFlag
    TcRnShadowedName{}
      -> WarningWithFlag Opt_WarnNameShadowing
    TcRnInvalidWarningCategory{}
      -> ErrorWithoutFlag
    TcRnDuplicateWarningDecls{}
      -> ErrorWithoutFlag
    TcRnSimplifierTooManyIterations{}
      -> ErrorWithoutFlag
    TcRnIllegalPatSynDecl{}
      -> ErrorWithoutFlag
    TcRnLinearPatSyn{}
      -> ErrorWithoutFlag
    TcRnEmptyRecordUpdate
      -> ErrorWithoutFlag
    TcRnIllegalFieldPunning{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardsInRecord{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardInType{}
      -> ErrorWithoutFlag
    TcRnIllegalNamedWildcardInTypeArgument{}
      -> ErrorWithoutFlag
    TcRnIllegalImplicitTyVarInTypeArgument{}
      -> ErrorWithoutFlag
    TcRnDuplicateFieldName{}
      -> ErrorWithoutFlag
    TcRnIllegalViewPattern{}
      -> ErrorWithoutFlag
    TcRnCharLiteralOutOfRange{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardsInConstructor{}
      -> ErrorWithoutFlag
    TcRnIgnoringAnnotations{}
      -> WarningWithoutFlag
    TcRnAnnotationInSafeHaskell
      -> ErrorWithoutFlag
    TcRnInvalidTypeApplication{}
      -> ErrorWithoutFlag
    TcRnTagToEnumMissingValArg
      -> ErrorWithoutFlag
    TcRnTagToEnumUnspecifiedResTy{}
      -> ErrorWithoutFlag
    TcRnTagToEnumResTyNotAnEnum{}
      -> ErrorWithoutFlag
    TcRnTagToEnumResTyTypeData{}
      -> ErrorWithoutFlag
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> ErrorWithoutFlag
    TcRnIllegalHsBootOrSigDecl {}
      -> ErrorWithoutFlag
    TcRnBootMismatch {}
      -> ErrorWithoutFlag
    TcRnRecursivePatternSynonym{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigTyVarMismatch{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSigBadQuantifier{}
      -> ErrorWithoutFlag
    TcRnMissingSignature what exported
      -> WarningWithFlags $ missingSignatureWarningFlags what exported
    TcRnPolymorphicBinderMissingSig{}
      -> WarningWithFlag Opt_WarnMissingLocalSignatures
    TcRnOverloadedSig{}
      -> ErrorWithoutFlag
    TcRnTupleConstraintInst{}
      -> ErrorWithoutFlag
    TcRnUserTypeError{}
      -> ErrorWithoutFlag
    TcRnConstraintInKind{}
      -> ErrorWithoutFlag
    TcRnUnboxedTupleOrSumTypeFuncArg{}
      -> ErrorWithoutFlag
    TcRnLinearFuncInKind{}
      -> ErrorWithoutFlag
    TcRnForAllEscapeError{}
      -> ErrorWithoutFlag
    TcRnSimplifiableConstraint{}
      -> WarningWithFlag Opt_WarnSimplifiableClassConstraints
    TcRnArityMismatch{}
      -> ErrorWithoutFlag
    TcRnIllegalInstance rea
      -> illegalInstanceReason rea
    TcRnVDQInTermType{}
      -> ErrorWithoutFlag
    TcRnBadQuantPredHead{}
      -> ErrorWithoutFlag
    TcRnIllegalTupleConstraint{}
      -> ErrorWithoutFlag
    TcRnNonTypeVarArgInConstraint{}
      -> ErrorWithoutFlag
    TcRnIllegalImplicitParam{}
      -> ErrorWithoutFlag
    TcRnIllegalConstraintSynonymOfKind{}
      -> ErrorWithoutFlag
    TcRnOversaturatedVisibleKindArg{}
      -> ErrorWithoutFlag
    TcRnForAllRankErr{}
      -> ErrorWithoutFlag
    TcRnMonomorphicBindings{}
      -> WarningWithFlag Opt_WarnMonomorphism
    TcRnOrphanInstance{}
      -> WarningWithFlag Opt_WarnOrphans
    TcRnFunDepConflict{}
      -> ErrorWithoutFlag
    TcRnDupInstanceDecls{}
      -> ErrorWithoutFlag
    TcRnConflictingFamInstDecls{}
      -> ErrorWithoutFlag
    TcRnFamInstNotInjective{}
      -> ErrorWithoutFlag
    TcRnBangOnUnliftedType{}
      -> WarningWithFlag Opt_WarnRedundantStrictnessFlags
    TcRnLazyBangOnUnliftedType{}
      -> WarningWithFlag Opt_WarnRedundantStrictnessFlags
    TcRnMultipleDefaultDeclarations{}
      -> ErrorWithoutFlag
    TcRnBadDefaultType{}
      -> ErrorWithoutFlag
    TcRnPatSynBundledWithNonDataCon{}
      -> ErrorWithoutFlag
    TcRnPatSynBundledWithWrongType{}
      -> ErrorWithoutFlag
    TcRnDupeModuleExport{}
      -> WarningWithFlag Opt_WarnDuplicateExports
    TcRnExportedModNotImported{}
      -> ErrorWithoutFlag
    TcRnNullExportedModule{}
      -> WarningWithFlag Opt_WarnDodgyExports
    TcRnMissingExportList{}
      -> WarningWithFlag Opt_WarnMissingExportList
    TcRnExportHiddenComponents{}
      -> ErrorWithoutFlag
    TcRnExportHiddenDefault{}
      -> ErrorWithoutFlag
    TcRnDuplicateExport{}
      -> WarningWithFlag Opt_WarnDuplicateExports
    TcRnDuplicateNamedDefaultExport{}
      -> WarningWithFlag Opt_WarnDuplicateExports
    TcRnExportedParentChildMismatch{}
      -> ErrorWithoutFlag
    TcRnConflictingExports{}
      -> ErrorWithoutFlag
    TcRnDuplicateFieldExport {}
      -> ErrorWithoutFlag
    TcRnAmbiguousFieldInUpdate {}
      -> ErrorWithoutFlag
    TcRnAmbiguousRecordUpdate{}
      -> WarningWithFlag Opt_WarnAmbiguousFields
    TcRnMissingFields{}
      -> WarningWithFlag Opt_WarnMissingFields
    TcRnFieldUpdateInvalidType{}
      -> ErrorWithoutFlag
    TcRnMissingStrictFields{}
      -> ErrorWithoutFlag
    TcRnBadRecordUpdate{}
      -> ErrorWithoutFlag
    TcRnIllegalStaticExpression {}
      -> ErrorWithoutFlag
    TcRnStaticFormNotClosed{}
      -> ErrorWithoutFlag
    TcRnUselessTypeable
      -> WarningWithFlag Opt_WarnDerivingTypeable
    TcRnDerivingDefaults{}
      -> WarningWithFlag Opt_WarnDerivingDefaults
    TcRnNonUnaryTypeclassConstraint{}
      -> ErrorWithoutFlag
    TcRnPartialTypeSignatures{}
      -> WarningWithFlag Opt_WarnPartialTypeSignatures
    TcRnCannotDeriveInstance _ _ _ _ rea
      -> case rea of
           DerivErrNotWellKinded{}                 -> ErrorWithoutFlag
           DerivErrSafeHaskellGenericInst          -> ErrorWithoutFlag
           DerivErrDerivingViaWrongKind{}          -> ErrorWithoutFlag
           DerivErrNoEtaReduce{}                   -> ErrorWithoutFlag
           DerivErrBootFileFound                   -> ErrorWithoutFlag
           DerivErrDataConsNotAllInScope{}         -> ErrorWithoutFlag
           DerivErrGNDUsedOnData                   -> ErrorWithoutFlag
           DerivErrNullaryClasses                  -> ErrorWithoutFlag
           DerivErrLastArgMustBeApp                -> ErrorWithoutFlag
           DerivErrNoFamilyInstance{}              -> ErrorWithoutFlag
           DerivErrNotStockDeriveable{}            -> ErrorWithoutFlag
           DerivErrHasAssociatedDatatypes{}        -> ErrorWithoutFlag
           DerivErrNewtypeNonDeriveableClass       -> ErrorWithoutFlag
           DerivErrCannotEtaReduceEnough{}         -> ErrorWithoutFlag
           DerivErrOnlyAnyClassDeriveable{}        -> ErrorWithoutFlag
           DerivErrNotDeriveable{}                 -> ErrorWithoutFlag
           DerivErrNotAClass{}                     -> ErrorWithoutFlag
           DerivErrNoConstructors{}                -> ErrorWithoutFlag
           DerivErrLangExtRequired{}               -> ErrorWithoutFlag
           DerivErrDunnoHowToDeriveForType{}       -> ErrorWithoutFlag
           DerivErrMustBeEnumType{}                -> ErrorWithoutFlag
           DerivErrMustHaveExactlyOneConstructor{} -> ErrorWithoutFlag
           DerivErrMustHaveSomeParameters{}        -> ErrorWithoutFlag
           DerivErrMustNotHaveClassContext{}       -> ErrorWithoutFlag
           DerivErrBadConstructor{}                -> ErrorWithoutFlag
           DerivErrGenerics{}                      -> ErrorWithoutFlag
           DerivErrEnumOrProduct{}                 -> ErrorWithoutFlag
    TcRnLookupInstance _ _ _
      -> ErrorWithoutFlag
    TcRnLazyGADTPattern
      -> ErrorWithoutFlag
    TcRnArrowProcGADTPattern
      -> ErrorWithoutFlag
    TcRnTypeEqualityOutOfScope
      -> WarningWithFlag Opt_WarnTypeEqualityOutOfScope
    TcRnTypeEqualityRequiresOperators
      -> WarningWithFlag Opt_WarnTypeEqualityRequiresOperators
    TcRnIllegalTypeOperator {}
      -> ErrorWithoutFlag
    TcRnIllegalTypeOperatorDecl {}
      -> ErrorWithoutFlag
    TcRnGADTMonoLocalBinds {}
      -> WarningWithFlag Opt_WarnGADTMonoLocalBinds
    TcRnIncorrectNameSpace {}
      -> ErrorWithoutFlag
    TcRnNotInScope {}
      -> ErrorWithoutFlag
    TcRnTermNameInType {}
      -> ErrorWithoutFlag
    TcRnUntickedPromotedThing {}
      -> WarningWithFlag Opt_WarnUntickedPromotedConstructors
    TcRnIllegalBuiltinSyntax {}
      -> ErrorWithoutFlag
    TcRnWarnDefaulting {}
      -> WarningWithFlag Opt_WarnTypeDefaults
    TcRnWarnClashingDefaultImports {}
      -> WarningWithFlag Opt_WarnTypeDefaults
    TcRnForeignImportPrimExtNotSet{}
      -> ErrorWithoutFlag
    TcRnForeignImportPrimSafeAnn{}
      -> ErrorWithoutFlag
    TcRnForeignFunctionImportAsValue{}
      -> ErrorWithoutFlag
    TcRnFunPtrImportWithoutAmpersand{}
      -> WarningWithFlag Opt_WarnDodgyForeignImports
    TcRnIllegalForeignDeclBackend{}
      -> ErrorWithoutFlag
    TcRnUnsupportedCallConv _ unsupportedCC
      -> case unsupportedCC of
           StdCallConvUnsupported -> WarningWithFlag Opt_WarnUnsupportedCallingConventions
           _ -> ErrorWithoutFlag
    TcRnIllegalForeignType{}
      -> ErrorWithoutFlag
    TcRnInvalidCIdentifier{}
      -> ErrorWithoutFlag
    TcRnExpectedValueId{}
      -> ErrorWithoutFlag
    TcRnRecSelectorEscapedTyVar{}
      -> ErrorWithoutFlag
    TcRnPatSynNotBidirectional{}
      -> ErrorWithoutFlag
    TcRnIllegalDerivingItem{}
      -> ErrorWithoutFlag
    TcRnIllegalDefaultClass{}
      -> ErrorWithoutFlag
    TcRnIllegalNamedDefault{}
      -> ErrorWithoutFlag
    TcRnUnexpectedAnnotation{}
      -> ErrorWithoutFlag
    TcRnIllegalRecordSyntax{}
      -> ErrorWithoutFlag
    TcRnInvalidVisibleKindArgument{}
      -> ErrorWithoutFlag
    TcRnTooManyBinders{}
      -> ErrorWithoutFlag
    TcRnDifferentNamesForTyVar{}
      -> ErrorWithoutFlag
    TcRnDisconnectedTyVar{}
      -> ErrorWithoutFlag
    TcRnInvalidReturnKind{}
      -> ErrorWithoutFlag
    TcRnClassKindNotConstraint{}
      -> ErrorWithoutFlag
    TcRnUnpromotableThing{}
      -> ErrorWithoutFlag
    TcRnIllegalTermLevelUse{}
      -> ErrorWithoutFlag
    TcRnMatchesHaveDiffNumArgs{}
      -> ErrorWithoutFlag
    TcRnCannotBindScopedTyVarInPatSig{}
      -> ErrorWithoutFlag
    TcRnCannotBindTyVarsInPatBind{}
      -> ErrorWithoutFlag
    TcRnTooManyTyArgsInConPattern{}
      -> ErrorWithoutFlag
    TcRnMultipleInlinePragmas{}
      -> WarningWithoutFlag
    TcRnUnexpectedPragmas{}
      -> WarningWithoutFlag
    TcRnNonOverloadedSpecialisePragma{}
      -> WarningWithoutFlag
    TcRnSpecialiseNotVisible{}
      -> WarningWithoutFlag
    TcRnPragmaWarning{pragma_warning_msg}
      -> WarningWithCategory (warningTxtCategory pragma_warning_msg)
    TcRnDifferentExportWarnings _ _
      -> ErrorWithoutFlag
    TcRnIncompleteExportWarnings _ _
      -> WarningWithFlag Opt_WarnIncompleteExportWarnings
    TcRnIllegalHsigDefaultMethods{}
      -> ErrorWithoutFlag
    TcRnHsigFixityMismatch{}
      -> ErrorWithoutFlag
    TcRnHsigShapeMismatch{}
      -> ErrorWithoutFlag
    TcRnHsigMissingModuleExport{}
      -> ErrorWithoutFlag
    TcRnBadGenericMethod{}
      -> ErrorWithoutFlag
    TcRnWarningMinimalDefIncomplete{}
      -> WarningWithoutFlag
    TcRnDefaultMethodForPragmaLacksBinding{}
      -> ErrorWithoutFlag
    TcRnIgnoreSpecialisePragmaOnDefMethod{}
      -> WarningWithoutFlag
    TcRnBadMethodErr{}
      -> ErrorWithoutFlag
    TcRnIllegalTypeData
      -> ErrorWithoutFlag
    TcRnIllegalQuasiQuotes{}
      -> ErrorWithoutFlag
    TcRnTHError err
      -> thErrorReason err
    TcRnTypeDataForbids{}
      -> ErrorWithoutFlag
    TcRnIllegalNewtype{}
      -> ErrorWithoutFlag
    TcRnOrPatBindsVariables{}
      -> ErrorWithoutFlag
    TcRnUnsatisfiedMinimalDef{}
      -> WarningWithFlag (Opt_WarnMissingMethods)
    TcRnMisplacedInstSig{}
      -> ErrorWithoutFlag
    TcRnNoRebindableSyntaxRecordDot{}
      -> ErrorWithoutFlag
    TcRnNoFieldPunsRecordDot{}
      -> ErrorWithoutFlag
    TcRnListComprehensionDuplicateBinding{}
      -> ErrorWithoutFlag
    TcRnEmptyStmtsGroup{}
      -> ErrorWithoutFlag
    TcRnLastStmtNotExpr{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStatementInContext{}
      -> ErrorWithoutFlag
    TcRnSectionWithoutParentheses{}
      -> ErrorWithoutFlag
    TcRnIllegalImplicitParameterBindings{}
      -> ErrorWithoutFlag
    TcRnIllegalTupleSection{}
      -> ErrorWithoutFlag
    TcRnCapturedTermName{}
      -> WarningWithFlag Opt_WarnTermVariableCapture
    TcRnBindingOfExistingName{}
      -> ErrorWithoutFlag
    TcRnMultipleFixityDecls{}
      -> ErrorWithoutFlag
    TcRnIllegalPatternSynonymDecl{}
      -> ErrorWithoutFlag
    TcRnIllegalClassBinding{}
      -> ErrorWithoutFlag
    TcRnOrphanCompletePragma{}
      -> ErrorWithoutFlag
    TcRnSpecSigShape{}
      -> ErrorWithoutFlag
    TcRnEmptyCase{}
      -> ErrorWithoutFlag
    TcRnNonStdGuards{}
      -> WarningWithoutFlag
    TcRnDuplicateSigDecl{}
      -> ErrorWithoutFlag
    TcRnMisplacedSigDecl{}
      -> ErrorWithoutFlag
    TcRnUnexpectedDefaultSig{}
      -> ErrorWithoutFlag
    TcRnDuplicateMinimalSig{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStandaloneDerivingDecl{}
      -> ErrorWithoutFlag
    TcRnUnusedVariableInRuleDecl{}
      -> ErrorWithoutFlag
    TcRnUnexpectedStandaloneKindSig{}
      -> ErrorWithoutFlag
    TcRnIllegalRuleLhs{}
      -> ErrorWithoutFlag
    TcRnRuleLhsEqualities{}
      -> WarningWithFlag Opt_WarnRuleLhsEqualities
    TcRnDuplicateRoleAnnot{}
      -> ErrorWithoutFlag
    TcRnDuplicateKindSig{}
      -> ErrorWithoutFlag
    TcRnIllegalDerivStrategy{}
      -> ErrorWithoutFlag
    TcRnIllegalMultipleDerivClauses{}
      -> ErrorWithoutFlag
    TcRnNoDerivStratSpecified{}
      -> WarningWithFlag Opt_WarnMissingDerivingStrategies
    TcRnStupidThetaInGadt{}
      -> ErrorWithoutFlag
    TcRnShadowedTyVarNameInFamResult{}
      -> ErrorWithoutFlag
    TcRnIncorrectTyVarOnLhsOfInjCond{}
      -> ErrorWithoutFlag
    TcRnUnknownTyVarsOnRhsOfInjCond{}
      -> ErrorWithoutFlag
    TcRnBadlyStaged{}
      -> ErrorWithoutFlag
    TcRnBadlyStagedType{}
      -> WarningWithFlag Opt_WarnBadlyStagedTypes
    TcRnStageRestriction{}
      -> ErrorWithoutFlag
    TcRnTyThingUsedWrong{}
      -> ErrorWithoutFlag
    TcRnCannotDefaultKindVar{}
      -> ErrorWithoutFlag
    TcRnUninferrableTyVar{}
      -> ErrorWithoutFlag
    TcRnSkolemEscape{}
      -> ErrorWithoutFlag
    TcRnPatSynEscapedCoercion{}
      -> ErrorWithoutFlag
    TcRnPatSynExistentialInResult{}
      -> ErrorWithoutFlag
    TcRnPatSynArityMismatch{}
      -> ErrorWithoutFlag
    TcRnPatSynInvalidRhs{}
      -> ErrorWithoutFlag
    TcRnTyFamDepsDisabled{}
      -> ErrorWithoutFlag
    TcRnAbstractClosedTyFamDecl{}
      -> ErrorWithoutFlag
    TcRnPartialFieldSelector{}
      -> WarningWithFlag Opt_WarnPartialFields
    TcRnHasFieldResolvedIncomplete{}
      -> WarningWithFlag Opt_WarnIncompleteRecordSelectors
    TcRnBadFieldAnnotation _ _ LazyFieldsDisabled
      -> ErrorWithoutFlag
    TcRnBadFieldAnnotation{}
      -> WarningWithoutFlag
    TcRnSuperclassCycle{}
      -> ErrorWithoutFlag
    TcRnDefaultSigMismatch{}
      -> ErrorWithoutFlag
    TcRnTyFamsDisabled{}
      -> ErrorWithoutFlag
    TcRnBadTyConTelescope {}
      -> ErrorWithoutFlag
    TcRnTyFamResultDisabled{}
      -> ErrorWithoutFlag
    TcRnRoleValidationFailed{}
      -> ErrorWithoutFlag
    TcRnCommonFieldResultTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnCommonFieldTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnClassExtensionDisabled{}
      -> ErrorWithoutFlag
    TcRnDataConParentTypeMismatch{}
      -> ErrorWithoutFlag
    TcRnGADTsDisabled{}
      -> ErrorWithoutFlag
    TcRnExistentialQuantificationDisabled{}
      -> ErrorWithoutFlag
    TcRnGADTDataContext{}
      -> ErrorWithoutFlag
    TcRnMultipleConForNewtype{}
      -> ErrorWithoutFlag
    TcRnKindSignaturesDisabled{}
      -> ErrorWithoutFlag
    TcRnEmptyDataDeclsDisabled{}
      -> ErrorWithoutFlag
    TcRnRoleMismatch{}
      -> ErrorWithoutFlag
    TcRnRoleCountMismatch{}
      -> ErrorWithoutFlag
    TcRnIllegalRoleAnnotation{}
      -> ErrorWithoutFlag
    TcRnRoleAnnotationsDisabled{}
      -> ErrorWithoutFlag
    TcRnIncoherentRoles{}
      -> ErrorWithoutFlag
    TcRnUnexpectedKindVar{}
      -> ErrorWithoutFlag
    TcRnNegativeNumTypeLiteral{}
      -> ErrorWithoutFlag
    TcRnIllegalKind{}
      -> ErrorWithoutFlag
    TcRnPrecedenceParsingError{}
      -> ErrorWithoutFlag
    TcRnSectionPrecedenceError{}
      -> ErrorWithoutFlag
    TcRnUnexpectedPatSigType{}
      -> ErrorWithoutFlag
    TcRnIllegalKindSignature{}
      -> ErrorWithoutFlag
    TcRnUnusedQuantifiedTypeVar{}
      -> WarningWithFlag Opt_WarnUnusedForalls
    TcRnDataKindsError _ thing
      -- DataKinds errors can arise from either the renamer (Left) or the
      -- typechecker (Right). The latter category of DataKinds errors are a
      -- fairly recent addition to GHC (introduced in GHC 9.10), and in order
      -- to prevent these new errors from breaking users' code, we temporarily
      -- downgrade these errors to warnings. See Note [Checking for DataKinds]
      -- (Wrinkle: Migration story for DataKinds typechecker errors)
      -- in GHC.Tc.Validity.
      -> case thing of
           Left  _ -> ErrorWithoutFlag
           Right _ -> WarningWithFlag Opt_WarnDataKindsTC
    TcRnTypeSynonymCycle{}
      -> ErrorWithoutFlag
    TcRnZonkerMessage msg
      -> zonkerMessageReason msg
    TcRnInterfaceError err
      -> interfaceErrorReason err
    TcRnSelfImport{}
      -> ErrorWithoutFlag
    TcRnNoExplicitImportList{}
      -> WarningWithFlag Opt_WarnMissingImportList
    TcRnSafeImportsDisabled{}
      -> ErrorWithoutFlag
    TcRnDeprecatedModule _ txt
      -> WarningWithCategory (warningTxtCategory txt)
    TcRnRedundantSourceImport{}
      -> WarningWithoutFlag
    TcRnImportLookup{}
      -> ErrorWithoutFlag
    TcRnUnusedImport{}
      -> WarningWithFlag Opt_WarnUnusedImports
    TcRnDuplicateDecls{}
      -> ErrorWithoutFlag
    TcRnPackageImportsDisabled
      -> ErrorWithoutFlag
    TcRnIllegalDataCon{}
      -> ErrorWithoutFlag
    TcRnNestedForallsContexts{}
      -> ErrorWithoutFlag
    TcRnRedundantRecordWildcard
      -> WarningWithFlag Opt_WarnRedundantRecordWildcards
    TcRnUnusedRecordWildcard{}
      -> WarningWithFlag Opt_WarnUnusedRecordWildcards
    TcRnUnusedName _ prov
      -> WarningWithFlag $ case prov of
        UnusedNameTopDecl -> Opt_WarnUnusedTopBinds
        UnusedNameImported _ -> Opt_WarnUnusedTopBinds
        UnusedNameTypePattern -> Opt_WarnUnusedTypePatterns
        UnusedNameMatch -> Opt_WarnUnusedMatches
        UnusedNameLocalBind -> Opt_WarnUnusedLocalBinds
    TcRnQualifiedBinder{}
      -> ErrorWithoutFlag
    TcRnTypeApplicationsDisabled{}
      -> ErrorWithoutFlag
    TcRnInvalidRecordField{}
      -> ErrorWithoutFlag
    TcRnTupleTooLarge{}
      -> ErrorWithoutFlag
    TcRnCTupleTooLarge{}
      -> ErrorWithoutFlag
    TcRnIllegalInferredTyVars{}
      -> ErrorWithoutFlag
    TcRnAmbiguousName{}
      -> ErrorWithoutFlag
    TcRnBindingNameConflict{}
      -> ErrorWithoutFlag
    TcRnNonCanonicalDefinition (NonCanonicalMonoid _) _
      -> WarningWithFlag Opt_WarnNonCanonicalMonoidInstances
    TcRnNonCanonicalDefinition (NonCanonicalMonad _) _
      -> WarningWithFlag Opt_WarnNonCanonicalMonadInstances
    TcRnDefaultedExceptionContext{}
      -> WarningWithFlag Opt_WarnDefaultedExceptionContext
    TcRnImplicitImportOfPrelude {}
      -> WarningWithFlag Opt_WarnImplicitPrelude
    TcRnMissingMain {}
      -> ErrorWithoutFlag
    TcRnGhciUnliftedBind {}
      -> ErrorWithoutFlag
    TcRnGhciMonadLookupFail {}
      -> ErrorWithoutFlag
    TcRnMissingRoleAnnotation{}
      -> WarningWithFlag Opt_WarnMissingRoleAnnotations
    TcRnIllegalInvisTyVarBndr{}
      -> ErrorWithoutFlag
    TcRnIllegalWildcardTyVarBndr{}
      -> ErrorWithoutFlag
    TcRnInvalidInvisTyVarBndr{}
      -> ErrorWithoutFlag
    TcRnInvisBndrWithoutSig{}
      -> ErrorWithoutFlag
    TcRnImplicitRhsQuantification{}
      -> WarningWithFlag Opt_WarnImplicitRhsQuantification
    TcRnPatersonCondFailure{}
      -> ErrorWithoutFlag
    TcRnIllformedTypePattern{}
      -> ErrorWithoutFlag
    TcRnIllegalTypePattern{}
      -> ErrorWithoutFlag
    TcRnIllformedTypeArgument{}
      -> ErrorWithoutFlag
    TcRnIllegalTypeExpr{}
      -> ErrorWithoutFlag
    TcRnInvalidDefaultedTyVar{}
      -> ErrorWithoutFlag
    TcRnNamespacedWarningPragmaWithoutFlag{}
      -> ErrorWithoutFlag
    TcRnIllegalInvisibleTypePattern{}
      -> ErrorWithoutFlag
    TcRnNamespacedFixitySigWithoutFlag{}
      -> ErrorWithoutFlag
    TcRnOutOfArityTyVar{}
      -> ErrorWithoutFlag
    TcRnUnexpectedTypeSyntaxInTerms{}
      -> ErrorWithoutFlag

  diagnosticHints = \case
    TcRnUnknownMessage m
      -> diagnosticHints m
    TcRnMessageWithInfo _ msg_with_info
      -> case msg_with_info of
           TcRnMessageDetailed info m ->
             errInfoHints info ++ diagnosticHints m
    TcRnWithHsDocContext _ msg
      -> diagnosticHints msg
    TcRnSolverReport (SolverReportWithCtxt ctxt msg) _
      -> tcSolverReportMsgHints ctxt msg
    TcRnSolverDepthError {}
      -> [SuggestIncreaseReductionDepth]
    TcRnRedundantConstraints{}
      -> noHints
    TcRnInaccessibleCode{}
      -> noHints
    TcRnInaccessibleCoAxBranch{}
      -> noHints
    TcRnTypeDoesNotHaveFixedRuntimeRep{}
      -> noHints
    TcRnImplicitLift{}
      -> noHints
    TcRnUnusedPatternBinds{}
      -> noHints
    TcRnDodgyImports{}
      -> noHints
    TcRnDodgyExports{}
      -> noHints
    TcRnMissingImportList{}
      -> noHints
    TcRnUnsafeDueToPlugin{}
      -> noHints
    TcRnModMissingRealSrcSpan{}
      -> noHints
    TcRnIdNotExportedFromModuleSig name mod
      -> [SuggestAddToHSigExportList name $ Just mod]
    TcRnIdNotExportedFromLocalSig name
      -> [SuggestAddToHSigExportList name Nothing]
    TcRnShadowedName{}
      -> noHints
    TcRnInvalidWarningCategory{}
      -> noHints
    TcRnDuplicateWarningDecls{}
      -> noHints
    TcRnSimplifierTooManyIterations{}
      -> [SuggestIncreaseSimplifierIterations]
    TcRnIllegalPatSynDecl{}
      -> noHints
    TcRnLinearPatSyn{}
      -> noHints
    TcRnEmptyRecordUpdate{}
      -> noHints
    TcRnIllegalFieldPunning{}
      -> [suggestExtension LangExt.NamedFieldPuns]
    TcRnIllegalWildcardsInRecord{}
      -> [suggestExtension LangExt.RecordWildCards]
    TcRnIllegalWildcardInType{}
      -> noHints
    TcRnIllegalNamedWildcardInTypeArgument{}
      -> [SuggestAnonymousWildcard]
    TcRnIllegalImplicitTyVarInTypeArgument tv
      -> [SuggestExplicitQuantification tv]
    TcRnDuplicateFieldName{}
      -> noHints
    TcRnIllegalViewPattern{}
      -> [suggestExtension LangExt.ViewPatterns]
    TcRnCharLiteralOutOfRange{}
      -> noHints
    TcRnIllegalWildcardsInConstructor{}
      -> noHints
    TcRnIgnoringAnnotations{}
      -> noHints
    TcRnAnnotationInSafeHaskell
      -> noHints
    TcRnInvalidTypeApplication{}
      -> noHints
    TcRnTagToEnumMissingValArg
      -> noHints
    TcRnTagToEnumUnspecifiedResTy{}
      -> noHints
    TcRnTagToEnumResTyNotAnEnum{}
      -> noHints
    TcRnTagToEnumResTyTypeData{}
      -> noHints
    TcRnArrowIfThenElsePredDependsOnResultTy
      -> noHints
    TcRnIllegalHsBootOrSigDecl {}
      -> noHints
    TcRnBootMismatch boot_or_sig err
      | Hsig <- boot_or_sig
      , BootMismatch _ _ (BootMismatchedTyCons _boot_tc real_tc tc_errs) <- err
      , any is_synAbsData_etaReduce (NE.toList tc_errs)
      -> [SuggestEtaReduceAbsDataTySyn real_tc]
      | otherwise
      -> noHints
      where
        is_synAbsData_etaReduce (SynAbstractData SynAbsDataTySynNotNullary) = True
        is_synAbsData_etaReduce _ = False
    TcRnRecursivePatternSynonym{}
      -> noHints
    TcRnPartialTypeSigTyVarMismatch{}
      -> noHints
    TcRnPartialTypeSigBadQuantifier{}
      -> noHints
    TcRnMissingSignature {}
      -> noHints
    TcRnPolymorphicBinderMissingSig{}
      -> noHints
    TcRnOverloadedSig{}
      -> noHints
    TcRnTupleConstraintInst{}
      -> noHints
    TcRnUserTypeError{}
      -> noHints
    TcRnConstraintInKind{}
      -> noHints
    TcRnUnboxedTupleOrSumTypeFuncArg tuple_or_sum _
      -> [suggestExtension $ unboxedTupleOrSumExtension tuple_or_sum]
    TcRnLinearFuncInKind{}
      -> noHints
    TcRnForAllEscapeError{}
      -> noHints
    TcRnVDQInTermType mb_ty
      | isJust mb_ty -> [suggestExtension LangExt.RequiredTypeArguments]
      | otherwise    -> []
    TcRnBadQuantPredHead{}
      -> noHints
    TcRnIllegalTupleConstraint{}
      -> [suggestExtension LangExt.ConstraintKinds]
    TcRnNonTypeVarArgInConstraint{}
      -> [suggestExtension LangExt.FlexibleContexts]
    TcRnIllegalImplicitParam{}
      -> noHints
    TcRnIllegalConstraintSynonymOfKind{}
      -> [suggestExtension LangExt.ConstraintKinds]
    TcRnOversaturatedVisibleKindArg{}
      -> noHints
    TcRnForAllRankErr rank _
      -> case rank of
           LimitedRank{}      -> [suggestExtension LangExt.RankNTypes]
           MonoTypeRankZero   -> [suggestExtension LangExt.RankNTypes]
           MonoTypeTyConArg   -> [suggestExtension LangExt.ImpredicativeTypes]
           MonoTypeSynArg     -> [suggestExtension LangExt.LiberalTypeSynonyms]
           MonoTypeConstraint -> [suggestExtension LangExt.QuantifiedConstraints]
           _                  -> noHints
    TcRnSimplifiableConstraint{}
      -> noHints
    TcRnArityMismatch{}
      -> noHints
    TcRnIllegalInstance rea
      -> illegalInstanceHints rea
    TcRnMonomorphicBindings bindings
      -> case bindings of
          []     -> noHints
          (x:xs) -> [SuggestAddTypeSignatures $ NamedBindings (x NE.:| xs)]
    TcRnOrphanInstance clsOrFamInst
      -> [SuggestFixOrphanInst { isFamilyInstance = isFam }]
        where
          isFam = case clsOrFamInst :: Either ClsInst FamInst of
            Left  _clsInst -> Nothing
            Right famInst  -> Just $ fi_flavor famInst
    TcRnFunDepConflict{}
      -> noHints
    TcRnDupInstanceDecls{}
      -> noHints
    TcRnConflictingFamInstDecls{}
      -> noHints
    TcRnFamInstNotInjective rea _ _
      -> case rea of
           InjErrRhsBareTyVar{}      -> noHints
           InjErrRhsCannotBeATypeFam -> noHints
           InjErrRhsOverlap          -> noHints
           InjErrCannotInferFromRhs _ _ suggestUndInst
             | YesSuggestUndecidableInstaces <- suggestUndInst
             -> [suggestExtension LangExt.UndecidableInstances]
             | otherwise
             -> noHints
    TcRnBangOnUnliftedType{}
      -> noHints
    TcRnLazyBangOnUnliftedType{}
      -> noHints
    TcRnMultipleDefaultDeclarations{}
      -> noHints
    TcRnBadDefaultType{}
      -> noHints
    TcRnPatSynBundledWithNonDataCon{}
      -> noHints
    TcRnPatSynBundledWithWrongType{}
      -> noHints
    TcRnDupeModuleExport{}
      -> noHints
    TcRnExportedModNotImported{}
      -> noHints
    TcRnNullExportedModule{}
      -> noHints
    TcRnMissingExportList{}
      -> noHints
    TcRnExportHiddenComponents{}
      -> noHints
    TcRnExportHiddenDefault{}
      -> noHints
    TcRnDuplicateExport{}
      -> noHints
    TcRnDuplicateNamedDefaultExport{}
      -> noHints
    TcRnExportedParentChildMismatch{}
      -> noHints
    TcRnConflictingExports{}
      -> noHints
    TcRnDuplicateFieldExport {}
      -> [suggestExtension LangExt.DuplicateRecordFields]
    TcRnAmbiguousFieldInUpdate {}
      -> [suggestExtension LangExt.DisambiguateRecordFields]
    TcRnAmbiguousRecordUpdate{}
      -> noHints
    TcRnMissingFields{}
      -> noHints
    TcRnFieldUpdateInvalidType{}
      -> noHints
    TcRnMissingStrictFields{}
      -> noHints
    TcRnBadRecordUpdate{}
      -> noHints
    TcRnIllegalStaticExpression {}
      -> [suggestExtension LangExt.StaticPointers]
    TcRnStaticFormNotClosed{}
      -> noHints
    TcRnUselessTypeable
      -> noHints
    TcRnDerivingDefaults{}
      -> [useDerivingStrategies]
    TcRnNonUnaryTypeclassConstraint{}
      -> noHints
    TcRnPartialTypeSignatures suggestParSig _
      -> case suggestParSig of
           YesSuggestPartialTypeSignatures
             -> let info = text "to use the inferred type"
                in [suggestExtensionWithInfo info LangExt.PartialTypeSignatures]
           NoSuggestPartialTypeSignatures
             -> noHints
    TcRnCannotDeriveInstance cls _ _ newtype_deriving rea
      -> deriveInstanceErrReasonHints cls newtype_deriving rea
    TcRnLookupInstance _ _ _
      -> noHints
    TcRnLazyGADTPattern
      -> noHints
    TcRnArrowProcGADTPattern
      -> noHints
    TcRnTypeEqualityOutOfScope
      -> noHints
    TcRnTypeEqualityRequiresOperators
      -> [suggestExtension LangExt.TypeOperators]
    TcRnIllegalTypeOperator {}
      -> [suggestExtension LangExt.TypeOperators]
    TcRnIllegalTypeOperatorDecl {}
      -> [suggestExtension LangExt.TypeOperators]
    TcRnGADTMonoLocalBinds {}
      -> [suggestAnyExtension [LangExt.GADTs, LangExt.TypeFamilies]]
    TcRnIncorrectNameSpace nm is_th_use
      | is_th_use
      -> [SuggestAppropriateTHTick (nameNameSpace nm)]
      | otherwise
      -> noHints
    TcRnNotInScope err _nm
      -> scopeErrorHints err
    TcRnTermNameInType {}
      -> noHints
    TcRnUntickedPromotedThing thing
      -> [SuggestAddTick thing]
    TcRnIllegalBuiltinSyntax {}
      -> noHints
    TcRnWarnDefaulting {}
      -> noHints
    TcRnWarnClashingDefaultImports cls local imports
      -> suggestDefaultDeclaration cls (fold local) (cd_types <$> NE.toList imports)
    TcRnForeignImportPrimExtNotSet{}
      -> [suggestExtension LangExt.GHCForeignImportPrim]
    TcRnForeignImportPrimSafeAnn{}
      -> noHints
    TcRnForeignFunctionImportAsValue{}
      -> noHints
    TcRnFunPtrImportWithoutAmpersand{}
      -> noHints
    TcRnIllegalForeignDeclBackend{}
      -> noHints
    TcRnUnsupportedCallConv{}
      -> noHints
    TcRnIllegalForeignType _ reason
      -> case reason of
           TypeCannotBeMarshaled _ why
             | NewtypeDataConNotInScope tc _ <- why
             -> let tc_nm = tyConName tc
                    dc = dataConName $ head $ tyConDataCons tc
                in [ ImportSuggestion (occName dc)
                   $ ImportDataCon Nothing (nameOccName tc_nm) ]
             | UnliftedFFITypesNeeded <- why
             -> [suggestExtension LangExt.UnliftedFFITypes]
           _ -> noHints
    TcRnInvalidCIdentifier{}
      -> noHints
    TcRnExpectedValueId{}
      -> noHints
    TcRnRecSelectorEscapedTyVar{}
      -> [SuggestPatternMatchingSyntax]
    TcRnPatSynNotBidirectional{}
      -> noHints
    TcRnIllegalDerivingItem{}
      -> noHints
    TcRnIllegalDefaultClass{}
      -> noHints
    TcRnIllegalNamedDefault{}
      -> [suggestExtension LangExt.NamedDefaults]
    TcRnUnexpectedAnnotation{}
      -> noHints
    TcRnIllegalRecordSyntax{}
      -> noHints
    TcRnInvalidVisibleKindArgument{}
      -> noHints
    TcRnTooManyBinders{}
      -> noHints
    TcRnDifferentNamesForTyVar{}
      -> noHints
    TcRnDisconnectedTyVar n
      -> [SuggestBindTyVarExplicitly n]
    TcRnInvalidReturnKind _ _ _ mb_suggest_unlifted_ext
      -> case mb_suggest_unlifted_ext of
           Nothing -> noHints
           Just SuggestUnliftedNewtypes -> [suggestExtension LangExt.UnliftedNewtypes]
           Just SuggestUnliftedDatatypes -> [suggestExtension LangExt.UnliftedDatatypes]
    TcRnClassKindNotConstraint{}
      -> noHints
    TcRnUnpromotableThing{}
      -> noHints
    TcRnIllegalTermLevelUse {}
      -> noHints
    TcRnMatchesHaveDiffNumArgs{}
      -> noHints
    TcRnCannotBindScopedTyVarInPatSig{}
      -> noHints
    TcRnCannotBindTyVarsInPatBind{}
      -> noHints
    TcRnTooManyTyArgsInConPattern{}
      -> noHints
    TcRnMultipleInlinePragmas{}
      -> noHints
    TcRnUnexpectedPragmas{}
      -> noHints
    TcRnNonOverloadedSpecialisePragma{}
      -> noHints
    TcRnSpecialiseNotVisible name
      -> [SuggestSpecialiseVisibilityHints name]
    TcRnPragmaWarning{}
      -> noHints
    TcRnDifferentExportWarnings _ _
      -> noHints
    TcRnIncompleteExportWarnings _ _
      -> noHints
    TcRnIllegalHsigDefaultMethods{}
      -> noHints
    TcRnIllegalQuasiQuotes{}
      -> [suggestExtension LangExt.QuasiQuotes]
    TcRnTHError err
      -> thErrorHints err
    TcRnHsigFixityMismatch{}
      -> noHints
    TcRnHsigShapeMismatch{}
      -> noHints
    TcRnHsigMissingModuleExport{}
      -> noHints
    TcRnBadGenericMethod{}
      -> noHints
    TcRnWarningMinimalDefIncomplete{}
      -> noHints
    TcRnDefaultMethodForPragmaLacksBinding{}
      -> noHints
    TcRnIgnoreSpecialisePragmaOnDefMethod{}
      -> noHints
    TcRnBadMethodErr{}
      -> noHints
    TcRnIllegalTypeData
      -> [suggestExtension LangExt.TypeData]
    TcRnTypeDataForbids{}
      -> noHints
    TcRnIllegalNewtype{}
      -> noHints
    TcRnOrPatBindsVariables{}
      -> noHints
    TcRnUnsatisfiedMinimalDef{}
      -> noHints
    TcRnMisplacedInstSig{}
      -> [suggestExtension LangExt.InstanceSigs]
    TcRnNoRebindableSyntaxRecordDot{}
      -> noHints
    TcRnNoFieldPunsRecordDot{}
      -> noHints
    TcRnListComprehensionDuplicateBinding{}
      -> noHints
    TcRnEmptyStmtsGroup EmptyStmtsGroupInDoNotation{}
      -> [suggestExtension LangExt.NondecreasingIndentation]
    TcRnEmptyStmtsGroup{}
      -> noHints
    TcRnLastStmtNotExpr{}
      -> noHints
    TcRnUnexpectedStatementInContext _ _ mExt
      | Nothing <- mExt -> noHints
      | Just ext <- mExt -> [suggestExtension ext]
    TcRnSectionWithoutParentheses{}
      -> noHints
    TcRnIllegalImplicitParameterBindings{}
      -> noHints
    TcRnIllegalTupleSection{}
      -> [suggestExtension LangExt.TupleSections]
    TcRnCapturedTermName{}
      -> [SuggestRenameTypeVariable]
    TcRnBindingOfExistingName{}
      -> noHints
    TcRnMultipleFixityDecls{}
      -> noHints
    TcRnIllegalPatternSynonymDecl{}
      -> [suggestExtension LangExt.PatternSynonyms]
    TcRnIllegalClassBinding{}
      -> noHints
    TcRnOrphanCompletePragma{}
      -> noHints
    TcRnEmptyCase _ reason ->
      case reason of
        EmptyCaseWithoutFlag{}    -> [suggestExtension LangExt.EmptyCase]
        EmptyCaseDisallowedCtxt{} -> noHints
        EmptyCaseForall{}         -> noHints
    TcRnSpecSigShape{}
      -> noHints
    TcRnNonStdGuards{}
      -> [suggestExtension LangExt.PatternGuards]
    TcRnDuplicateSigDecl{}
      -> noHints
    TcRnMisplacedSigDecl{}
      -> noHints
    TcRnUnexpectedDefaultSig{}
      -> [suggestExtension LangExt.DefaultSignatures]
    TcRnDuplicateMinimalSig{}
      -> noHints
    TcRnUnexpectedStandaloneDerivingDecl{}
      -> [suggestExtension LangExt.StandaloneDeriving]
    TcRnUnusedVariableInRuleDecl{}
      -> noHints
    TcRnUnexpectedStandaloneKindSig{}
      -> [suggestExtension LangExt.StandaloneKindSignatures]
    TcRnIllegalRuleLhs{}
      -> noHints
    TcRnRuleLhsEqualities{}
      -> noHints
    TcRnDuplicateRoleAnnot{}
      -> noHints
    TcRnDuplicateKindSig{}
      -> noHints
    TcRnIllegalDerivStrategy ds -> case ds of
      ViaStrategy{} -> [suggestExtension LangExt.DerivingVia]
      _ -> [suggestExtension LangExt.DerivingStrategies]
    TcRnIllegalMultipleDerivClauses{}
      -> [suggestExtension LangExt.DerivingStrategies]
    TcRnNoDerivStratSpecified is_ds_enabled info -> do
      let explicit_strategy_hint = case info of
            TcRnNoDerivingClauseStrategySpecified assumed_derivings ->
              SuggestExplicitDerivingClauseStrategies assumed_derivings
            TcRnNoStandaloneDerivingStrategySpecified assumed_strategy deriv_sig ->
              SuggestExplicitStandaloneDerivingStrategy assumed_strategy deriv_sig
      explicit_strategy_hint : [suggestExtension LangExt.DerivingStrategies | not is_ds_enabled]
    TcRnStupidThetaInGadt{}
      -> noHints
    TcRnShadowedTyVarNameInFamResult{}
      -> noHints
    TcRnIncorrectTyVarOnLhsOfInjCond{}
      -> noHints
    TcRnUnknownTyVarsOnRhsOfInjCond{}
      -> noHints
    TcRnBadlyStaged{}
      -> noHints
    TcRnBadlyStagedType{}
      -> noHints
    TcRnStageRestriction{}
      -> noHints
    TcRnTyThingUsedWrong{}
      -> noHints
    TcRnCannotDefaultKindVar{}
      -> noHints
    TcRnUninferrableTyVar{}
      -> noHints
    TcRnSkolemEscape{}
      -> noHints
    TcRnPatSynEscapedCoercion{}
      -> noHints
    TcRnPatSynExistentialInResult{}
      -> noHints
    TcRnPatSynArityMismatch{}
      -> noHints
    TcRnPatSynInvalidRhs name pat args (PatSynNotInvertible _)
      -> [SuggestExplicitBidiPatSyn name pat args]
    TcRnPatSynInvalidRhs{}
      -> noHints
    TcRnTyFamDepsDisabled{}
      -> [suggestExtension LangExt.TypeFamilyDependencies]
    TcRnAbstractClosedTyFamDecl{}
      -> noHints
    TcRnPartialFieldSelector{}
      -> noHints
    TcRnHasFieldResolvedIncomplete{}
      -> noHints
    TcRnBadFieldAnnotation _ _ LazyFieldsDisabled
      -> [suggestExtension LangExt.StrictData]
    TcRnBadFieldAnnotation{}
      -> noHints
    TcRnSuperclassCycle{}
      -> [suggestExtension LangExt.UndecidableSuperClasses]
    TcRnDefaultSigMismatch{}
      -> noHints
    TcRnTyFamsDisabled{}
      -> [suggestExtension LangExt.TypeFamilies]
    TcRnBadTyConTelescope{}
      -> noHints
    TcRnTyFamResultDisabled{}
      -> [suggestExtension LangExt.TypeFamilyDependencies]
    TcRnRoleValidationFailed{}
      -> noHints
    TcRnCommonFieldResultTypeMismatch{}
      -> noHints
    TcRnCommonFieldTypeMismatch{}
      -> noHints
    TcRnClassExtensionDisabled _ MultiParamDisabled{}
      -> [suggestExtension LangExt.MultiParamTypeClasses]
    TcRnClassExtensionDisabled _ FunDepsDisabled{}
      -> [suggestExtension LangExt.FunctionalDependencies]
    TcRnClassExtensionDisabled _ ConstrainedClassMethodsDisabled{}
      -> [suggestExtension LangExt.ConstrainedClassMethods]
    TcRnDataConParentTypeMismatch{}
      -> noHints
    TcRnGADTsDisabled{}
      -> [suggestExtension LangExt.GADTs]
    TcRnExistentialQuantificationDisabled{}
      -> [suggestAnyExtension [LangExt.ExistentialQuantification, LangExt.GADTs]]
    TcRnGADTDataContext{}
      -> noHints
    TcRnMultipleConForNewtype{}
      -> noHints
    TcRnKindSignaturesDisabled{}
      -> [suggestExtension LangExt.KindSignatures]
    TcRnEmptyDataDeclsDisabled{}
      -> [suggestExtension LangExt.EmptyDataDecls]
    TcRnRoleMismatch{}
      -> noHints
    TcRnRoleCountMismatch{}
      -> noHints
    TcRnIllegalRoleAnnotation{}
      -> noHints
    TcRnRoleAnnotationsDisabled{}
      -> [suggestExtension LangExt.RoleAnnotations]
    TcRnIncoherentRoles{}
      -> [suggestExtension LangExt.IncoherentInstances]
    TcRnUnexpectedKindVar{}
      -> [suggestExtension LangExt.PolyKinds]
    TcRnNegativeNumTypeLiteral{}
      -> noHints
    TcRnIllegalKind _ suggest_polyKinds
      -> if suggest_polyKinds
         then [suggestExtension LangExt.PolyKinds]
         else noHints
    TcRnPrecedenceParsingError{}
      -> noHints
    TcRnSectionPrecedenceError{}
      -> noHints
    TcRnUnexpectedPatSigType{}
      -> [suggestExtension LangExt.ScopedTypeVariables]
    TcRnIllegalKindSignature{}
      -> [suggestExtension LangExt.KindSignatures]
    TcRnUnusedQuantifiedTypeVar{}
      -> noHints
    TcRnDataKindsError{}
      -> [suggestExtension LangExt.DataKinds]
    TcRnTypeSynonymCycle{}
      -> noHints
    TcRnZonkerMessage msg
      -> zonkerMessageHints msg
    TcRnInterfaceError reason
      -> interfaceErrorHints reason
    TcRnSelfImport{}
      -> noHints
    TcRnNoExplicitImportList{}
      -> noHints
    TcRnSafeImportsDisabled{}
      -> [SuggestSafeHaskell]
    TcRnDeprecatedModule{}
      -> noHints
    TcRnRedundantSourceImport{}
      -> noHints
    TcRnImportLookup (ImportLookupBad k _ is ie patsyns_enabled) ->
      let mod_name = moduleName $ is_mod is
          occ = rdrNameOcc $ ieName ie
      in case k of
        BadImportAvailVar          -> [ImportSuggestion occ $ CouldRemoveTypeKeyword mod_name]
        BadImportNotExported suggs -> suggs
        BadImportAvailTyCon ex_ns  ->
          [useExtensionInOrderTo empty LangExt.ExplicitNamespaces | not ex_ns]
          ++ [ImportSuggestion occ $ CouldAddTypeKeyword mod_name]
        BadImportAvailDataCon par  -> [ImportSuggestion occ $ ImportDataCon (Just (mod_name, patsyns_enabled)) par]
        BadImportNotExportedSubordinates{} -> noHints
    TcRnImportLookup{}
      -> noHints
    TcRnUnusedImport{}
      -> noHints
    TcRnDuplicateDecls _ fs
      -> [suggestExtension LangExt.DuplicateRecordFields | all isFieldName fs]
    TcRnPackageImportsDisabled
      -> [suggestExtension LangExt.PackageImports]
    TcRnIllegalDataCon{}
      -> noHints
    TcRnNestedForallsContexts{}
      -> noHints
    TcRnRedundantRecordWildcard
      -> [SuggestRemoveRecordWildcard]
    TcRnUnusedRecordWildcard{}
      -> [SuggestRemoveRecordWildcard]
    TcRnUnusedName{}
      -> noHints
    TcRnQualifiedBinder{}
      -> noHints
    TcRnTypeApplicationsDisabled{}
      -> [suggestExtension LangExt.TypeApplications]
    TcRnInvalidRecordField{}
      -> noHints
    TcRnTupleTooLarge{}
      -> noHints
    TcRnCTupleTooLarge{}
      -> noHints
    TcRnIllegalInferredTyVars{}
      -> noHints
    TcRnAmbiguousName{}
      -> noHints
    TcRnBindingNameConflict{}
      -> noHints
    TcRnNonCanonicalDefinition reason _
      -> suggestNonCanonicalDefinition reason
    TcRnDefaultedExceptionContext _
      -> noHints
    TcRnImplicitImportOfPrelude {}
      -> noHints
    TcRnMissingMain {}
      -> noHints
    TcRnGhciUnliftedBind {}
      -> noHints
    TcRnGhciMonadLookupFail {}
      -> noHints
    TcRnMissingRoleAnnotation{}
      -> noHints
    TcRnIllegalInvisTyVarBndr{}
      -> [suggestExtension LangExt.TypeAbstractions]
    TcRnIllegalWildcardTyVarBndr{}
      -> [suggestExtension LangExt.TypeAbstractions]
    TcRnInvalidInvisTyVarBndr{}
      -> noHints
    TcRnInvisBndrWithoutSig name _
      -> [SuggestAddStandaloneKindSignature name]
    TcRnImplicitRhsQuantification kv
      -> [SuggestBindTyVarOnLhs (unLoc kv)]
    TcRnPatersonCondFailure{}
      -> [suggestExtension LangExt.UndecidableInstances]
    TcRnIllformedTypePattern{}
      -> noHints
    TcRnIllegalTypePattern{}
      -> noHints
    TcRnIllformedTypeArgument{}
      -> noHints
    TcRnIllegalTypeExpr{}
      -> noHints
    TcRnInvalidDefaultedTyVar{}
      -> noHints
    TcRnNamespacedWarningPragmaWithoutFlag{}
      -> [suggestExtension LangExt.ExplicitNamespaces]
    TcRnIllegalInvisibleTypePattern _ reason
      -> case reason of
          InvisPatWithoutFlag -> [suggestExtension LangExt.TypeAbstractions]
          InvisPatNoForall    -> noHints
          InvisPatMisplaced   -> noHints
    TcRnNamespacedFixitySigWithoutFlag{}
      -> [suggestExtension LangExt.ExplicitNamespaces]
    TcRnOutOfArityTyVar{}
      -> noHints
    TcRnUnexpectedTypeSyntaxInTerms syntax
      -> [suggestExtension (typeSyntaxExtension syntax)]

  diagnosticCode = constructorCode @GHC


note :: SDoc -> SDoc
note note = "Note" <> colon <+> note <> dot

-- | Change [x] to "x", [x, y] to "x and y", [x, y, z] to "x, y, and z",
-- and so on.  The `and` stands for any `conjunction`, which is passed in.
commafyWith :: SDoc -> [SDoc] -> [SDoc]
commafyWith _ [] = []
commafyWith _ [x] = [x]
commafyWith conjunction [x, y] = [x <+> conjunction <+> y]
commafyWith conjunction xs = addConjunction $ punctuate comma xs
    where addConjunction [x, y] = [x, conjunction, y]
          addConjunction (x : xs) = x : addConjunction xs
          addConjunction _ = panic "commafyWith expected 2 or more elements"

deriveInstanceErrReasonHints :: Class
                             -> UsingGeneralizedNewtypeDeriving
                             -> DeriveInstanceErrReason
                             -> [GhcHint]
deriveInstanceErrReasonHints cls newtype_deriving = \case
  DerivErrNotWellKinded _ _ n_args_to_keep
    | cls `hasKey` gen1ClassKey && n_args_to_keep >= 0
    -> [suggestExtension LangExt.PolyKinds]
    | otherwise
    -> noHints
  DerivErrSafeHaskellGenericInst  -> noHints
  DerivErrDerivingViaWrongKind{}  -> noHints
  DerivErrNoEtaReduce{}           -> noHints
  DerivErrBootFileFound           -> noHints
  DerivErrDataConsNotAllInScope{} -> noHints
  DerivErrGNDUsedOnData           -> noHints
  DerivErrNullaryClasses          -> noHints
  DerivErrLastArgMustBeApp        -> noHints
  DerivErrNoFamilyInstance{}      -> noHints
  DerivErrNotStockDeriveable deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrHasAssociatedDatatypes{}
    -> noHints
  DerivErrNewtypeNonDeriveableClass
    | newtype_deriving == NoGeneralizedNewtypeDeriving
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrCannotEtaReduceEnough{}
    | newtype_deriving == NoGeneralizedNewtypeDeriving
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrOnlyAnyClassDeriveable _ deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrNotDeriveable deriveAnyClassEnabled
    | deriveAnyClassEnabled == NoDeriveAnyClassEnabled
    -> [suggestExtension LangExt.DeriveAnyClass]
    | otherwise
    -> noHints
  DerivErrNotAClass{}
    -> noHints
  DerivErrNoConstructors{}
    -> let info = text "to enable deriving for empty data types"
       in [useExtensionInOrderTo info LangExt.EmptyDataDeriving]
  DerivErrLangExtRequired{}
    -- This is a slightly weird corner case of GHC: we are failing
    -- to derive a typeclass instance because a particular 'Extension'
    -- is not enabled (and so we report in the main error), but here
    -- we don't want to /repeat/ to enable the extension in the hint.
    -> noHints
  DerivErrDunnoHowToDeriveForType{}
    -> noHints
  DerivErrMustBeEnumType rep_tc
    -- We want to suggest GND only if this /is/ a newtype.
    | newtype_deriving == NoGeneralizedNewtypeDeriving && isNewTyCon rep_tc
    -> [useGND]
    | otherwise
    -> noHints
  DerivErrMustHaveExactlyOneConstructor{}
    -> noHints
  DerivErrMustHaveSomeParameters{}
    -> noHints
  DerivErrMustNotHaveClassContext{}
    -> noHints
  DerivErrBadConstructor wcard _
    -> case wcard of
         Nothing        -> noHints
         Just YesHasWildcard -> [SuggestFillInWildcardConstraint]
         Just NoHasWildcard  -> [SuggestAddStandaloneDerivation]
  DerivErrGenerics{}
    -> noHints
  DerivErrEnumOrProduct{}
    -> noHints

--------------------------------------------------------------------------------

-- | Pretty-print supplementary information, to add to an error report.
pprSolverReportSupplementary :: HoleFitDispConfig -> SupplementaryInfo -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprSolverReportSupplementary hfdc = \case
  SupplementaryBindings     binds -> pprRelevantBindings binds
  SupplementaryHoleFits     fits  -> pprValidHoleFits hfdc fits
  SupplementaryCts          cts   -> pprConstraintsInclude cts
  SupplementaryImportErrors errs  -> vcat (map ppr $ NE.toList errs)

-- | Display a collection of valid hole fits.
pprValidHoleFits :: HoleFitDispConfig -> ValidHoleFits -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprValidHoleFits hfdc (ValidHoleFits (Fits fits discarded_fits) (Fits refs discarded_refs))
  = fits_msg $$ refs_msg

  where
    fits_msg, refs_msg, fits_discard_msg, refs_discard_msg :: SDoc
    fits_msg = ppUnless (null fits) $
                    hang (text "Valid hole fits include") 2 $
                    vcat (map (pprHoleFit hfdc) fits)
                      $$ ppWhen  discarded_fits fits_discard_msg
    refs_msg = ppUnless (null refs) $
                  hang (text "Valid refinement hole fits include") 2 $
                  vcat (map (pprHoleFit hfdc) refs)
                    $$ ppWhen discarded_refs refs_discard_msg
    fits_discard_msg =
      text "(Some hole fits suppressed;" <+>
      text "use -fmax-valid-hole-fits=N" <+>
      text "or -fno-max-valid-hole-fits)"
    refs_discard_msg =
      text "(Some refinement hole fits suppressed;" <+>
      text "use -fmax-refinement-hole-fits=N" <+>
      text "or -fno-max-refinement-hole-fits)"

-- For pretty printing hole fits, we display the name and type of the fit,
-- with added '_' to represent any extra arguments in case of a non-zero
-- refinement level.
pprHoleFit :: HoleFitDispConfig -> HoleFit -> SDoc
pprHoleFit _ (RawHoleFit sd) = sd
pprHoleFit (HFDC sWrp sWrpVars sTy sProv sMs) (TcHoleFit (HoleFit {..})) =
 hang display 2 provenance
 where tyApp = sep $ zipWithEqual pprArg vars hfWrap
         where pprArg b arg = case binderFlag b of
                                Specified -> text "@" <> pprParendType arg
                                  -- Do not print type application for inferred
                                  -- variables (#16456)
                                Inferred  -> empty
                                Required  -> pprPanic "pprHoleFit: bad Required"
                                                         (ppr b <+> ppr arg)
       tyAppVars = sep $ punctuate comma $
           zipWithEqual (\v t -> ppr (binderVar v) <+> text "~" <+> pprParendType t)
           vars hfWrap

       vars = unwrapTypeVars hfType
         where
           -- Attempts to get all the quantified type variables in a type,
           -- e.g.
           -- return :: forall (m :: * -> *) Monad m => (forall a . a -> m a)
           -- into [m, a]
           unwrapTypeVars :: Type -> [ForAllTyBinder]
           unwrapTypeVars t = vars ++ case splitFunTy_maybe unforalled of
                               Just (_, _, _, unfunned) -> unwrapTypeVars unfunned
                               _ -> []
             where (vars, unforalled) = splitForAllForAllTyBinders t
       holeVs = sep $ map (parens . (text "_" <+> dcolon <+>) . ppr) hfMatches
       holeDisp = if sMs then holeVs
                  else sep $ replicate (length hfMatches) $ text "_"
       occDisp = case hfCand of
                   GreHFCand gre   -> pprPrefixOcc (greName gre)
                   NameHFCand name -> pprPrefixOcc name
                   IdHFCand id_    -> pprPrefixOcc id_
       tyDisp = ppWhen sTy $ dcolon <+> ppr hfType
       has = not . null
       wrapDisp = ppWhen (has hfWrap && (sWrp || sWrpVars))
                   $ text "with" <+> if sWrp || not sTy
                                     then occDisp <+> tyApp
                                     else tyAppVars
       docs = case hfDoc of
                Just d -> pprHsDocStrings d
                _ -> empty
       funcInfo = ppWhen (has hfMatches && sTy) $
                    text "where" <+> occDisp <+> tyDisp
       subDisp = occDisp <+> if has hfMatches then holeDisp else tyDisp
       display =  subDisp $$ nest 2 (funcInfo $+$ docs $+$ wrapDisp)
       provenance = ppWhen sProv $ parens $
             case hfCand of
                 GreHFCand gre -> pprNameProvenance gre
                 NameHFCand name -> text "bound at" <+> ppr (getSrcLoc name)
                 IdHFCand id_ -> text "bound at" <+> ppr (getSrcLoc id_)

-- | Add a "Constraints include..." message.
--
-- See Note [Constraints include ...]
pprConstraintsInclude :: NE.NonEmpty (PredType, RealSrcSpan) -> SDoc
-- This function should be in "GHC.Tc.Errors.Ppr",
-- but we need it here because 'TcRnMessageDetails' needs an 'SDoc'.
pprConstraintsInclude cts
  = hang (text "Constraints include")
        2 (vcat $ map pprConstraint $ NE.toList cts)
  where
    pprConstraint (constraint, loc) =
      ppr constraint <+> nest 2 (parens (text "from" <+> ppr loc))

messageWithInfoDiagnosticMessage :: UnitState
                                 -> ErrInfo
                                 -> Bool
                                 -> DecoratedSDoc
                                 -> DecoratedSDoc
messageWithInfoDiagnosticMessage unit_state (ErrInfo {..}) show_ctxt important =
  let ctxt = pprWithUnitState unit_state
           $ vcat $ map pprErrCtxtMsg  [ ctx | ctx <- errInfoContext, show_ctxt ]

      supp = case errInfoSupplementary of
        Nothing -> empty
        Just (hfdc, supp_msgs) ->
          pprWithUnitState unit_state $
          vcat $ map (pprSolverReportSupplementary hfdc) supp_msgs
  in mapDecoratedSDoc (pprWithUnitState unit_state) important
       `unionDecoratedSDoc`
     mkDecorated [ctxt, supp]

messageWithHsDocContext :: TcRnMessageOpts -> HsDocContext -> DecoratedSDoc -> DecoratedSDoc
messageWithHsDocContext opts ctxt main_msg = do
      if tcOptsShowContext opts
         then main_msg `unionDecoratedSDoc` ctxt_msg
         else main_msg
      where
        ctxt_msg = mkSimpleDecorated (inHsDocContext ctxt)

--------------------------------------------------------------------------------

dodgy_msg :: Outputable ie => SDoc -> GlobalRdrElt -> ie -> SDoc
dodgy_msg kind tc ie
  = vcat [ text "The" <+> kind <+> text "item" <+> quotes (ppr ie) <+> text "suggests that"
         , quotes (ppr $ greName tc) <+> text "has" <+> sep rest ]
  where
    rest :: [SDoc]
    rest =
      case greInfo tc of
        IAmTyCon ClassFlavour
          -> [ text "(in-scope) class methods or associated types" <> comma
             , text "but it has none" ]
        IAmTyCon _
          -> [ text "(in-scope) constructors or record fields" <> comma
             , text "but it has none" ]
        _ -> [ text "children" <> comma
             , text "but it is not a type constructor or a class" ]

dodgy_msg_insert :: GlobalRdrElt -> IE GhcRn
dodgy_msg_insert tc_gre = IEThingAll (Nothing, noAnn) ii Nothing
  where
    ii = noLocA (IEName noExtField $ noLocA $ greName tc_gre)

pprTypeDoesNotHaveFixedRuntimeRep :: Type -> FixedRuntimeRepProvenance -> SDoc
pprTypeDoesNotHaveFixedRuntimeRep ty prov =
  let what = pprFixedRuntimeRepProvenance prov
  in text "The" <+> what <+> text "does not have a fixed runtime representation:"
  $$ format_frr_err ty

format_frr_err :: Type  -- ^ the type which doesn't have a fixed runtime representation
                -> SDoc
format_frr_err ty
  = (bullet <+> ppr tidy_ty <+> dcolon <+> ppr tidy_ki)
  where
    (tidy_env, tidy_ty) = tidyOpenTypeX emptyTidyEnv ty
    tidy_ki             = tidyType tidy_env (typeKind ty)

pprField :: (FieldLabelString, TcType) -> SDoc
pprField (f,ty) = ppr f <+> dcolon <+> ppr ty

pprRecordFieldPart :: RecordFieldPart -> SDoc
pprRecordFieldPart = \case
  RecordFieldDecl {}       -> text "declaration"
  RecordFieldConstructor{} -> text "construction"
  RecordFieldPattern{}     -> text "pattern"
  RecordFieldUpdate        -> text "update"

ppr_opfix :: (OpName, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | NegateOp <- op = text "prefix `-'"
           | otherwise      = quotes (ppr op)

pprBindings :: [Name] -> SDoc
pprBindings = pprWithCommas (quotes . ppr)


injectivityErrorHerald :: SDoc
injectivityErrorHerald =
  text "Type family equation violates the family's injectivity annotation."

formatExportItemError :: SDoc -> String -> SDoc
formatExportItemError exportedThing reason =
  hsep [ text "The export item"
       , quotes exportedThing
       , text reason ]

-- | What warning flags are associated with the given missing signature?
missingSignatureWarningFlags :: MissingSignature -> Exported -> NonEmpty WarningFlag
missingSignatureWarningFlags (MissingTopLevelBindingSig {}) exported
  -- We prefer "bigger" warnings first: #14794
  --
  -- See Note [Warnings controlled by multiple flags]
  = Opt_WarnMissingSignatures :|
    [ Opt_WarnMissingExportedSignatures | IsExported == exported ]
missingSignatureWarningFlags (MissingPatSynSig {}) exported
  = Opt_WarnMissingPatternSynonymSignatures :|
    [ Opt_WarnMissingExportedPatternSynonymSignatures | IsExported  == exported ]
missingSignatureWarningFlags (MissingTyConKindSig ty_con _) _
  = Opt_WarnMissingKindSignatures :| [Opt_WarnMissingPolyKindSignatures | isForAllTy_invis_ty (tyConKind ty_con) ]

useDerivingStrategies :: GhcHint
useDerivingStrategies =
  useExtensionInOrderTo (text "to pick a different strategy") LangExt.DerivingStrategies

useGND :: GhcHint
useGND = let info = text "for GHC's" <+> text "newtype-deriving extension"
         in suggestExtensionWithInfo info LangExt.GeneralizedNewtypeDeriving

cannotMakeDerivedInstanceHerald :: Class
                                -> [Type]
                                -> Maybe (DerivStrategy GhcTc)
                                -> UsingGeneralizedNewtypeDeriving
                                -> Bool -- ^ If False, only prints the why.
                                -> SDoc
                                -> SDoc
cannotMakeDerivedInstanceHerald cls cls_args mb_strat newtype_deriving pprHerald why =
  if pprHerald
     then sep [(hang (text "Can't make a derived instance of")
                   2 (quotes (ppr pred) <+> via_mechanism)
                $$ nest 2 extra) <> colon,
               nest 2 why]
      else why
  where
    strat_used = isJust mb_strat
    extra | not strat_used, (newtype_deriving == YesGeneralizedNewtypeDeriving)
          = text "(even with cunning GeneralizedNewtypeDeriving)"
          | otherwise = empty
    pred = mkClassPred cls cls_args
    via_mechanism | strat_used
                  , Just strat <- mb_strat
                  = text "with the" <+> (derivStrategyName strat) <+> text "strategy"
                  | otherwise
                  = empty

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = text "Constructor" <+> quotes (ppr con) <+> msg

derivErrDiagnosticMessage :: Class
                          -> [Type]
                          -> Maybe (DerivStrategy GhcTc)
                          -> UsingGeneralizedNewtypeDeriving
                          -> Bool -- If True, includes the herald \"can't make a derived..\"
                          -> DeriveInstanceErrReason
                          -> SDoc
derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving pprHerald = \case
  DerivErrNotWellKinded tc cls_kind _
    -> sep [ hang (text "Cannot derive well-kinded instance of form"
                         <+> quotes (pprClassPred cls cls_tys
                                       <+> parens (ppr tc <+> text "...")))
                  2 empty
           , nest 2 (text "Class" <+> quotes (ppr cls)
                         <+> text "expects an argument of kind"
                         <+> quotes (pprKind cls_kind))
           ]
  DerivErrSafeHaskellGenericInst
    ->     text "Generic instances can only be derived in"
       <+> text "Safe Haskell using the stock strategy."
  DerivErrDerivingViaWrongKind cls_kind via_ty via_kind
    -> hang (text "Cannot derive instance via" <+> quotes (pprType via_ty))
          2 (text "Class" <+> quotes (ppr cls)
                  <+> text "expects an argument of kind"
                  <+> quotes (pprKind cls_kind) <> char ','
         $+$ text "but" <+> quotes (pprType via_ty)
                  <+> text "has kind" <+> quotes (pprKind via_kind))
  DerivErrNoEtaReduce inst_ty
    -> sep [text "Cannot eta-reduce to an instance of form",
            nest 2 (text "instance (...) =>"
                   <+> pprClassPred cls (cls_tys ++ [inst_ty]))]
  DerivErrBootFileFound
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Cannot derive instances in hs-boot files"
          $+$ text "Write an instance declaration instead")
  DerivErrDataConsNotAllInScope tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (hang (text "The data constructors of" <+> quotes (ppr tc) <+> text "are not all in scope")
            2 (text "so you cannot derive an instance for it"))
  DerivErrGNDUsedOnData
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "GeneralizedNewtypeDeriving cannot be used on non-newtypes")
  DerivErrNullaryClasses
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Cannot derive instances for nullary classes")
  DerivErrLastArgMustBeApp
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         ( text "The last argument of the instance must be a"
         <+> text "data or newtype application")
  DerivErrNoFamilyInstance tc tc_args
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "No family instance for" <+> quotes (pprTypeApp tc tc_args))
  DerivErrNotStockDeriveable _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr cls) <+> text "is not a stock derivable class (Eq, Show, etc.)")
  DerivErrHasAssociatedDatatypes hasAdfs at_last_cls_tv_in_kinds at_without_last_cls_tv
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         $ vcat [ ppWhen (hasAdfs == YesHasAdfs) adfs_msg
               , case at_without_last_cls_tv of
                    YesAssociatedTyNotParamOverLastTyVar tc -> at_without_last_cls_tv_msg tc
                    NoAssociatedTyNotParamOverLastTyVar     -> empty
               , case at_last_cls_tv_in_kinds of
                   YesAssocTyLastVarInKind tc -> at_last_cls_tv_in_kinds_msg tc
                   NoAssocTyLastVarInKind     -> empty
               ]
       where

         adfs_msg  = text "the class has associated data types"

         at_without_last_cls_tv_msg at_tc = hang
           (text "the associated type" <+> quotes (ppr at_tc)
            <+> text "is not parameterized over the last type variable")
           2 (text "of the class" <+> quotes (ppr cls))

         at_last_cls_tv_in_kinds_msg at_tc = hang
           (text "the associated type" <+> quotes (ppr at_tc)
            <+> text "contains the last type variable")
          2 (text "of the class" <+> quotes (ppr cls)
            <+> text "in a kind, which is not (yet) allowed")
  DerivErrNewtypeNonDeriveableClass
    -> derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving pprHerald (DerivErrNotStockDeriveable NoDeriveAnyClassEnabled)
  DerivErrCannotEtaReduceEnough eta_ok
    -> let cant_derive_err = ppUnless eta_ok eta_msg
           eta_msg = text "cannot eta-reduce the representation type enough"
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
          cant_derive_err
  DerivErrOnlyAnyClassDeriveable tc _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr tc) <+> text "is a type class,"
                          <+> text "and can only have a derived instance"
                          $+$ text "if DeriveAnyClass is enabled")
  DerivErrNotDeriveable _
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald empty
  DerivErrNotAClass predType
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (ppr predType) <+> text "is not a class")
  DerivErrNoConstructors rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (pprSourceTyCon rep_tc) <+> text "must have at least one data constructor")
  DerivErrLangExtRequired ext
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "You need " <> ppr ext
            <+> text "to derive an instance for this class")
  DerivErrDunnoHowToDeriveForType ty
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
        (hang (text "Don't know how to derive" <+> quotes (ppr cls))
              2 (text "for type" <+> quotes (ppr ty)))
  DerivErrMustBeEnumType rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (sep [ quotes (pprSourceTyCon rep_tc) <+>
                text "must be an enumeration type"
              , text "(an enumeration consists of one or more nullary, non-GADT constructors)" ])

  DerivErrMustHaveExactlyOneConstructor rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (quotes (pprSourceTyCon rep_tc) <+> text "must have precisely one constructor")
  DerivErrMustHaveSomeParameters rep_tc
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Data type" <+> quotes (ppr rep_tc) <+> text "must have some type parameters")
  DerivErrMustNotHaveClassContext rep_tc bad_stupid_theta
    -> cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
         (text "Data type" <+> quotes (ppr rep_tc)
           <+> text "must not have a class context:" <+> pprTheta bad_stupid_theta)
  DerivErrBadConstructor _ reasons
    -> let why = vcat $ map renderReason reasons
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald why
         where
           renderReason = \case
                 DerivErrBadConExistential con
                   -> badCon con $ text "must be truly polymorphic in the last argument of the data type"
                 DerivErrBadConCovariant con
                   -> badCon con $ text "must not use the type variable in a function argument"
                 DerivErrBadConFunTypes con
                   -> badCon con $ text "must not contain function types"
                 DerivErrBadConWrongArg con
                   -> badCon con $ text "must use the type variable only as the last argument of a data type"
                 DerivErrBadConIsGADT con
                   -> badCon con $ text "is a GADT"
                 DerivErrBadConHasExistentials con
                   -> badCon con $ text "has existential type variables in its type"
                 DerivErrBadConHasConstraints con
                   -> badCon con $ text "has constraints in its type"
                 DerivErrBadConHasHigherRankType con
                   -> badCon con $ text "has a higher-rank type"
  DerivErrGenerics reasons
    -> let why = vcat $ map renderReason reasons
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald why
         where
           renderReason = \case
             DerivErrGenericsMustNotHaveDatatypeContext tc_name
                -> ppr tc_name <+> text "must not have a datatype context"
             DerivErrGenericsMustNotHaveExoticArgs dc
                -> ppr dc <+> text "must not have exotic unlifted or polymorphic arguments"
             DerivErrGenericsMustBeVanillaDataCon dc
                -> ppr dc <+> text "must be a vanilla data constructor"
             DerivErrGenericsMustHaveSomeTypeParams rep_tc
                ->     text "Data type" <+> quotes (ppr rep_tc)
                   <+> text "must have some type parameters"
             DerivErrGenericsMustNotHaveExistentials con
               -> badCon con $ text "must not have existential arguments"
             DerivErrGenericsWrongArgKind con
               -> badCon con $
                    text "applies a type to an argument involving the last parameter"
                 $$ text "but the applied type is not of kind * -> *"
  DerivErrEnumOrProduct this that
    -> let ppr1 = derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving False this
           ppr2 = derivErrDiagnosticMessage cls cls_tys mb_strat newtype_deriving False that
       in cannotMakeDerivedInstanceHerald cls cls_tys mb_strat newtype_deriving pprHerald
          (ppr1 $$ text "  or" $$ ppr2)

lookupInstanceErrDiagnosticMessage :: Class
                                   -> [Type]
                                   -> LookupInstanceErrReason
                                   -> SDoc
lookupInstanceErrDiagnosticMessage cls tys = \case
  LookupInstErrNotExact
    -> text "Not an exact match (i.e., some variables get instantiated)"
  LookupInstErrFlexiVar
    -> text "flexible type variable:" <+> (ppr $ mkTyConApp (classTyCon cls) tys)
  LookupInstErrNotFound
    -> text "instance not found" <+> (ppr $ mkTyConApp (classTyCon cls) tys)

{- *********************************************************************
*                                                                      *
              Outputable SolverReportErrCtxt (for debugging)
*                                                                      *
**********************************************************************-}

instance Outputable SolverReportErrCtxt where
  ppr (CEC { cec_binds              = bvar
           , cec_defer_type_errors  = dte
           , cec_expr_holes         = eh
           , cec_type_holes         = th
           , cec_out_of_scope_holes = osh
           , cec_warn_redundant     = wr
           , cec_expand_syns        = es
           , cec_suppress           = sup })
    = text "CEC" <+> braces (vcat
         [ text "cec_binds"              <+> equals <+> ppr bvar
         , text "cec_defer_type_errors"  <+> equals <+> ppr dte
         , text "cec_expr_holes"         <+> equals <+> ppr eh
         , text "cec_type_holes"         <+> equals <+> ppr th
         , text "cec_out_of_scope_holes" <+> equals <+> ppr osh
         , text "cec_warn_redundant"     <+> equals <+> ppr wr
         , text "cec_expand_syns"        <+> equals <+> ppr es
         , text "cec_suppress"           <+> equals <+> ppr sup ])

{- *********************************************************************
*                                                                      *
                    Outputting TcSolverReportMsg errors
*                                                                      *
**********************************************************************-}

-- | Pretty-print a 'SolverReportWithCtxt', containing a 'TcSolverReportMsg'
-- with its enclosing 'SolverReportErrCtxt'.
pprSolverReportWithCtxt :: SolverReportWithCtxt -> SDoc
pprSolverReportWithCtxt (SolverReportWithCtxt { reportContext = ctxt, reportContent = msg })
   = pprTcSolverReportMsg ctxt msg

-- | Pretty-print a 'TcSolverReportMsg', with its enclosing 'SolverReportErrCtxt'.
pprTcSolverReportMsg :: SolverReportErrCtxt -> TcSolverReportMsg -> SDoc
pprTcSolverReportMsg _ (BadTelescope telescope skols) =
  hang (text "These kind and type variables:" <+> ppr telescope $$
       text "are out of dependency order. Perhaps try this ordering:")
    2 (pprTyVars sorted_tvs)
  where
    sorted_tvs = scopedSort skols
pprTcSolverReportMsg _ (UserTypeError ty) =
  pprUserTypeErrorTy ty
pprTcSolverReportMsg _ (UnsatisfiableError ty) =
  pprUserTypeErrorTy ty
pprTcSolverReportMsg ctxt (ReportHoleError hole err) =
  pprHoleError ctxt hole err
pprTcSolverReportMsg ctxt
  (CannotUnifyVariable
    { mismatchMsg         = msg
    , cannotUnifyReason   = reason })
  =  pprMismatchMsg ctxt msg
  $$ pprCannotUnifyVariableReason ctxt reason
pprTcSolverReportMsg ctxt
  (Mismatch
     { mismatchMsg           = mismatch_msg
     , mismatchTyVarInfo     = tv_info
     , mismatchAmbiguityInfo = ambig_infos
     , mismatchCoercibleInfo = coercible_info })
  = vcat ([ pprMismatchMsg ctxt mismatch_msg
          , maybe empty (pprTyVarInfo ctxt) tv_info
          , maybe empty pprCoercibleMsg coercible_info ]
          ++ (map pprAmbiguityInfo ambig_infos))
pprTcSolverReportMsg _ (FixedRuntimeRepError frr_origs) =
  vcat (map make_msg frr_origs)
  where
    -- Assemble the error message: pair up each origin with the corresponding type, e.g.
    --   • FixedRuntimeRep origin msg 1 ...
    --       a :: TYPE r1
    --   • FixedRuntimeRep origin msg 2 ...
    --       b :: TYPE r2
    make_msg :: FixedRuntimeRepErrorInfo -> SDoc
    make_msg (FRR_Info { frr_info_origin =
                           FixedRuntimeRepOrigin
                             { frr_type    = ty
                             , frr_context = frr_ctxt }
                       , frr_info_not_concrete =
                         mb_not_conc }) =
      -- Add bullet points if there is more than one error.
      (if length frr_origs > 1 then (bullet <+>) else id) $
        vcat [ sep [ pprFixedRuntimeRepContext frr_ctxt
                   , text "does not have a fixed runtime representation." ]
             , type_printout ty
             , case mb_not_conc of
                Nothing -> empty
                Just (conc_tv, not_conc) ->
                  unsolved_concrete_eq_explanation conc_tv not_conc ]

    -- Don't print out the type (only the kind), if the type includes
    -- a confusing cast, unless the user passed -fprint-explicit-coercions.
    --
    -- Example:
    --
    --   In T20363, we have a representation-polymorphism error with a type
    --   of the form
    --
    --     ( (# #) |> co ) :: TYPE NilRep
    --
    --   where NilRep is a nullary type family application which reduces to TupleRep '[].
    --   We prefer avoiding showing the cast to the user, but we also don't want to
    --   print the confusing:
    --
    --     (# #) :: TYPE NilRep
    --
    --  So in this case we simply don't print the type, only the kind.
    confusing_cast :: Type -> Bool
    confusing_cast ty =
      case ty of
        CastTy inner_ty _
          -- A confusing cast is one that is responsible
          -- for a representation-polymorphism error.
          -> isConcreteType (typeKind inner_ty)
        _ -> False

    type_printout :: Type -> SDoc
    type_printout ty =
      sdocOption sdocPrintExplicitCoercions $ \ show_coercions ->
        if  confusing_cast ty && not show_coercions
        then vcat [ text "Its kind is:"
                  , nest 2 $ pprWithTYPE (typeKind ty)
                  , text "(Use -fprint-explicit-coercions to see the full type.)" ]
        else vcat [ text "Its type is:"
                  , nest 2 $ ppr ty <+> dcolon <+> pprWithTYPE (typeKind ty) ]

    unsolved_concrete_eq_explanation :: TcTyVar -> Type -> SDoc
    unsolved_concrete_eq_explanation tv not_conc =
          text "Cannot unify" <+> quotes (ppr not_conc)
      <+> text "with the type variable" <+> quotes (ppr tv)
      $$  text "because the former is not a concrete" <+> what <> dot
      where
        ki = tyVarKind tv
        what :: SDoc
        what
          | isRuntimeRepTy ki
          = quotes (text "RuntimeRep")
          | isLevityTy ki
          = quotes (text "Levity")
          | otherwise
          = text "type"
pprTcSolverReportMsg _ (BlockedEquality item) =
  vcat [ hang (text "Cannot use equality for substitution:")
           2 (ppr (errorItemPred item))
       , text "Doing so would be ill-kinded." ]
pprTcSolverReportMsg _ (ExpectingMoreArguments n thing) =
  text "Expecting" <+> speakN (abs n) <+>
    more <+> quotes (ppr thing)
  where
    more
     | n == 1    = text "more argument to"
     | otherwise = text "more arguments to" -- n > 1
pprTcSolverReportMsg ctxt (UnboundImplicitParams (item :| items)) =
  let givens = getUserGivens ctxt
  in if null givens
     then addArising (errorItemCtLoc item) $
            sep [ text "Unbound implicit parameter" <> plural preds
                , nest 2 (pprParendTheta preds) ]
     else pprMismatchMsg ctxt (CouldNotDeduce givens (item :| items) Nothing)
  where
    preds = map errorItemPred (item : items)
pprTcSolverReportMsg _ (AmbiguityPreventsSolvingCt item ambigs) =
  pprAmbiguityInfo (Ambiguity True ambigs) <+>
  pprArising (errorItemCtLoc item) $$
  text "prevents the constraint" <+> quotes (pprParendType $ errorItemPred item)
  <+> text "from being solved."
pprTcSolverReportMsg ctxt@(CEC {cec_encl = implics})
  (CannotResolveInstance item unifiers candidates rel_binds)
  =
    vcat
      [ no_inst_msg
      , nest 2 extra_note
      , mb_patsyn_prov `orElse` empty
      , ppWhen (has_ambigs && not (null unifiers && null useful_givens))
        (vcat [ ppUnless lead_with_ambig $
                  pprAmbiguityInfo (Ambiguity False (ambig_kvs, ambig_tvs))
              , pprRelevantBindings rel_binds
                -- "Relevant bindings" can help explain to the user where an
                -- ambiguous type variable comes from.
              , potential_msg ])
      , ppWhen (isNothing mb_patsyn_prov) $
            -- Don't suggest fixes for the provided context of a pattern
            -- synonym; the right fix is to bind more in the pattern
        show_fixes (ctxtFixes has_ambigs pred implics
                    ++ drv_fixes ++ naked_sc_fixes)
      , ppWhen (not (null candidates))
        (hang (text "There are instances for similar types:")
            2 (vcat (map ppr candidates)))
            -- See Note [Report candidate instances]
      ]
  where
    orig          = errorItemOrigin item
    pred          = errorItemPred item
    (clas, tys)   = getClassPredTys pred
    -- See Note [Highlighting ambiguous type variables] in GHC.Tc.Errors
    (ambig_kvs, ambig_tvs) = ambigTkvsOfTy pred
    ambigs = ambig_kvs ++ ambig_tvs
    has_ambigs = not (null ambigs)
    useful_givens = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
         -- useful_givens are the enclosing implications with non-empty givens,
         -- modulo the horrid discardProvCtxtGivens
    lead_with_ambig = not (null ambigs)
                   && not (any isRuntimeUnkSkol ambigs)
                   && not (null unifiers)
                   && null useful_givens

    no_inst_msg :: SDoc
    no_inst_msg
      | lead_with_ambig
      = pprTcSolverReportMsg ctxt $ AmbiguityPreventsSolvingCt item (ambig_kvs, ambig_tvs)
      | otherwise
      = pprMismatchMsg ctxt $ CouldNotDeduce useful_givens (item :| []) Nothing

    -- Report "potential instances" only when the constraint arises
    -- directly from the user's use of an overloaded function
    want_potential (TypeEqOrigin {}) = False
    want_potential _                 = True

    potential_msg
      = ppWhen (not (null unifiers) && want_potential orig) $
          potential_hdr $$
          potentialInstancesErrMsg (PotentialInstances { matches = [], unifiers })

    potential_hdr
      = ppWhen lead_with_ambig $
        text "Probable fix: use a type annotation to specify what"
        <+> pprQuotedList ambig_tvs <+> text "should be."

    mb_patsyn_prov :: Maybe SDoc
    mb_patsyn_prov
      | not lead_with_ambig
      , ProvCtxtOrigin PSB{ psb_def = L _ pat } <- orig
      = Just (vcat [ text "In other words, a successful match on the pattern"
                   , nest 2 $ ppr pat
                   , text "does not provide the constraint" <+> pprParendType pred ])
      | otherwise = Nothing

    extra_note | any isFunTy (filterOutInvisibleTypes (classTyCon clas) tys)
               = text "(maybe you haven't applied a function to enough arguments?)"
               | className clas == typeableClassName  -- Avoid mysterious "No instance for (Typeable T)
               , [_,ty] <- tys                        -- Look for (Typeable (k->*) (T k))
               , Just (tc,_) <- tcSplitTyConApp_maybe ty
               , not (isTypeFamilyTyCon tc)
               = hang (text "GHC can't yet do polykinded")
                    2 (text "Typeable" <+>
                       parens (ppr ty <+> dcolon <+> ppr (typeKind ty)))
               | otherwise
               = empty

    drv_fixes = case orig of
                   DerivClauseOrigin                  -> [drv_fix False]
                   StandAloneDerivOrigin              -> [drv_fix True]
                   DerivOriginDC _ _       standalone -> [drv_fix standalone]
                   DerivOriginCoerce _ _ _ standalone -> [drv_fix standalone]
                   _                                  -> []

    drv_fix standalone_wildcard
      | standalone_wildcard
      = text "fill in the wildcard constraint yourself"
      | otherwise
      = hang (text "use a standalone 'deriving instance' declaration,")
           2 (text "so you can specify the instance context yourself")

    -- naked_sc_fix: try to produce a helpful error message for
    -- superclass constraints caught by the subtleties described by
    -- Note [Recursive superclasses] in GHC.TyCl.Instance
    naked_sc_fixes
      | ScOrigin _ NakedSc <- orig  -- A superclass wanted with no instance decls used yet
      , any non_tyvar_preds useful_givens  -- Some non-tyvar givens
      = [vcat [ text "If the constraint looks soluble from a superclass of the instance context,"
              , text "read 'Undecidable instances and loopy superclasses' in the user manual" ]]
      | otherwise = []

    non_tyvar_preds :: UserGiven -> Bool
    non_tyvar_preds = any non_tyvar_pred . ic_given

    non_tyvar_pred :: EvVar -> Bool
    -- Tells if the Given is of form (C ty1 .. tyn), where the tys are not all tyvars
    non_tyvar_pred given = case getClassPredTys_maybe (idType given) of
                             Just (_, tys) -> not (all isTyVarTy tys)
                             Nothing       -> False

pprTcSolverReportMsg (CEC {cec_encl = implics}) (OverlappingInstances item matches unifiers) =
  vcat
    [ addArising ct_loc $
        (text "Overlapping instances for"
        <+> pprType (mkClassPred clas tys))
    , ppUnless (null matching_givens) $
                  sep [text "Matching givens (or their superclasses):"
                      , nest 2 (vcat matching_givens)]
    ,  potentialInstancesErrMsg
        (PotentialInstances { matches = NE.toList matches, unifiers })
    ,  ppWhen (null matching_givens && null (NE.tail matches) && null unifiers) $
       -- Intuitively, some given matched the wanted in their
       -- flattened or rewritten (from given equalities) form
       -- but the matcher can't figure that out because the
       -- constraints are non-flat and non-rewritten so we
       -- simply report back the whole given
       -- context. Accelerate Smart.hs showed this problem.
         sep [ text "There exists a (perhaps superclass) match:"
             , nest 2 (vcat (pp_givens useful_givens))]

    ,  ppWhen (null $ NE.tail matches) $
       parens (vcat [ ppUnless (null tyCoVars) $
                        text "The choice depends on the instantiation of" <+>
                          quotes (pprWithCommas ppr tyCoVars)
                    , ppUnless (null famTyCons) $
                        if (null tyCoVars)
                          then
                            text "The choice depends on the result of evaluating" <+>
                              quotes (pprWithCommas ppr famTyCons)
                          else
                            text "and the result of evaluating" <+>
                              quotes (pprWithCommas ppr famTyCons)
                    , ppWhen (null (matching_givens)) $
                      vcat [ text "To pick the first instance above, use IncoherentInstances"
                           , text "when compiling the other instance declarations"]
               ])]
  where
    ct_loc          = errorItemCtLoc item
    orig            = ctLocOrigin ct_loc
    pred            = errorItemPred item
    (clas, tys)     = getClassPredTys pred
    tyCoVars        = tyCoVarsOfTypesList tys
    famTyCons       = filter isFamilyTyCon $ concatMap (nonDetEltsUniqSet . tyConsOfType) tys
    useful_givens   = discardProvCtxtGivens orig (getUserGivensFromImplics implics)
    matching_givens = mapMaybe matchable useful_givens
    matchable implic@(Implic { ic_given = evvars, ic_info = skol_info })
      = case ev_vars_matching of
             [] -> Nothing
             _  -> Just $ hang (pprTheta ev_vars_matching)
                            2 (sep [ text "bound by" <+> ppr skol_info
                                   , text "at" <+>
                                     ppr (getCtLocEnvLoc (ic_env implic)) ])
        where ev_vars_matching = [ pred
                                 | ev_var <- evvars
                                 , let pred = evVarPred ev_var
                                 , any can_match (pred : transSuperClasses pred) ]
              can_match pred
                 = case getClassPredTys_maybe pred of
                     Just (clas', tys') -> clas' == clas
                                          && isJust (tcMatchTys tys tys')
                     Nothing -> False
pprTcSolverReportMsg _ (UnsafeOverlap item match unsafe_overlapped) =
  vcat [ addArising ct_loc (text "Unsafe overlapping instances for"
                  <+> pprType (mkClassPred clas tys))
       , sep [text "The matching instance is:",
              nest 2 (pprInstance match)]
       , vcat [ text "It is compiled in a Safe module and as such can only"
              , text "overlap instances from the same module, however it"
              , text "overlaps the following instances from different" <+>
                text "modules:"
              , nest 2 (vcat [pprInstances $ NE.toList unsafe_overlapped])
              ]
       ]
  where
    ct_loc      = errorItemCtLoc item
    pred        = errorItemPred item
    (clas, tys) = getClassPredTys pred

pprTcSolverReportMsg _ MultiplicityCoercionsNotSupported = text "GHC bug #19517: GHC currently does not support programs using GADTs or type families to witness equality of multiplicities"

pprCannotUnifyVariableReason :: SolverReportErrCtxt -> CannotUnifyVariableReason -> SDoc
pprCannotUnifyVariableReason ctxt (CannotUnifyWithPolytype item tv1 ty2 mb_tv_info) =
  vcat [ (if isSkolemTyVar tv1
          then text "Cannot equate type variable"
          else text "Cannot instantiate unification variable")
         <+> quotes (ppr tv1)
       , hang (text "with a" <+> what <+> text "involving polytypes:") 2 (ppr ty2)
       , maybe empty (pprTyVarInfo ctxt) mb_tv_info ]
  where
    what = text $ levelString $
           ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel

pprCannotUnifyVariableReason _ (SkolemEscape item implic esc_skols) =
  let
    esc_doc = sep [ text "because" <+> what <+> text "variable" <> plural esc_skols
                <+> pprQuotedList esc_skols
              , text "would escape" <+>
                if isSingleton esc_skols then text "its scope"
                                         else text "their scope" ]
  in
  vcat [ nest 2 $ esc_doc
       , sep [ (if isSingleton esc_skols
                then text "This (rigid, skolem)" <+>
                     what <+> text "variable is"
                else text "These (rigid, skolem)" <+>
                     what <+> text "variables are")
         <+> text "bound by"
       , nest 2 $ ppr (ic_info implic)
       , nest 2 $ text "at" <+>
         ppr (getCtLocEnvLoc (ic_env implic)) ] ]
  where
    what = text $ levelString $
           ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel

pprCannotUnifyVariableReason ctxt
  (OccursCheck
    { occursCheckInterestingTyVars = interesting_tvs
    , occursCheckAmbiguityInfos    = ambig_infos })
  = ppr_interesting_tyVars interesting_tvs
  $$ vcat (map pprAmbiguityInfo ambig_infos)
  where
    ppr_interesting_tyVars [] = empty
    ppr_interesting_tyVars (tv:tvs) =
      hang (text "Type variable kinds:") 2 $
      vcat (map (tyvar_binding . tidyTyCoVarOcc (cec_tidy ctxt))
                (tv:tvs))
    tyvar_binding tyvar = ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)
pprCannotUnifyVariableReason ctxt (DifferentTyVars tv_info)
  = pprTyVarInfo ctxt tv_info
pprCannotUnifyVariableReason ctxt (RepresentationalEq tv_info mb_coercible_msg)
  = pprTyVarInfo ctxt tv_info
  $$ maybe empty pprCoercibleMsg mb_coercible_msg

pprUntouchableVariable :: TcTyVar -> Implication -> SDoc
pprUntouchableVariable tv (Implic { ic_given = given, ic_info = skol_info, ic_env = env })
  = sep [ quotes (ppr tv) <+> text "is untouchable"
        , nest 2 $ text "inside the constraints:" <+> pprEvVarTheta given
        , nest 2 $ text "bound by" <+> ppr skol_info
        , nest 2 $ text "at" <+> ppr (getCtLocEnvLoc env) ]

pprMismatchMsg :: SolverReportErrCtxt -> MismatchMsg -> SDoc
pprMismatchMsg ctxt
  (BasicMismatch { mismatch_ea   = ea
                 , mismatch_item = item
                 , mismatch_ty1  = ty1  -- Expected
                 , mismatch_ty2  = ty2  -- Actual
                 , mismatch_whenMatching = mb_match_txt
                 , mismatch_mb_same_occ  = same_occ_info })
  =  vcat [ addArising (errorItemCtLoc item) msg
          , ea_extra
          , maybe empty (pprWhenMatching ctxt) mb_match_txt
          , maybe empty pprSameOccInfo same_occ_info ]
  where
    msg
      | (isLiftedRuntimeRep ty1 && isUnliftedRuntimeRep ty2) ||
        (isLiftedRuntimeRep ty2 && isUnliftedRuntimeRep ty1) ||
        (isLiftedLevity ty1 && isUnliftedLevity ty2) ||
        (isLiftedLevity ty2 && isUnliftedLevity ty1)
      = text "Couldn't match a lifted type with an unlifted type"

      | isAtomicTy ty1 || isAtomicTy ty2
      = -- Print with quotes
        sep [ text herald1 <+> quotes (ppr ty1)
            , nest padding $
              text herald2 <+> quotes (ppr ty2) ]

      | otherwise
      = -- Print with vertical layout
        vcat [ text herald1 <> colon <+> ppr ty1
             , nest padding $
               text herald2 <> colon <+> ppr ty2 ]

    herald1 = conc [ "Couldn't match"
                   , if is_repr then "representation of" else ""
                   , if want_ea then "expected"          else ""
                   , what ]
    herald2 = conc [ "with"
                   , if is_repr then "that of"           else ""
                   , if want_ea then ("actual " ++ what) else "" ]

    padding = length herald1 - length herald2

    (want_ea, ea_extra)
      = case ea of
         NoEA        -> (False, empty)
         EA mb_extra -> (True , maybe empty (pprExpectedActualInfo ctxt) mb_extra)
    is_repr = case errorItemEqRel item of { ReprEq -> True; NomEq -> False }

    what = levelString (ctLocTypeOrKind_maybe (errorItemCtLoc item) `orElse` TypeLevel)

    conc :: [String] -> String
    conc = unwords . filter (not . null)

pprMismatchMsg _
  (KindMismatch { kmismatch_what     = thing
                , kmismatch_expected = exp
                , kmismatch_actual   = act })
  = hang (text "Expected" <+> kind_desc <> comma)
      2 (text "but" <+> quotes (ppr thing) <+> text "has kind" <+>
        quotes (ppr act))
  where
    kind_desc | isConstraintLikeKind exp = text "a constraint"
              | Just arg <- kindRep_maybe exp  -- TYPE t0
              , tcIsTyVarTy arg = sdocOption sdocPrintExplicitRuntimeReps $ \case
                                   True  -> text "kind" <+> quotes (ppr exp)
                                   False -> text "a type"
              | otherwise       = text "kind" <+> quotes (ppr exp)

pprMismatchMsg ctxt
  (TypeEqMismatch { teq_mismatch_item     = item
                  , teq_mismatch_ty1      = ty1   -- These types are the actual types
                  , teq_mismatch_ty2      = ty2   --   that don't match; may be swapped
                  , teq_mismatch_expected = exp   -- These are the context of
                  , teq_mismatch_actual   = act   --   the mis-match
                  , teq_mismatch_what     = mb_thing
                  , teq_mb_same_occ       = mb_same_occ })
  = addArising ct_loc $
    pprWithInvisibleBitsWhen ppr_invis_bits msg
    $$ maybe empty pprSameOccInfo mb_same_occ
  where

    msg | Just (torc, rep) <- sORTKind_maybe exp
        = msg_for_exp_sort torc rep

        | Just nargs_msg <- num_args_msg
        , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
        = nargs_msg $$ pprMismatchMsg ctxt ea_msg

        | ea_looks_same ty1 ty2 exp act
        , Right ea_msg <- mk_ea_msg ctxt (Just item) level orig
        = pprMismatchMsg ctxt ea_msg

        | otherwise
        = bale_out_msg

      -- bale_out_msg: the mismatched types are /inside/ exp and act
    bale_out_msg = vcat errs
      where
        errs = case mk_ea_msg ctxt Nothing level orig of
                  Left ea_info -> pprMismatchMsg ctxt mismatch_err
                                : map (pprExpectedActualInfo ctxt) ea_info
                  Right ea_err -> [ pprMismatchMsg ctxt mismatch_err
                                  , pprMismatchMsg ctxt ea_err ]
        mismatch_err = mkBasicMismatchMsg NoEA item ty1 ty2

      -- 'expected' is (TYPE rep) or (CONSTRAINT rep)
    msg_for_exp_sort exp_torc exp_rep
      | Just (act_torc, act_rep) <- sORTKind_maybe act
      = -- (TYPE exp_rep) ~ (CONSTRAINT act_rep) etc
        msg_torc_torc act_torc act_rep
      | otherwise
      = -- (TYPE _) ~ Bool, etc
        maybe_num_args_msg $$
        sep [ text "Expected a" <+> ppr_torc exp_torc <> comma
            , text "but" <+> case mb_thing of
                Nothing    -> text "found something with kind"
                Just thing -> quotes (ppr thing) <+> text "has kind"
            , quotes (pprWithTYPE act) ]

      where
        msg_torc_torc act_torc act_rep
          | exp_torc == act_torc
          = msg_same_torc act_torc act_rep
          | otherwise
          = sep [ text "Expected a" <+> ppr_torc exp_torc <> comma
                , text "but" <+> case mb_thing of
                     Nothing    -> text "found a"
                     Just thing -> quotes (ppr thing) <+> text "is a"
                  <+> ppr_torc act_torc ]

        msg_same_torc act_torc act_rep
          | Just exp_doc <- describe_rep exp_rep
          , Just act_doc <- describe_rep act_rep
          = sep [ text "Expected" <+> exp_doc <+> ppr_torc exp_torc <> comma
                , text "but" <+> case mb_thing of
                     Just thing -> quotes (ppr thing) <+> text "is"
                     Nothing    -> text "got"
                  <+> act_doc <+> ppr_torc act_torc ]
        msg_same_torc _ _ = bale_out_msg

    ct_loc = errorItemCtLoc item
    orig   = errorItemOrigin item
    level  = ctLocTypeOrKind_maybe ct_loc `orElse` TypeLevel
    ppr_invis_bits = shouldPprWithInvisibleBits ty1 ty2 orig

    num_args_msg = case level of
      KindLevel
        | not (isMetaTyVarTy exp) && not (isMetaTyVarTy act)
           -- if one is a meta-tyvar, then it's possible that the user
           -- has asked for something impredicative, and we couldn't unify.
           -- Don't bother with counting arguments.
        -> let n_act = count_args act
               n_exp = count_args exp in
           case n_act - n_exp of
             n | n > 0   -- we don't know how many args there are, so don't
                         -- recommend removing args that aren't
               , Just thing <- mb_thing
               -> Just $ pprTcSolverReportMsg ctxt (ExpectingMoreArguments n thing)
             _ -> Nothing

      _ -> Nothing

    maybe_num_args_msg = num_args_msg `orElse` empty

    count_args ty = count isVisiblePiTyBinder $ fst $ splitPiTys ty

    ppr_torc TypeLike       = text "type";
    ppr_torc ConstraintLike = text "constraint"

    describe_rep :: RuntimeRepType -> Maybe SDoc
    -- describe_rep IntRep            = Just "an IntRep"
    -- describe_rep (BoxedRep Lifted) = Just "a lifted"
    --   etc
    describe_rep rep
      | Just (rr_tc, rr_args) <- splitRuntimeRep_maybe rep
      = case rr_args of
          [lev_ty] | rr_tc `hasKey` boxedRepDataConKey
                   , Just lev <- levityType_maybe lev_ty
                -> case lev of
                      Lifted   -> Just (text "a lifted")
                      Unlifted -> Just (text "a boxed unlifted")
          [] | rr_tc `hasKey` tupleRepDataConTyConKey -> Just (text "a zero-bit")
             | starts_with_vowel rr_occ -> Just (text "an" <+> text rr_occ)
             | otherwise                -> Just (text "a"  <+> text rr_occ)
             where
               rr_occ = occNameString (getOccName rr_tc)

          _ -> Nothing -- Must be TupleRep [r1..rn]
      | otherwise = Nothing

    starts_with_vowel (c:_) = c `elem` ("AEIOU" :: String)
    starts_with_vowel []    = False

pprMismatchMsg ctxt (CouldNotDeduce useful_givens (item :| others) mb_extra)
  = main_msg $$
     case supplementary of
      Left infos
        -> vcat (map (pprExpectedActualInfo ctxt) infos)
      Right other_msg
        -> pprMismatchMsg ctxt other_msg
  where
    main_msg
      | null useful_givens
      = addArising ct_loc (no_instance_msg <+> missing)
      | otherwise
      = vcat (addArising ct_loc (no_deduce_msg <+> missing)
              : pp_givens useful_givens)

    supplementary = case mb_extra of
      Nothing
        -> Left []
      Just (CND_Extra level ty1 ty2)
        -> mk_supplementary_ea_msg ctxt level ty1 ty2 orig
    ct_loc = errorItemCtLoc item
    orig   = ctLocOrigin ct_loc
    wanteds = map errorItemPred (item:others)

    no_instance_msg =
      case wanteds of
        [wanted] | Just (tc, _) <- splitTyConApp_maybe wanted
                 -- Don't say "no instance" for a constraint such as "c" for a type variable c.
                 , isClassTyCon tc -> text "No instance for"
        _ -> text "Could not solve:"

    no_deduce_msg =
      case wanteds of
        [_wanted] -> text "Could not deduce"
        _         -> text "Could not deduce:"

    missing =
      case wanteds of
        [wanted] -> quotes (ppr wanted)
        _        -> pprTheta wanteds


-- | Whether to print explicit kinds (with @-fprint-explicit-kinds@)
-- in an 'SDoc' when a type mismatch occurs to due invisible parts of the types.
-- See Note [Showing invisible bits of types in error messages]
--
-- This function first checks to see if the 'CtOrigin' argument is a
-- 'TypeEqOrigin'. If so, it first checks whether the equality is a visible
-- equality; if it's not, definitely print the kinds. Even if the equality is
-- a visible equality, check the expected/actual types to see if the types
-- have equal visible components. If the 'CtOrigin' is
-- not a 'TypeEqOrigin', fall back on the actual mismatched types themselves.
shouldPprWithInvisibleBits :: Type -> Type -> CtOrigin -> Bool
shouldPprWithInvisibleBits _ty1 _ty2 (TypeEqOrigin { uo_actual = act
                                                   , uo_expected = exp
                                                   , uo_visible = vis })
  | not vis   = True                  -- See tests T15870, T16204c
  | otherwise = mayLookIdentical act exp   -- See tests T9171, T9144.
shouldPprWithInvisibleBits ty1 ty2 _ct
  = mayLookIdentical ty1 ty2

{- Note [Showing invisible bits of types in error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It can be terribly confusing to get an error message like (#9171)

    Couldn't match expected type ‘GetParam Base (GetParam Base Int)’
                with actual type ‘GetParam Base (GetParam Base Int)’

The reason may be that the kinds don't match up.  Typically you'll get
more useful information, but not when it's as a result of ambiguity.

To mitigate this, when we find a type or kind mis-match:

* See if normally-visible parts of the type would make the two types
  look different.  This check is made by
  `GHC.Core.TyCo.Compare.mayLookIdentical`

* If not, display the types with their normally-visible parts made visible,
  by setting flags in the `SDocContext":
  Specifically:
    - Display kind arguments: sdocPrintExplicitKinds
    - Don't default away runtime-reps: sdocPrintExplicitRuntimeReps,
           which controls `GHC.Iface.Type.hideNonStandardTypes`
  (NB: foralls are always printed by pprType, it turns out.)

As a result the above error message would instead be displayed as:

    Couldn't match expected type
                  ‘GetParam @* @k2 @* Base (GetParam @* @* @k2 Base Int)’
                with actual type
                  ‘GetParam @* @k20 @* Base (GetParam @* @* @k20 Base Int)’

Which makes it clearer that the culprit is the mismatch between `k2` and `k20`.

Another example of what goes wrong without this: #24553.
-}

{- *********************************************************************
*                                                                      *
                 Displaying potential instances
*                                                                      *
**********************************************************************-}

-- | Directly display the given matching and unifying instances,
-- with a header for each: `Matching instances`/`Potentially matching instances`.
pprPotentialInstances :: (ClsInst -> SDoc) -> PotentialInstances -> SDoc
pprPotentialInstances ppr_inst (PotentialInstances { matches, unifiers }) =
  vcat
    [ ppWhen (not $ null matches) $
       text "Matching instance" <> plural matches <> colon $$
         nest 2 (vcat (map ppr_inst matches))
    , ppWhen (not $ null unifiers) $
        (text "Potentially matching instance" <> plural unifiers <> colon) $$
         nest 2 (vcat (map ppr_inst unifiers))
    ]

-- | Display a summary of available instances, omitting those involving
-- out-of-scope types, in order to explain why we couldn't solve a particular
-- constraint, e.g. due to instance overlap or out-of-scope types.
--
-- To directly display a collection of matching/unifying instances,
-- use 'pprPotentialInstances'.
potentialInstancesErrMsg :: PotentialInstances -> SDoc
-- See Note [Displaying potential instances]
potentialInstancesErrMsg potentials =
  sdocOption sdocPrintPotentialInstances $ \print_insts ->
  getPprStyle $ \sty ->
    potentials_msg_with_options potentials print_insts sty

-- | Display a summary of available instances, omitting out-of-scope ones.
--
-- Use 'potentialInstancesErrMsg' to automatically set the pretty-printing
-- options.
potentials_msg_with_options :: PotentialInstances
                            -> Bool -- ^ Whether to print /all/ potential instances
                            -> PprStyle
                            -> SDoc
potentials_msg_with_options
  (PotentialInstances { matches, unifiers })
  show_all_potentials sty
  | null matches && null unifiers
  = empty

  | null show_these_matches && null show_these_unifiers
  = vcat [ not_in_scope_msg empty
         , flag_hint ]

  | otherwise
  = vcat [ pprPotentialInstances
            pprInstance -- print instance + location info
            (PotentialInstances
              { matches  = show_these_matches
              , unifiers = show_these_unifiers })
         , overlapping_but_not_more_specific_msg sorted_matches
         , nest 2 $ vcat
           [ ppWhen (n_in_scope_hidden > 0) $
             text "...plus"
               <+> speakNOf n_in_scope_hidden (text "other")
           , ppWhen (not_in_scopes > 0) $
              not_in_scope_msg (text "...plus")
           , flag_hint ] ]
  where
    n_show_matches, n_show_unifiers :: Int
    n_show_matches  = 3
    n_show_unifiers = 2

    (in_scope_matches, not_in_scope_matches) = partition inst_in_scope matches
    (in_scope_unifiers, not_in_scope_unifiers) = partition inst_in_scope unifiers
    sorted_matches = sortBy fuzzyClsInstCmp in_scope_matches
    sorted_unifiers = sortBy fuzzyClsInstCmp in_scope_unifiers
    (show_these_matches, show_these_unifiers)
       | show_all_potentials = (sorted_matches, sorted_unifiers)
       | otherwise           = (take n_show_matches  sorted_matches
                               ,take n_show_unifiers sorted_unifiers)
    n_in_scope_hidden
      = length sorted_matches + length sorted_unifiers
      - length show_these_matches - length show_these_unifiers

       -- "in scope" means that all the type constructors
       -- are lexically in scope; these instances are likely
       -- to be more useful
    inst_in_scope :: ClsInst -> Bool
    inst_in_scope cls_inst = nameSetAll name_in_scope $
                             orphNamesOfTypes (is_tys cls_inst)

    name_in_scope name
      | pretendNameIsInScope name
      = True -- E.g. (->); see Note [pretendNameIsInScope] in GHC.Builtin.Names
      | Just mod <- nameModule_maybe name
      = qual_in_scope (qualName sty mod Nothing (nameOccName name))
      | otherwise
      = True

    qual_in_scope :: QualifyName -> Bool
    qual_in_scope NameUnqual    = True
    qual_in_scope (NameQual {}) = True
    qual_in_scope _             = False

    not_in_scopes :: Int
    not_in_scopes = length not_in_scope_matches + length not_in_scope_unifiers

    not_in_scope_msg herald =
      hang (herald <+> speakNOf not_in_scopes (text "instance")
                     <+> text "involving out-of-scope types")
           2 (ppWhen show_all_potentials $
               pprPotentialInstances
               pprInstanceHdr -- only print the header, not the instance location info
                 (PotentialInstances
                   { matches = not_in_scope_matches
                   , unifiers = not_in_scope_unifiers
                   }))

    flag_hint = ppUnless (show_all_potentials
                         || (equalLength show_these_matches matches
                             && equalLength show_these_unifiers unifiers)) $
                text "(use -fprint-potential-instances to see them all)"

-- | Compute a message informing the user of any instances that are overlapped
-- but were not discarded because the instance overlapping them wasn't
-- strictly more specific.
overlapping_but_not_more_specific_msg :: [ClsInst] -> SDoc
overlapping_but_not_more_specific_msg insts
  -- Only print one example of "overlapping but not strictly more specific",
  -- to avoid information overload.
  | overlap : _ <- overlapping_but_not_more_specific
  = overlap_header $$ ppr_overlapping overlap
  | otherwise
  = empty
    where
      overlap_header :: SDoc
      overlap_header
        | [_] <- overlapping_but_not_more_specific
        = text "An overlapping instance can only be chosen when it is strictly more specific."
        | otherwise
        = text "Overlapping instances can only be chosen when they are strictly more specific."
      overlapping_but_not_more_specific :: [(ClsInst, ClsInst)]
      overlapping_but_not_more_specific
        = nubOrdBy (comparing (is_dfun . fst))
          [ (overlapper, overlappee)
          | these <- groupBy ((==) `on` is_cls_nm) insts
          -- Take all pairs of distinct instances...
          , one:others <- tails these -- if `these = [inst_1, inst_2, ...]`
          , other <- others           -- then we get pairs `(one, other) = (inst_i, inst_j)` with `i < j`
          -- ... such that one instance in the pair overlaps the other...
          , let mb_overlapping
                  | hasOverlappingFlag (overlapMode $ is_flag one)
                  || hasOverlappableFlag (overlapMode $ is_flag other)
                  = [(one, other)]
                  | hasOverlappingFlag (overlapMode $ is_flag other)
                  || hasOverlappableFlag (overlapMode $ is_flag one)
                  = [(other, one)]
                  | otherwise
                  = []
          , (overlapper, overlappee) <- mb_overlapping
          -- ... but the overlapper is not more specific than the overlappee.
          , not (overlapper `more_specific_than` overlappee)
          ]
      more_specific_than :: ClsInst -> ClsInst -> Bool
      is1 `more_specific_than` is2
        = isJust (tcMatchTys (is_tys is1) (is_tys is2))
      ppr_overlapping :: (ClsInst, ClsInst) -> SDoc
      ppr_overlapping (overlapper, overlappee)
        = text "The first instance that follows overlaps the second, but is not more specific than it:"
        $$ nest 2 (vcat $ map pprInstanceHdr [overlapper, overlappee])

{- Note [Displaying potential instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When showing a list of instances for
  - overlapping instances (show ones that match)
  - no such instance (show ones that could match)
we want to give it a bit of structure.  Here's the plan

* Say that an instance is "in scope" if all of the
  type constructors it mentions are lexically in scope.
  These are the ones most likely to be useful to the programmer.

* Show at most n_show in-scope instances,
  and summarise the rest ("plus N others")

* Summarise the not-in-scope instances ("plus 4 not in scope")

* Add the flag -fshow-potential-instances which replaces the
  summary with the full list
-}

{- *********************************************************************
*                                                                      *
             Outputting additional solver report information
*                                                                      *
**********************************************************************-}

-- | Pretty-print an informational message, to accompany a 'TcSolverReportMsg'.
pprExpectedActualInfo :: SolverReportErrCtxt -> ExpectedActualInfo -> SDoc
pprExpectedActualInfo _ (ExpectedActual { ea_expected = exp, ea_actual = act }) =
  vcat
    [ text "Expected:" <+> ppr exp
    , text "  Actual:" <+> ppr act ]
pprExpectedActualInfo _
  (ExpectedActualAfterTySynExpansion
    { ea_expanded_expected = exp
    , ea_expanded_actual   = act } )
  = vcat
      [ text "Type synonyms expanded:"
      , text "Expected type:" <+> ppr exp
      , text "  Actual type:" <+> ppr act ]

pprCoercibleMsg :: CoercibleMsg -> SDoc
pprCoercibleMsg (UnknownRoles ty) =
  note $ "We cannot know what roles the parameters to" <+> quotes (ppr ty) <+> "have;" $$
           "we must assume that the role is nominal"
pprCoercibleMsg (TyConIsAbstract tc) =
  note $ "The type constructor" <+> quotes (pprSourceTyCon tc) <+> "is abstract"
pprCoercibleMsg (OutOfScopeNewtypeConstructor tc dc) =
  hang (text "The data constructor" <+> quotes (ppr $ dataConName dc))
    2 (sep [ text "of newtype" <+> quotes (pprSourceTyCon tc)
           , text "is not in scope" ])

pprWhenMatching :: SolverReportErrCtxt -> WhenMatching -> SDoc
pprWhenMatching ctxt (WhenMatching cty1 cty2 sub_o mb_sub_t_or_k) =
  sdocOption sdocPrintExplicitCoercions $ \printExplicitCoercions ->
    if printExplicitCoercions
       || not (cty1 `pickyEqType` cty2)
      then vcat [ hang (text "When matching" <+> sub_whats)
                      2 (vcat [ ppr cty1 <+> dcolon <+>
                               ppr (typeKind cty1)
                             , ppr cty2 <+> dcolon <+>
                               ppr (typeKind cty2) ])
                , supplementary ]
      else text "When matching the kind of" <+> quotes (ppr cty1)
  where
    sub_t_or_k = mb_sub_t_or_k `orElse` TypeLevel
    sub_whats  = text (levelString sub_t_or_k) <> char 's'
    supplementary =
      case mk_supplementary_ea_msg ctxt sub_t_or_k cty1 cty2 sub_o of
        Left infos -> vcat $ map (pprExpectedActualInfo ctxt) infos
        Right msg  -> pprMismatchMsg ctxt msg

pprTyVarInfo :: SolverReportErrCtxt -> TyVarInfo -> SDoc
pprTyVarInfo ctxt (TyVarInfo { thisTyVar = tv1, otherTy = mb_tv2, thisTyVarIsUntouchable = mb_implic })
  = vcat [ mk_msg tv1
         , maybe empty (pprUntouchableVariable tv1) mb_implic
         , case mb_tv2 of { Nothing -> empty; Just tv2 -> mk_msg tv2 } ]
  where
    mk_msg tv = case tcTyVarDetails tv of
      SkolemTv sk_info _ _ -> pprSkols ctxt [(getSkolemInfo sk_info, [tv])]
      RuntimeUnk {} -> quotes (ppr tv) <+> text "is an interactive-debugger skolem"
      MetaTv {}     -> empty

pprAmbiguityInfo :: AmbiguityInfo -> SDoc
pprAmbiguityInfo (Ambiguity prepend_msg (ambig_kvs, ambig_tvs)) = msg
  where

    msg |  any isRuntimeUnkSkol ambig_kvs  -- See Note [Runtime skolems]
        || any isRuntimeUnkSkol ambig_tvs
        = vcat [ text "Cannot resolve unknown runtime type"
                 <> plural ambig_tvs <+> pprQuotedList ambig_tvs
               , text "Use :print or :force to determine these types"]

        | not (null ambig_tvs)
        = pp_ambig (text "type") ambig_tvs

        | otherwise
        = pp_ambig (text "kind") ambig_kvs

    pp_ambig what tkvs
      | prepend_msg -- "Ambiguous type variable 't0'"
      = text "Ambiguous" <+> what <+> text "variable"
        <> plural tkvs <+> pprQuotedList tkvs

      | otherwise -- "The type variable 't0' is ambiguous"
      = text "The" <+> what <+> text "variable" <> plural tkvs
        <+> pprQuotedList tkvs <+> isOrAre tkvs <+> text "ambiguous"
pprAmbiguityInfo (NonInjectiveTyFam tc) =
  note $ quotes (ppr tc) <+> text "is a non-injective type family"

pprSameOccInfo :: SameOccInfo -> SDoc
pprSameOccInfo (SameOcc same_pkg n1 n2) =
  note (ppr_from same_pkg n1 $$ ppr_from same_pkg n2)
  where
    ppr_from same_pkg nm
      | isGoodSrcSpan loc
      = hang (quotes (ppr nm) <+> text "is defined at")
           2 (ppr loc)
      | otherwise  -- Imported things have an UnhelpfulSrcSpan
      = hang (quotes (ppr nm))
           2 (sep [ text "is defined in" <+> quotes (ppr (moduleName mod))
                  , ppUnless (same_pkg || pkg == mainUnit) $
                    nest 4 $ text "in package" <+> quotes (ppr pkg) ])
      where
        pkg = moduleUnit mod
        mod = nameModule nm
        loc = nameSrcSpan nm

{- *********************************************************************
*                                                                      *
                  Outputting HoleError messages
*                                                                      *
**********************************************************************-}

pprHoleError :: SolverReportErrCtxt -> Hole -> HoleError -> SDoc
pprHoleError _ (Hole { hole_ty, hole_occ = rdr }) OutOfScopeHole
  = out_of_scope_msg
  where
    herald | isDataOcc (rdrNameOcc rdr) = text "Data constructor not in scope:"
           | otherwise     = text "Variable not in scope:"
    out_of_scope_msg -- Print v :: ty only if the type has structure
      | boring_type = hang herald 2 (ppr rdr)
      | otherwise   = hang herald 2 (pp_rdr_with_type rdr hole_ty)
    boring_type = isTyVarTy hole_ty
pprHoleError ctxt (Hole { hole_ty, hole_occ}) (HoleError sort other_tvs hole_skol_info) =
  vcat [ hole_msg
       , tyvars_msg
       , case sort of { ExprHole {} -> expr_hole_hint; _ -> type_hole_hint } ]

  where

    hole_msg = case sort of
      ExprHole {} ->
        hang (text "Found hole:")
          2 (pp_rdr_with_type hole_occ hole_ty)
      TypeHole ->
        hang (text "Found type wildcard" <+> quotes (ppr hole_occ))
          2 (text "standing for" <+> quotes pp_hole_type_with_kind)
      ConstraintHole ->
        hang (text "Found extra-constraints wildcard standing for")
          2 (quotes $ pprType hole_ty)  -- always kind Constraint

    hole_kind = typeKind hole_ty

    pp_hole_type_with_kind
      | isLiftedTypeKind hole_kind
        || isCoVarType hole_ty -- Don't print the kind of unlifted
                               -- equalities (#15039)
      = pprType hole_ty
      | otherwise
      = pprType hole_ty <+> dcolon <+> pprKind hole_kind

    tyvars = tyCoVarsOfTypeList hole_ty
    tyvars_msg = ppUnless (null tyvars) $
                 text "Where:" <+> (vcat (map loc_msg other_tvs)
                                    $$ pprSkols ctxt hole_skol_info)
                      -- Coercion variables can be free in the
                      -- hole, via kind casts
    expr_hole_hint                       -- Give hint for, say,   f x = _x
         | lengthFS (occNameFS (rdrNameOcc hole_occ)) > 1  -- Don't give this hint for plain "_"
         = text "Or perhaps" <+> quotes (ppr hole_occ)
           <+> text "is mis-spelled, or not in scope"
         | otherwise
         = empty

    type_hole_hint
         | ErrorWithoutFlag <- cec_type_holes ctxt
         = text "To use the inferred type, enable PartialTypeSignatures"
         | otherwise
         = empty

    loc_msg tv
       | isTyVar tv
       = case tcTyVarDetails tv of
           MetaTv {} -> quotes (ppr tv) <+> text "is an ambiguous type variable"
           _         -> empty  -- Skolems dealt with already
       | otherwise  -- A coercion variable can be free in the hole type
       = ppWhenOption sdocPrintExplicitCoercions $
           quotes (ppr tv) <+> text "is a coercion variable"

pp_rdr_with_type :: RdrName -> Type -> SDoc
pp_rdr_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

{- *********************************************************************
*                                                                      *
                  Outputting ScopeError messages
*                                                                      *
**********************************************************************-}

pprScopeError :: RdrName -> NotInScopeError -> SDoc
pprScopeError rdr_name scope_err =
  case scope_err of
    NotInScope ->
      hang (text "Not in scope:")
        2 (what <+> quotes (ppr rdr_name))
    NotARecordField ->
      hang (text "Not in scope:")
        2 (text "record field" <+> quotes (ppr rdr_name))
    NoExactName name ->
      text "The Name" <+> quotes (ppr name) <+> text "is not in scope."
    SameName gres ->
      assertPpr (length gres >= 2) (text "pprScopeError SameName: fewer than 2 elements" $$ nest 2 (ppr gres))
      $ hang (text "Same Name in multiple name-spaces:")
           2 (vcat (map pp_one sorted_names))
      where
        sorted_names = sortBy (leftmost_smallest `on` nameSrcSpan)
                     $ map greName gres
        pp_one name
          = hang (pprNameSpace (occNameSpace (getOccName name))
                  <+> quotes (ppr name) <> comma)
               2 (text "declared at:" <+> ppr (nameSrcLoc name))
    MissingBinding sig _ ->
      sep [ text "The" <+> pprSigLike sig
               <+> text "for" <+> quotes (ppr rdr_name)
          , nest 2 $ text "lacks an accompanying binding" ]
    NoTopLevelBinding ->
      hang (text "No top-level binding for")
        2 (what <+> quotes (ppr rdr_name) <+> text "in this module")
    UnknownSubordinate parent_nm sub ->
      quotes (ppr rdr_name) <+> text "is not a (visible)" <+> pprSubordinate parent_nm sub
    NotInScopeTc env ->
      vcat[text "GHC internal error:" <+> quotes (ppr rdr_name) <+>
      text "is not in scope during type checking, but it passed the renamer",
      text "tcl_env of environment:" <+> ppr env]
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

scopeErrorHints :: NotInScopeError -> [GhcHint]
scopeErrorHints scope_err =
  case scope_err of
    NotInScope             -> noHints
    NotARecordField        -> noHints
    NoExactName {}         -> [SuggestDumpSlices]
    SameName {}            -> [SuggestDumpSlices]
    MissingBinding _ hints -> hints
    NoTopLevelBinding      -> noHints
    UnknownSubordinate {}  -> noHints
    NotInScopeTc _         -> noHints

tcSolverReportMsgHints :: SolverReportErrCtxt -> TcSolverReportMsg -> [GhcHint]
tcSolverReportMsgHints ctxt = \case
  BadTelescope {}
    -> noHints
  UserTypeError {}
    -> noHints
  UnsatisfiableError {}
    -> noHints
  ReportHoleError {}
    -> noHints
  CannotUnifyVariable mismatch_msg rea
    -> mismatchMsgHints ctxt mismatch_msg ++ cannotUnifyVariableHints rea
  Mismatch { mismatchMsg = mismatch_msg }
    -> mismatchMsgHints ctxt mismatch_msg
  FixedRuntimeRepError {}
    -> noHints
  BlockedEquality {}
    -> noHints
  ExpectingMoreArguments {}
    -> noHints
  UnboundImplicitParams {}
    -> noHints
  AmbiguityPreventsSolvingCt {}
    -> noHints
  CannotResolveInstance {}
    -> noHints
  OverlappingInstances {}
    -> noHints
  UnsafeOverlap {}
   -> noHints
  MultiplicityCoercionsNotSupported {}
   -> noHints

mismatchMsgHints :: SolverReportErrCtxt -> MismatchMsg -> [GhcHint]
mismatchMsgHints ctxt msg =
  maybeToList [ hint | (exp,act) <- mismatchMsg_ExpectedActuals msg
                     , hint <- suggestAddSig ctxt exp act ]

mismatchMsg_ExpectedActuals :: MismatchMsg -> Maybe (Type, Type)
mismatchMsg_ExpectedActuals = \case
  BasicMismatch { mismatch_ty1 = exp, mismatch_ty2 = act } ->
    Just (exp, act)
  KindMismatch { kmismatch_expected = exp, kmismatch_actual = act } ->
    Just (exp, act)
  TypeEqMismatch { teq_mismatch_expected = exp, teq_mismatch_actual = act } ->
    Just (exp,act)
  CouldNotDeduce { cnd_extra = cnd_extra }
    | Just (CND_Extra _ exp act) <- cnd_extra
    -> Just (exp, act)
    | otherwise
    -> Nothing

cannotUnifyVariableHints :: CannotUnifyVariableReason -> [GhcHint]
cannotUnifyVariableHints = \case
  CannotUnifyWithPolytype {}
    -> noHints
  OccursCheck {}
    -> noHints
  SkolemEscape {}
    -> noHints
  DifferentTyVars {}
    -> noHints
  RepresentationalEq {}
    -> noHints

suggestAddSig :: SolverReportErrCtxt -> TcType -> TcType -> Maybe GhcHint
-- See Note [Suggest adding a type signature]
suggestAddSig ctxt ty1 _ty2
  | bndr : bndrs <- inferred_bndrs
  = Just $ SuggestAddTypeSignatures $ NamedBindings (bndr :| bndrs)
  | otherwise
  = Nothing
  where
    inferred_bndrs =
      case getTyVar_maybe ty1 of
        Just tv | isSkolemTyVar tv -> find (cec_encl ctxt) False tv
        _                          -> []

    -- 'find' returns the binders of an InferSkol for 'tv',
    -- provided there is an intervening implication with
    -- ic_given_eqs /= NoGivenEqs (i.e. a GADT match)
    find [] _ _ = []
    find (implic:implics) seen_eqs tv
       | tv `elem` ic_skols implic
       , InferSkol prs <- ic_info implic
       , seen_eqs
       = map fst prs
       | otherwise
       = find implics (seen_eqs || ic_given_eqs implic /= NoGivenEqs) tv

{- Note [Suggest adding a type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The OutsideIn algorithm rejects GADT programs that don't have a principal
type, and indeed some that do.  Example:
   data T a where
     MkT :: Int -> T Int

   f (MkT n) = n

Does this have type f :: T a -> a, or f :: T a -> Int?
The error that shows up tends to be an attempt to unify an
untouchable type variable.  So suggestAddSig sees if the offending
type variable is bound by an *inferred* signature, and suggests
adding a declared signature instead.

More specifically, we suggest adding a type sig if we have p ~ ty, and
p is a skolem bound by an InferSkol.  Those skolems were created from
unification variables in simplifyInfer.  Why didn't we unify?  It must
have been because of an intervening GADT or existential, making it
untouchable. Either way, a type signature would help.  For GADTs, it
might make it typeable; for existentials the attempt to write a
signature will fail -- or at least will produce a better error message
next time

This initially came up in #8968, concerning pattern synonyms.
-}

{- *********************************************************************
*                                                                      *
                  Outputting ImportError messages
*                                                                      *
**********************************************************************-}

instance Outputable ImportError where
  ppr err = note $ case err of
      MissingModule mod_name -> "No module named" <+> quoted mod_name <+> "is imported"
      ModulesDoNotExport mods what_look occ_name
        | mod NE.:| [] <- mods -> "The module" <+> quoted mod <+> "does not export" <+> what <+> quoted occ_name
        | otherwise -> "Neither" <+> quotedListWithNor (map ppr $ NE.toList mods) <+> "export" <+> what <+> quoted occ_name
        where
          what :: SDoc
          what = case what_look of
            WL_ConLike -> text "data constructor"
            WL_RecField -> text "record field"
            WL_Term -> text "term"
            _ -> empty
    where
      quoted :: Outputable a => a -> SDoc
      quoted = quotes . ppr

{- *********************************************************************
*                                                                      *
             Suggested fixes for implication constraints
*                                                                      *
**********************************************************************-}

-- TODO: these functions should use GhcHint instead.

show_fixes :: [SDoc] -> SDoc
show_fixes []     = empty
show_fixes (f:fs) = sep [ text "Possible fix:"
                        , nest 2 (vcat (f : map (text "or" <+>) fs))]

ctxtFixes :: Bool -> PredType -> [Implication] -> [SDoc]
ctxtFixes has_ambig_tvs pred implics
  | not has_ambig_tvs
  , isTyVarClassPred pred   -- Don't suggest adding (Eq T) to the context, say
  , (skol:skols) <- usefulContext implics pred
  , let what | null skols
             , SigSkol (PatSynCtxt {}) _ _ <- skol
             = text "\"required\""
             | otherwise
             = empty
  = [sep [ text "add" <+> pprParendType pred
           <+> text "to the" <+> what <+> text "context of"
         , nest 2 $ ppr_skol skol $$
                    vcat [ text "or" <+> ppr_skol skol
                         | skol <- skols ] ] ]
  | otherwise = []
  where
    ppr_skol (PatSkol (RealDataCon dc) _) = text "the data constructor" <+> quotes (ppr dc)
    ppr_skol (PatSkol (PatSynCon ps)   _) = text "the pattern synonym"  <+> quotes (ppr ps)
    ppr_skol skol_info = ppr skol_info

usefulContext :: [Implication] -> PredType -> [SkolemInfoAnon]
-- usefulContext picks out the implications whose context
-- the programmer might plausibly augment to solve 'pred'
usefulContext implics pred
  = go implics
  where
    pred_tvs = tyCoVarsOfType pred
    go [] = []
    go (ic : ics)
       | implausible ic = rest
       | otherwise      = ic_info ic : rest
       where
          -- Stop when the context binds a variable free in the predicate
          rest | any (`elemVarSet` pred_tvs) (ic_skols ic) = []
               | otherwise                                 = go ics

    implausible ic
      | null (ic_skols ic)            = True
      | implausible_info (ic_info ic) = True
      | otherwise                     = False

    implausible_info (SigSkol (InfSigCtxt {}) _ _) = True
    implausible_info _                             = False
    -- Do not suggest adding constraints to an *inferred* type signature

pp_givens :: [Implication] -> [SDoc]
pp_givens givens
   = case givens of
         []     -> []
         (g:gs) ->      ppr_given (text "from the context:") g
                 : map (ppr_given (text "or from:")) gs
    where
       ppr_given herald implic@(Implic { ic_given = gs, ic_info = skol_info })
           = hang (herald <+> pprEvVarTheta (mkMinimalBySCs evVarPred gs))
             -- See Note [Suppress redundant givens during error reporting]
             -- for why we use mkMinimalBySCs above.
                2 (sep [ text "bound by" <+> ppr skol_info
                       , text "at" <+> ppr (getCtLocEnvLoc (ic_env implic)) ])

{- *********************************************************************
*                                                                      *
                       CtOrigin information
*                                                                      *
**********************************************************************-}

levelString :: TypeOrKind -> String
levelString TypeLevel = "type"
levelString KindLevel = "kind"

pprArising :: CtLoc -> SDoc
-- Used for the main, top-level error message
-- We've done special processing for TypeEq, KindEq, givens
pprArising ct_loc
  | in_generated_code = empty  -- See Note ["Arising from" messages in generated code]
  | suppress_origin   = empty
  | otherwise         = pprCtOrigin orig
  where
    orig = ctLocOrigin ct_loc
    in_generated_code = ctLocEnvInGeneratedCode (ctLocEnv ct_loc)
    suppress_origin
      | isGivenOrigin orig = True
      | otherwise          = case orig of
          TypeEqOrigin {}         -> True -- We've done special processing
          KindEqOrigin {}         -> True -- for TypeEq, KindEq, givens
          AmbiguityCheckOrigin {} -> True -- The "In the ambiguity check" context
                                          -- is sufficient; more would be repetitive
          _ -> False

-- Add the "arising from..." part to a message
addArising :: CtLoc -> SDoc -> SDoc
addArising ct_loc msg = hang msg 2 (pprArising ct_loc)

pprWithArising :: [Ct] -> SDoc
-- Print something like
--    (Eq a) arising from a use of x at y
--    (Show a) arising from a use of p at q
-- Also return a location for the error message
-- Works for Wanted/Derived only
pprWithArising []
  = panic "pprWithArising"
pprWithArising (ct:cts)
  | null cts
  = addArising loc (pprTheta [ctPred ct])
  | otherwise
  = vcat (map ppr_one (ct:cts))
  where
    loc = ctLoc ct
    ppr_one ct' = hang (parens (pprType (ctPred ct')))
                     2 (pprCtLoc (ctLoc ct'))

{- Note ["Arising from" messages in generated code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider code generated when we desugar code before typechecking;
see Note [Rebindable syntax and XXExprGhcRn].

In this code, constraints may be generated, but we don't want to
say "arising from a call of foo" if 'foo' doesn't appear in the
users code.  We leave the actual CtOrigin untouched (partly because
it is generated in many, many places), but suppress the "Arising from"
message for constraints that originate in generated code.
-}


{- *********************************************************************
*                                                                      *
                           SkolemInfo
*                                                                      *
**********************************************************************-}


tidySkolemInfo :: TidyEnv -> SkolemInfo -> SkolemInfo
tidySkolemInfo env (SkolemInfo u sk_anon) = SkolemInfo u (tidySkolemInfoAnon env sk_anon)

----------------
tidySkolemInfoAnon :: TidyEnv -> SkolemInfoAnon -> SkolemInfoAnon
tidySkolemInfoAnon env (DerivSkol ty)         = DerivSkol (tidyType env ty)
tidySkolemInfoAnon env (SigSkol cx ty tv_prs) = tidySigSkol env cx ty tv_prs
tidySkolemInfoAnon env (InferSkol ids)        = InferSkol (mapSnd (tidyType env) ids)
tidySkolemInfoAnon env (UnifyForAllSkol ty)   = UnifyForAllSkol (tidyType env ty)
tidySkolemInfoAnon _   info                   = info

tidySigSkol :: TidyEnv -> UserTypeCtxt
            -> TcType -> [(Name,TcTyVar)] -> SkolemInfoAnon
-- We need to take special care when tidying SigSkol
-- See Note [SigSkol SkolemInfo] in "GHC.Tc.Types.Origin"
tidySigSkol env cx ty tv_prs
  = SigSkol cx (tidy_ty env ty) tv_prs'
  where
    tv_prs' = mapSnd (tidyTyCoVarOcc env) tv_prs
    inst_env = mkNameEnv tv_prs'

    tidy_ty env (ForAllTy (Bndr tv vis) ty)
      = ForAllTy (Bndr tv' vis) (tidy_ty env' ty)
      where
        (env', tv') = tidy_tv_bndr env tv

    tidy_ty env ty@(FunTy { ft_mult = w, ft_arg = arg, ft_res = res })
      = -- Look under  c => t and t1 -> t2
        ty { ft_mult = tidy_ty env w
           , ft_arg  = tidyType env arg
           , ft_res  = tidy_ty env res }

    tidy_ty env ty = tidyType env ty

    tidy_tv_bndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
    tidy_tv_bndr env@(occ_env, subst) tv
      | Just tv' <- lookupNameEnv inst_env (tyVarName tv)
      = ((occ_env, extendVarEnv subst tv tv'), tv')

      | otherwise
      = tidyVarBndr env tv

pprSkols :: SolverReportErrCtxt -> [(SkolemInfoAnon, [TcTyVar])] -> SDoc
pprSkols ctxt zonked_ty_vars
  =
      let tidy_ty_vars = map (bimap (tidySkolemInfoAnon (cec_tidy ctxt)) id) zonked_ty_vars
      in vcat (map pp_one tidy_ty_vars)
  where

    no_msg = text "No skolem info - we could not find the origin of the following variables" <+> ppr zonked_ty_vars
       $$ text "This should not happen, please report it as a bug following the instructions at:"
       $$ text "https://gitlab.haskell.org/ghc/ghc/wikis/report-a-bug"


    pp_one (UnkSkol cs, tvs)
      = vcat [ hang (pprQuotedList tvs)
                 2 (is_or_are tvs "a" "(rigid, skolem)")
             , nest 2 (text "of unknown origin")
             , nest 2 (text "bound at" <+> ppr (skolsSpan tvs))
             , no_msg
             , prettyCallStackDoc cs
             ]
    pp_one (RuntimeUnkSkol, tvs)
      = hang (pprQuotedList tvs)
           2 (is_or_are tvs "an" "unknown runtime")
    pp_one (skol_info, tvs)
      = vcat [ hang (pprQuotedList tvs)
                  2 (is_or_are tvs "a"  "rigid" <+> text "bound by")
             , nest 2 (pprSkolInfo skol_info)
             , nest 2 (text "at" <+> ppr (skolsSpan tvs)) ]

    is_or_are [_] article adjective = text "is" <+> text article <+> text adjective
                                      <+> text "type variable"
    is_or_are _   _       adjective = text "are" <+> text adjective
                                      <+> text "type variables"

skolsSpan :: [TcTyVar] -> SrcSpan
skolsSpan skol_tvs = foldr1WithDefault noSrcSpan combineSrcSpans (map getSrcSpan skol_tvs)

{- *********************************************************************
*                                                                      *
                Utilities for expected/actual messages
*                                                                      *
**********************************************************************-}

mk_supplementary_ea_msg :: SolverReportErrCtxt -> TypeOrKind
                        -> Type -> Type -> CtOrigin -> Either [ExpectedActualInfo] MismatchMsg
mk_supplementary_ea_msg ctxt level ty1 ty2 orig
  | TypeEqOrigin { uo_expected = exp, uo_actual = act } <- orig
  , not (ea_looks_same ty1 ty2 exp act)
  = mk_ea_msg ctxt Nothing level orig
  | otherwise
  = Left []

ea_looks_same :: Type -> Type -> Type -> Type -> Bool
-- True if the faulting types (ty1, ty2) look the same as
-- the expected/actual types (exp, act).
-- If so, we don't want to redundantly report the latter
ea_looks_same ty1 ty2 exp act
  = (act `looks_same` ty1 && exp `looks_same` ty2) ||
    (exp `looks_same` ty1 && act `looks_same` ty2)
  where
    looks_same t1 t2 = t1 `pickyEqType` t2
                    || t1 `eqType` liftedTypeKind && t2 `eqType` liftedTypeKind
      -- pickyEqType is sensitive to synonyms, so only replies True
      -- when the types really look the same.  However,
      -- (TYPE 'LiftedRep) and Type both print the same way.

mk_ea_msg :: SolverReportErrCtxt -> Maybe ErrorItem -> TypeOrKind
          -> CtOrigin -> Either [ExpectedActualInfo] MismatchMsg
-- Constructs a "Couldn't match" message
-- The (Maybe ErrorItem) says whether this is the main top-level message (Just)
--     or a supplementary message (Nothing)
mk_ea_msg ctxt at_top level
  (TypeEqOrigin { uo_actual = act, uo_expected = exp, uo_thing = mb_thing })
  | Just thing <- mb_thing
  , KindLevel <- level
  = Right $ KindMismatch { kmismatch_what     = thing
                         , kmismatch_expected = exp
                         , kmismatch_actual   = act }
  | Just item <- at_top
  , let  ea = EA $ if expanded_syns then Just ea_expanded else Nothing
         mismatch = mkBasicMismatchMsg ea item exp act
  = Right mismatch
  | otherwise
  = Left $
    if expanded_syns
    then [ea,ea_expanded]
    else [ea]

  where
    ea = ExpectedActual { ea_expected = exp, ea_actual = act }
    ea_expanded =
      ExpectedActualAfterTySynExpansion
        { ea_expanded_expected = expTy1
        , ea_expanded_actual   = expTy2 }

    expanded_syns = cec_expand_syns ctxt
                 && not (expTy1 `pickyEqType` exp && expTy2 `pickyEqType` act)
    (expTy1, expTy2) = expandSynonymsToMatch exp act
mk_ea_msg _ _ _ _ = Left []

{- Note [Expanding type synonyms to make types similar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In type error messages, if -fprint-expanded-types is used, we want to expand
type synonyms to make expected and found types as similar as possible, but we
shouldn't expand types too much to make type messages even more verbose and
harder to understand. The whole point here is to make the difference in expected
and found types clearer.

`expandSynonymsToMatch` does this, it takes two types, and expands type synonyms
only as much as necessary. Given two types t1 and t2:

  * If they're already same, it just returns the types.

  * If they're in form `C1 t1_1 .. t1_n` and `C2 t2_1 .. t2_m` (C1 and C2 are
    type constructors), it expands C1 and C2 if they're different type synonyms.
    Then it recursively does the same thing on expanded types. If C1 and C2 are
    same, then it applies the same procedure to arguments of C1 and arguments of
    C2 to make them as similar as possible.

    Most important thing here is to keep number of synonym expansions at
    minimum. For example, if t1 is `T (T3, T5, Int)` and t2 is `T (T5, T3,
    Bool)` where T5 = T4, T4 = T3, ..., T1 = X, it returns `T (T3, T3, Int)` and
    `T (T3, T3, Bool)`.

  * Otherwise types don't have same shapes and so the difference is clearly
    visible. It doesn't do any expansions and show these types.

Note that we only expand top-layer type synonyms. Only when top-layer
constructors are the same we start expanding inner type synonyms.

Suppose top-layer type synonyms of t1 and t2 can expand N and M times,
respectively. If their type-synonym-expanded forms will meet at some point (i.e.
will have same shapes according to `sameShapes` function), it's possible to find
where they meet in O(N+M) top-layer type synonym expansions and O(min(N,M))
comparisons. We first collect all the top-layer expansions of t1 and t2 in two
lists, then drop the prefix of the longer list so that they have same lengths.
Then we search through both lists in parallel, and return the first pair of
types that have same shapes. Inner types of these two types with same shapes
are then expanded using the same algorithm.

In case they don't meet, we return the last pair of types in the lists, which
has top-layer type synonyms completely expanded. (in this case the inner types
are not expanded at all, as the current form already shows the type error)
-}

-- | Expand type synonyms in given types only enough to make them as similar as
-- possible. Returned types are the same in terms of used type synonyms.
--
-- To expand all synonyms, see 'Type.expandTypeSynonyms'.
--
-- See `ExpandSynsFail` tests in tests testsuite/tests/typecheck/should_fail for
-- some examples of how this should work.
expandSynonymsToMatch :: Type -> Type -> (Type, Type)
expandSynonymsToMatch ty1 ty2 = (ty1_ret, ty2_ret)
  where
    (ty1_ret, ty2_ret) = go ty1 ty2

    -- Returns (type synonym expanded version of first type,
    --          type synonym expanded version of second type)
    go :: Type -> Type -> (Type, Type)
    go t1 t2
      | t1 `pickyEqType` t2 =
        -- Types are same, nothing to do
        (t1, t2)

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , tys1 `equalLength` tys2 =
        -- Type constructors are same. They may be synonyms, but we don't
        -- expand further. The lengths of tys1 and tys2 must be equal;
        -- for example, with type S a = a, we don't want
        -- to zip (S Monad Int) and (S Bool).
        let (tys1', tys2') = unzip (zipWithEqual go tys1 tys2)
         in (TyConApp tc1 tys1', TyConApp tc2 tys2')

    go (AppTy t1_1 t1_2) (AppTy t2_1 t2_2) =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in (mkAppTy t1_1' t1_2', mkAppTy t2_1' t2_2')

    go ty1@(FunTy _ w1 t1_1 t1_2) ty2@(FunTy _ w2 t2_1 t2_2) | w1 `eqType` w2 =
      let (t1_1', t2_1') = go t1_1 t2_1
          (t1_2', t2_2') = go t1_2 t2_2
       in ( ty1 { ft_arg = t1_1', ft_res = t1_2' }
          , ty2 { ft_arg = t2_1', ft_res = t2_2' })

    go (ForAllTy b1 t1) (ForAllTy b2 t2) =
      -- NOTE: We may have a bug here, but we just can't reproduce it easily.
      -- See D1016 comments for details and our attempts at producing a test
      -- case. Short version: We probably need RnEnv2 to really get this right.
      let (t1', t2') = go t1 t2
       in (ForAllTy b1 t1', ForAllTy b2 t2')

    go (CastTy ty1 _) ty2 = go ty1 ty2
    go ty1 (CastTy ty2 _) = go ty1 ty2

    go t1 t2 =
      -- See Note [Expanding type synonyms to make types similar] for how this
      -- works
      let
        t1_exp_tys = t1 : tyExpansions t1
        t2_exp_tys = t2 : tyExpansions t2
        t1_exps    = length t1_exp_tys
        t2_exps    = length t2_exp_tys
        dif        = abs (t1_exps - t2_exps)
      in
        followExpansions $
          zipEqual
            (if t1_exps > t2_exps then drop dif t1_exp_tys else t1_exp_tys)
            (if t2_exps > t1_exps then drop dif t2_exp_tys else t2_exp_tys)

    -- Expand the top layer type synonyms repeatedly, collect expansions in a
    -- list. The list does not include the original type.
    --
    -- Example, if you have:
    --
    --   type T10 = T9
    --   type T9  = T8
    --   ...
    --   type T0  = Int
    --
    -- `tyExpansions T10` returns [T9, T8, T7, ..., Int]
    --
    -- This only expands the top layer, so if you have:
    --
    --   type M a = Maybe a
    --
    -- `tyExpansions (M T10)` returns [Maybe T10] (T10 is not expanded)
    tyExpansions :: Type -> [Type]
    tyExpansions = unfoldr (\t -> (\x -> (x, x)) `fmap` coreView t)

    -- Drop the type pairs until types in a pair look alike (i.e. the outer
    -- constructors are the same).
    followExpansions :: [(Type, Type)] -> (Type, Type)
    followExpansions [] = pprPanic "followExpansions" empty
    followExpansions [(t1, t2)]
      | sameShapes t1 t2 = go t1 t2 -- expand subtrees
      | otherwise        = (t1, t2) -- the difference is already visible
    followExpansions ((t1, t2) : tss)
      -- Traverse subtrees when the outer shapes are the same
      | sameShapes t1 t2 = go t1 t2
      -- Otherwise follow the expansions until they look alike
      | otherwise = followExpansions tss

    sameShapes :: Type -> Type -> Bool
    sameShapes AppTy{}          AppTy{}          = True
    sameShapes (TyConApp tc1 _) (TyConApp tc2 _) = tc1 == tc2
    sameShapes (FunTy {})       (FunTy {})       = True
    sameShapes (ForAllTy {})    (ForAllTy {})    = True
    sameShapes (CastTy ty1 _)   ty2              = sameShapes ty1 ty2
    sameShapes ty1              (CastTy ty2 _)   = sameShapes ty1 ty2
    sameShapes _                _                = False

{-
************************************************************************
*                                                                      *
\subsection{Contexts for renaming errors}
*                                                                      *
************************************************************************
-}

inHsDocContext :: HsDocContext -> SDoc
inHsDocContext ctxt = text "In" <+> pprHsDocContext ctxt

pprHsDocContext :: HsDocContext -> SDoc
pprHsDocContext (GenericCtx doc)      = doc
pprHsDocContext (TypeSigCtx vs)       = text "the type signature for" <+> ppr_sig_bndrs vs
pprHsDocContext (StandaloneKindSigCtx v)= text "the standalone kind signature for" <+> quotes (ppr v)
pprHsDocContext PatCtx                = text "a pattern type-signature"
pprHsDocContext SpecInstSigCtx        = text "a SPECIALISE instance pragma"
pprHsDocContext DefaultDeclCtx        = text "a `default' declaration"
pprHsDocContext DerivDeclCtx          = text "a deriving declaration"
pprHsDocContext (RuleCtx name)        = text "the rewrite rule" <+> doubleQuotes (ftext name)
pprHsDocContext (SpecECtx name)       = text "the SPECIALISE pragma for" <+> quotes (ppr name)
pprHsDocContext (TyDataCtx tycon)     = text "the data type declaration for" <+> quotes (ppr tycon)
pprHsDocContext (FamPatCtx tycon)     = text "a type pattern of family instance for" <+> quotes (ppr tycon)
pprHsDocContext (TySynCtx name)       = text "the declaration for type synonym" <+> quotes (ppr name)
pprHsDocContext (TyFamilyCtx name)    = text "the declaration for type family" <+> quotes (ppr name)
pprHsDocContext (ClassDeclCtx name)   = text "the declaration for class" <+> quotes (ppr name)
pprHsDocContext ExprWithTySigCtx      = text "an expression type signature"
pprHsDocContext TypBrCtx              = text "a Template-Haskell quoted type"
pprHsDocContext HsTypeCtx             = text "a type argument"
pprHsDocContext HsTypePatCtx          = text "a type argument in a pattern"
pprHsDocContext GHCiCtx               = text "GHCi input"
pprHsDocContext (SpliceTypeCtx hs_ty) = text "the spliced type" <+> quotes (ppr hs_ty)
pprHsDocContext ReifyInstancesCtx      = text "GHC.Tc.Gen.Splice.reifyInstances"
pprHsDocContext (ClassInstanceCtx inst_ty) =
  text "the instance declaration for" <+> quotes (ppr inst_ty)
pprHsDocContext (ClassMethodSigCtx name) = text "a class method signature for" <+> quotes (ppr name)
pprHsDocContext (SpecialiseSigCtx name) = text "a SPECIALISE signature for" <+> quotes (ppr name)
pprHsDocContext (PatSynSigCtx vs) =
  text "a pattern synonym signature for" <+> ppr_sig_bndrs vs

pprHsDocContext (ForeignDeclCtx name)
   = text "the foreign declaration for" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx [name])
   = text "the definition of data constructor" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx names)
   = text "the definition of data constructors" <+> interpp'SP names

ppr_sig_bndrs :: [LocatedN RdrName] -> SDoc
ppr_sig_bndrs bs = quotes (pprWithCommas ppr bs)

pprConversionFailReason :: ConversionFailReason -> SDoc
pprConversionFailReason = \case
  IllegalOccName ctxt_ns occ ->
    text "Illegal" <+> pprNameSpace ctxt_ns
    <+> text "name:" <+> quotes (text occ)
  SumAltArityExceeded alt arity ->
    text "Sum alternative" <+> int alt
    <+> text "exceeds its arity," <+> int arity
  IllegalSumAlt alt ->
    vcat [ text "Illegal sum alternative:" <+> int alt
         , nest 2 $ text "Sum alternatives must start from 1" ]
  IllegalSumArity arity ->
    vcat [ text "Illegal sum arity:" <+> int arity
         , nest 2 $ text "Sums must have an arity of at least 2" ]
  MalformedType typeOrKind ty ->
    text "Malformed " <> text ty_str <+> text (show ty)
    where ty_str = case typeOrKind of
                     TypeLevel -> "type"
                     KindLevel -> "kind"
  IllegalLastStatement do_or_lc stmt ->
    vcat [ text "Illegal last statement of" <+> pprAHsDoFlavour do_or_lc <> colon
         , nest 2 $ ppr stmt
         , text "(It should be an expression.)" ]
  KindSigsOnlyAllowedOnGADTs ->
    text "Kind signatures are only allowed on GADTs"
  IllegalDeclaration declDescr bad_decls ->
    sep [ text "Illegal" <+> what <+> text "in" <+> descrDoc <> colon
        , nest 2 bads ]
    where
      (what, bads) = case bad_decls of
        IllegalDecls (NE.toList -> decls) ->
            (text "declaration" <> plural decls, vcat $ map ppr decls)
        IllegalFamDecls (NE.toList -> decls) ->
            ( text "family declaration" <> plural decls, vcat $ map ppr decls)
      descrDoc = text $ case declDescr of
                   InstanceDecl -> "an instance declaration"
                   WhereClause -> "a where clause"
                   LetBinding -> "a let expression"
                   LetExpression -> "a let expression"
                   ClssDecl -> "a class declaration"
  CannotMixGADTConsWith98Cons ->
    text "Cannot mix GADT constructors with Haskell 98"
    <+> text "constructors"
  EmptyStmtListInDoBlock ->
    text "Empty stmt list in do-block"
  NonVarInInfixExpr ->
    text "Non-variable expression is not allowed in an infix expression"
  MultiWayIfWithoutAlts ->
    text "Multi-way if-expression with no alternatives"
  CasesExprWithoutAlts ->
    text "\\cases expression with no alternatives"
  ImplicitParamsWithOtherBinds ->
    text "Implicit parameters mixed with other bindings"
  InvalidCCallImpent from ->
    text (show from) <+> text "is not a valid ccall impent"
  RecGadtNoCons ->
    quotes (text "RecGadtC") <+> text "must have at least one constructor name"
  GadtNoCons ->
    quotes (text "GadtC") <+> text "must have at least one constructor name"
  InvalidTypeInstanceHeader tys ->
    text "Invalid type instance header:"
    <+> text (show tys)
  InvalidTyFamInstLHS lhs ->
    text "Invalid type family instance LHS:"
    <+> text (show lhs)
  InvalidImplicitParamBinding ->
    text "Implicit parameter binding only allowed in let or where"
  DefaultDataInstDecl adts ->
    (text "Default data instance declarations"
    <+> text "are not allowed:")
      $$ ppr adts
  FunBindLacksEquations nm ->
    text "Function binding for"
    <+> quotes (text (TH.pprint nm))
    <+> text "has no equations"
  EmptyGuard ->
    text "Empty guard"
  EmptyParStmt ->
    text "Empty par stmt"

pprTyThingUsedWrong :: WrongThingSort -> TcTyThing -> Name -> SDoc
pprTyThingUsedWrong sort thing name =
  pprTcTyThingCategory thing <+> quotes (ppr name) <+>
  text "used as a" <+> pprWrongThingSort sort

pprWrongThingSort :: WrongThingSort -> SDoc
pprWrongThingSort =
  text . \case
    WrongThingType -> "type"
    WrongThingDataCon -> "data constructor"
    WrongThingPatSyn -> "pattern synonym"
    WrongThingConLike -> "constructor-like thing"
    WrongThingClass -> "class"
    WrongThingTyCon -> "type constructor"
    WrongThingAxiom -> "axiom"

pprStageCheckReason :: StageCheckReason -> SDoc
pprStageCheckReason = \case
  StageCheckInstance _ t ->
    text "instance for" <+> quotes (ppr t)
  StageCheckSplice t ->
    quotes (ppr t)

pprUninferrableTyVarCtx :: UninferrableTyVarCtx -> SDoc
pprUninferrableTyVarCtx = \case
  UninfTyCtx_ClassContext theta ->
    sep [ text "the class context:", pprTheta theta ]
  UninfTyCtx_DataContext theta ->
    sep [ text "the datatype context:", pprTheta theta ]
  UninfTyCtx_ProvidedContext theta ->
    sep [ text "the provided context:" , pprTheta theta ]
  UninfTyCtx_TyFamRhs rhs_ty ->
    sep [ text "the type family equation right-hand side:" , ppr rhs_ty ]
  UninfTyCtx_TySynRhs rhs_ty ->
    sep [ text "the type synonym right-hand side:" , ppr rhs_ty ]
  UninfTyCtx_Sig exp_kind full_hs_ty ->
    hang (text "the kind" <+> ppr exp_kind) 2
         (text "of the type signature:" <+> ppr full_hs_ty)

pprPatSynInvalidRhsReason :: PatSynInvalidRhsReason -> SDoc
pprPatSynInvalidRhsReason = \case
  PatSynNotInvertible p ->
    text "Pattern" <+> quotes (ppr p) <+> text "is not invertible"
  PatSynUnboundVar var ->
    quotes (ppr var) <+> text "is not bound by the LHS of the pattern synonym"

pprBadFieldAnnotationReason :: BadFieldAnnotationReason -> SDoc
pprBadFieldAnnotationReason = \case
  LazyFieldsDisabled ->
    text "Lazy field annotations (~) are disabled"
  UnpackWithoutStrictness ->
    text "UNPACK pragma lacks '!'"
  BackpackUnpackAbstractType ->
    text "Ignoring unusable UNPACK pragma"

pprSuperclassCycleDetail :: SuperclassCycleDetail -> SDoc
pprSuperclassCycleDetail = \case
  SCD_HeadTyVar pred ->
    hang (text "one of whose superclass constraints is headed by a type variable:")
       2 (quotes (ppr pred))
  SCD_HeadTyFam pred ->
    hang (text "one of whose superclass constraints is headed by a type family:")
       2 (quotes (ppr pred))
  SCD_Superclass cls ->
    text "one of whose superclasses is" <+> quotes (ppr cls)

pprRoleValidationFailedReason :: Role -> RoleValidationFailedReason -> SDoc
pprRoleValidationFailedReason role = \case
  TyVarRoleMismatch tv role' ->
    text "type variable" <+> quotes (ppr tv) <+>
    text "cannot have role" <+> ppr role <+>
    text "because it was assigned role" <+> ppr role'
  TyVarMissingInEnv tv ->
    text "type variable" <+> quotes (ppr tv) <+> text "missing in environment"
  BadCoercionRole co ->
    text "coercion" <+> ppr co <+> text "has bad role" <+> ppr role

pprDisabledClassExtension :: Class -> DisabledClassExtension -> SDoc
pprDisabledClassExtension cls = \case
  MultiParamDisabled n ->
    text howMany <+> text "parameters for class" <+> quotes (ppr cls)
    where
      howMany | n == 0 = "No"
              | otherwise = "Too many"
  FunDepsDisabled ->
    text "Fundeps in class" <+> quotes (ppr cls)
  ConstrainedClassMethodsDisabled sel_id pred ->
    vcat [ hang (text "Constraint" <+> quotes (ppr pred)
                 <+> text "in the type of" <+> quotes (ppr sel_id))
              2 (text "constrains only the class type variables")]

pprImportLookup :: ImportLookupReason -> SDoc
pprImportLookup = \case
  ImportLookupBad k iface decl_spec ie _ps ->
    let
      pprImpDeclSpec :: ModIface -> ImpDeclSpec -> SDoc
      pprImpDeclSpec iface decl_spec =
        quotes (ppr (moduleName $ is_mod decl_spec)) <+> case mi_boot iface of
            IsBoot  -> text "(hi-boot interface)"
            NotBoot -> empty
      withContext msgs =
        hang (text "In the import of" <+> pprImpDeclSpec iface decl_spec <> colon)
          2 (vcat msgs)
    in case k of
      BadImportNotExported _ ->
        vcat
          [ text "Module" <+> pprImpDeclSpec iface decl_spec <+>
            text "does not export" <+> quotes (ppr ie) <> dot
          ]
      BadImportAvailVar ->
        withContext
          [ text "an item called"
              <+> quotes val <+> text "is exported, but it is not a type."
          ]
        where
          val_occ = rdrNameOcc $ ieName ie
          val = parenSymOcc val_occ (ppr val_occ)
      BadImportAvailTyCon {} ->
        withContext
          [ text "an item called"
            <+> quotes tycon <+> text "is exported, but it is a type."
          ]
        where
          tycon_occ = rdrNameOcc $ ieName ie
          tycon = parenSymOcc tycon_occ (ppr tycon_occ)
      BadImportNotExportedSubordinates ns ->
        withContext
          [ text "an item called" <+> quotes sub <+> text "is exported, but it does not export any children"
          , text "(constructors, class methods or field names) called"
          <+> pprWithCommas (quotes . ppr) ns <> dot
          ]
          where
            sub_occ = rdrNameOcc $ ieName ie
            sub = parenSymOcc sub_occ (ppr sub_occ)
      BadImportAvailDataCon dataType_occ ->
        withContext
          [ text "an item called" <+> quotes datacon
          , text "is exported, but it is a data constructor of"
          , quotes dataType <> dot
          ]
          where
            datacon_occ = rdrNameOcc $ ieName ie
            datacon = parenSymOcc datacon_occ (ppr datacon_occ)
            dataType = parenSymOcc dataType_occ (ppr dataType_occ)
  ImportLookupQualified rdr ->
    hang (text "Illegal qualified name in import item:")
       2 (ppr rdr)
  ImportLookupIllegal ->
    text "Illegal import item"
  ImportLookupAmbiguous rdr gres ->
    hang (text "Ambiguous name" <+> quotes (ppr rdr) <+> text "in import item. It could refer to:")
       2 (vcat (map (ppr . greOccName) gres))

pprUnusedImport :: ImportDecl GhcRn -> UnusedImportReason -> SDoc
pprUnusedImport decl = \case
  UnusedImportNone ->
    vcat [ pp_herald <+> quotes pp_mod <+> text "is redundant"
         , nest 2 (text "except perhaps to import instances from"
                   <+> quotes pp_mod)
         , text "To import instances alone, use:"
           <+> text "import" <+> pp_mod <> parens empty ]
  UnusedImportSome sort_unused ->
    sep [ pp_herald <+> quotes (pprWithCommas pp_unused sort_unused)
        , text "from module" <+> quotes pp_mod <+> text "is redundant"]
  where
    pp_mod = ppr (unLoc (ideclName decl))
    pp_herald = text "The" <+> pp_qual <+> text "import of"
    pp_qual
      | isImportDeclQualified (ideclQualified decl) = text "qualified"
      | otherwise                                   = empty
    pp_unused = \case
      UnusedImportNameRegular n ->
        pprNameUnqualified n
      UnusedImportNameRecField par fld_occ ->
        case par of
          ParentIs p -> pprNameUnqualified p <> parens (ppr fld_occ)
          NoParent   -> ppr fld_occ

pprUnusedName :: OccName -> UnusedNameProv -> SDoc
pprUnusedName name reason =
  sep [ msg <> colon
      , nest 2 $ pprNonVarNameSpace (occNameSpace name)
                 <+> quotes (ppr name)]
  where
    msg = case reason of
      UnusedNameTopDecl ->
        defined
      UnusedNameImported mod ->
        text "Imported from" <+> quotes (ppr mod) <+> text "but not used"
      UnusedNameTypePattern ->
        defined <+> text "on the right hand side"
      UnusedNameMatch ->
        defined
      UnusedNameLocalBind ->
        defined
    defined = text "Defined but not used"

-- When printing the name, take care to qualify it in the same
-- way as the provenance reported by pprNameProvenance, namely
-- the head of 'gre_imp'.  Otherwise we get confusing reports like
--   Ambiguous occurrence ‘null’
--   It could refer to either ‘T15487a.null’,
--                            imported from ‘Prelude’ at T15487.hs:1:8-13
--                     or ...
-- See #15487
pprAmbiguousGreName :: GlobalRdrEnv -> GlobalRdrElt -> SDoc
pprAmbiguousGreName gre_env gre
  | IAmRecField fld_info <- greInfo gre
  = sep [ text "the field" <+> quotes (ppr occ) <+> parent_info fld_info <> comma
        , pprNameProvenance gre ]
  | otherwise
  = sep [ quotes (pp_qual <> dot <> ppr occ) <> comma
        , pprNameProvenance gre ]

  where
    occ = greOccName gre
    parent_info fld_info =
      case first_con of
        PatSynName  ps -> text "of pattern synonym" <+> quotes (ppr ps)
        DataConName {} ->
          case greParent gre of
            ParentIs par
              -- For a data family, only reporting the family TyCon can be
              -- unhelpful (see T23301). So we give a bit of additional
              -- info in that case.
              | Just par_gre <- lookupGRE_Name gre_env par
              , IAmTyCon tc_flav <- greInfo par_gre
              , OpenFamilyFlavour (IAmData {}) _ <- tc_flav
              -> vcat [ ppr_cons
                      , text "in a data family instance of" <+> quotes (ppr par) ]
              | otherwise
              -> text "of record" <+> quotes (ppr par)
            NoParent -> ppr_cons
      where
        cons :: [ConLikeName]
        cons = nonDetEltsUniqSet $ recFieldCons fld_info
        first_con :: ConLikeName
        first_con = head cons
        ppr_cons :: SDoc
        ppr_cons = hsep [ text "belonging to data constructor"
                        , quotes (ppr $ nameOccName $ conLikeName_Name first_con)
                        , if length cons > 1 then parens (text "among others") else empty
                        ]
    pp_qual
        | gre_lcl gre
        = ppr (nameModule $ greName gre)
        | Just imp  <- headMaybe $ gre_imp gre
            -- This 'imp' is the one that
            -- pprNameProvenance chooses
        , ImpDeclSpec { is_as = mod } <- is_decl imp
        = ppr mod
        | otherwise
        = pprPanic "addNameClassErrRn" (ppr gre)
          -- Invariant: either 'lcl' is True or 'iss' is non-empty

pprNonCanonicalDefinition :: LHsSigType GhcRn
                          -> NonCanonicalDefinition
                          -> SDoc
pprNonCanonicalDefinition inst_ty = \case
  NonCanonicalMonoid sub -> case sub of
    NonCanonical_Sappend ->
      msg1 "(<>)" "mappend"
    NonCanonical_Mappend ->
      msg2 "mappend" "(<>)"
  NonCanonicalMonad sub -> case sub of
    NonCanonical_Pure ->
      msg1 "pure" "return"
    NonCanonical_ThenA ->
      msg1 "(*>)" "(>>)"
    NonCanonical_Return ->
      msg2 "return" "pure"
    NonCanonical_ThenM ->
      msg2 "(>>)" "(*>)"
  where
    msg1 :: String -> String -> SDoc
    msg1 lhs rhs =
      vcat [ text "Noncanonical" <+>
            quotes (text (lhs ++ " = " ++ rhs)) <+>
            text "definition detected"
          , inst
          ]

    msg2 :: String -> String -> SDoc
    msg2 lhs rhs =
      vcat [ text "Noncanonical" <+>
            quotes (text lhs) <+>
            text "definition detected"
          , inst
          , quotes (text lhs) <+>
            text "will eventually be removed in favour of" <+>
            quotes (text rhs)
          ]

    inst = instDeclCtxt1 inst_ty

    -- stolen from GHC.Tc.TyCl.Instance
    instDeclCtxt1 :: LHsSigType GhcRn -> SDoc
    instDeclCtxt1 hs_inst_ty
      = inst_decl_ctxt (ppr (getLHsInstDeclHead hs_inst_ty))

    inst_decl_ctxt :: SDoc -> SDoc
    inst_decl_ctxt doc = hang (text "in the instance declaration for")
                         2 (quotes doc <> text ".")

suggestNonCanonicalDefinition :: NonCanonicalDefinition -> [GhcHint]
suggestNonCanonicalDefinition reason =
  [action doc]
  where
    action = case reason of
      NonCanonicalMonoid sub -> case sub of
        NonCanonical_Sappend -> move sappendName mappendName
        NonCanonical_Mappend -> remove mappendName sappendName
      NonCanonicalMonad sub -> case sub of
        NonCanonical_Pure -> move pureAName returnMName
        NonCanonical_ThenA -> move thenAName thenMName
        NonCanonical_Return -> remove returnMName pureAName
        NonCanonical_ThenM -> remove thenMName thenAName

    move = SuggestMoveNonCanonicalDefinition
    remove = SuggestRemoveNonCanonicalDefinition

    doc = case reason of
      NonCanonicalMonoid _ -> doc_monoid
      NonCanonicalMonad _ -> doc_monad

    doc_monoid =
      "https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/semigroup-monoid"
    doc_monad =
      "https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return"

suggestDefaultDeclaration :: Class-> [Type] -> [[Type]] -> [GhcHint]
suggestDefaultDeclaration cls prefix seqs =
  [SuggestDefaultDeclaration cls $ supersequence (prefix : seqs)]
  where
    -- Not exactly the shortest possible supersequence, but it preserves
    -- the head sequence as the prefix of the result which is a requirement.
    supersequence :: [[Type]] -> [Type]
    supersequence [] = []
    supersequence ([] : seqs) = supersequence seqs
    supersequence ((x : xs) : seqs) =
      x : supersequence (xs : (dropHead x <$> seqs))
    dropHead x ys@(y : ys')
      | tcEqType x y = ys'
      | otherwise = ys
    dropHead _ [] = []

--------------------------------------------------------------------------------
-- hs-boot mismatch errors

pprBootMismatch :: HsBootOrSig -> BootMismatch -> SDoc
pprBootMismatch boot_or_sig = \case
  MissingBootThing nm err ->
    let def_or_exp = case err of
          MissingBootDefinition -> text "defined in"
          MissingBootExport     -> text "exported by"
    in quotes (ppr nm) <+> text "is exported by the"
       <+> ppr_boot_or_sig <> comma
       <+> text "but not"
       <+> def_or_exp <+> text "the implementing module."
  MissingBootInstance boot_dfun ->
    hang (text "instance" <+> ppr (idType boot_dfun))
       2 (text "is defined in the" <+> ppr ppr_boot_or_sig <> comma <+>
          text "but not in the implementing module.")
  BadReexportedBootThing name name' ->
    withUserStyle alwaysQualify AllTheWay $ vcat
        [ text "The" <+> ppr_boot_or_sig
           <+> text "(re)exports" <+> quotes (ppr name)
        , text "but the implementing module exports a different identifier" <+> quotes (ppr name')
        ]
  BootMismatch boot_thing real_thing err ->
    vcat
      [ ppr real_thing <+>
        text "has conflicting definitions in the module"
      , text "and its" <+> ppr_boot_or_sig <> dot,
                    text "Main module:" <+> real_doc
      , (case boot_or_sig of
          HsBoot -> text "  Boot file:"
          Hsig   -> text "  Hsig file:") <+> boot_doc
      , pprBootMismatchWhat boot_or_sig err
      ]
      where
        to_doc
          = pprTyThingInContext $
            showToHeader
              { ss_forall =
                  case boot_or_sig of
                    HsBoot -> ShowForAllMust
                    Hsig   -> ShowForAllWhen }

        real_doc = to_doc real_thing
        boot_doc = to_doc boot_thing

  where
    ppr_boot_or_sig = case boot_or_sig of
      HsBoot -> text "hs-boot file"
      Hsig   -> text "hsig file"


pprBootMismatchWhat :: HsBootOrSig -> BootMismatchWhat -> SDoc
pprBootMismatchWhat boot_or_sig = \case
  BootMismatchedIdTypes {} ->
    text "The two types are different."
  BootMismatchedTyCons tc1 tc2 errs ->
    vcat $ map (pprBootTyConMismatch boot_or_sig tc1 tc2) (NE.toList errs)

pprBootTyConMismatch :: HsBootOrSig -> TyCon -> TyCon
                     -> BootTyConMismatch -> SDoc
pprBootTyConMismatch boot_or_sig tc1 tc2 = \case
  TyConKindMismatch ->
    text "The types have different kinds."
  TyConRoleMismatch sub_type ->
    if sub_type
    then
      text "The roles are not compatible:" $$
      text "Main module:" <+> ppr (tyConRoles tc1) $$
      text "  Hsig file:" <+> ppr (tyConRoles tc2)
    else
      text "The roles do not match." $$
      if boot_or_sig == HsBoot
      then note $ "Roles on abstract types default to" <+> quotes "representational" <+> "in hs-boot files"
      else empty
  TyConSynonymMismatch {} -> empty -- nothing interesting to say
  TyConFlavourMismatch fam_flav1 fam_flav2 ->
    whenPprDebug $
      text "Family flavours" <+> ppr fam_flav1 <+> text "and" <+> ppr fam_flav2 <+>
      text "do not match"
  TyConAxiomMismatch ax_errs ->
    pprBootListMismatches (text "Type family equations do not match:")
      pprTyConAxiomMismatch ax_errs
  TyConInjectivityMismatch {} ->
    text "Injectivity annotations do not match"
  TyConMismatchedClasses _ _ err ->
    pprBootClassMismatch boot_or_sig err
  TyConMismatchedData _rhs1 _rhs2 err ->
    pprBootDataMismatch err
  SynAbstractData err ->
    pprSynAbstractDataError err
  TyConsVeryDifferent ->
    empty -- should be obvious to the user what the problem is

pprSynAbstractDataError :: SynAbstractDataError -> SDoc
pprSynAbstractDataError = \case
  SynAbsDataTySynNotNullary ->
    text "Illegal parameterized type synonym in implementation of abstract data."
  SynAbstractDataInvalidRHS bad_sub_tys ->
    let msgs = mapMaybe pprInvalidAbstractSubTy (NE.toList bad_sub_tys)
    in  case msgs of
      []     -> herald <> dot
      msg:[] -> hang (herald <> colon)
                   2 msg
      _      -> hang (herald <> colon)
                   2 (vcat $ map (<+> bullet) msgs)

  where
    herald = text "Illegal implementation of abstract data"
    pprInvalidAbstractSubTy = \case
      TyConApp tc _
        -> assertPpr (isTypeFamilyTyCon tc) (ppr tc) $
           Just $ text "Invalid type family" <+> quotes (ppr tc) <> dot
      ty@(ForAllTy {})
        -> Just $ text "Invalid polymorphic type" <> colon <+> ppr ty <> dot
      ty@(FunTy af _ _ _)
        | not (af == FTF_T_T)
        -> Just $ text "Invalid qualified type" <> colon <+> ppr ty <> dot
      _ -> Nothing

pprTyConAxiomMismatch :: BootListMismatch CoAxBranch BootAxiomBranchMismatch -> SDoc
pprTyConAxiomMismatch = \case
  MismatchedLength ->
    text "The number of equations differs."
  MismatchedThing i br1 br2 err ->
    hang (text "The" <+> speakNth (i+1) <+> text "equations do not match.")
       2 (pprCoAxBranchMismatch br1 br2 err)

pprCoAxBranchMismatch :: CoAxBranch -> CoAxBranch -> BootAxiomBranchMismatch -> SDoc
pprCoAxBranchMismatch _br1 _br2 err =
  text "The" <+> what <+> text "don't match."
  where
    what = case err of
      MismatchedAxiomBinders -> text "variables bound in the equation"
      MismatchedAxiomLHS     -> text "equation left-hand sides"
      MismatchedAxiomRHS     -> text "equation right-hand sides"

pprBootListMismatches :: SDoc -- ^ herald
                      -> (BootListMismatch item err -> SDoc)
                      -> BootListMismatches item err -> SDoc
pprBootListMismatches herald ppr_one errs =
  hang herald 2 msgs
  where
    msgs = case errs of
      err :| [] -> ppr_one err
      _         -> vcat $ map ((bullet <+>) . ppr_one) $ NE.toList errs

pprBootClassMismatch :: HsBootOrSig -> BootClassMismatch -> SDoc
pprBootClassMismatch boot_or_sig = \case
  MismatchedMethods errs ->
    pprBootListMismatches (text "The class methods do not match:")
      pprBootClassMethodListMismatch errs
  MismatchedATs at_errs ->
    pprBootListMismatches (text "The associated types do not match:")
      (pprATMismatch boot_or_sig) at_errs
  MismatchedFunDeps ->
    text "The functional dependencies do not match."
  MismatchedSuperclasses ->
    text "The superclass constraints do not match."
  MismatchedMinimalPragmas ->
    text "The MINIMAL pragmas are not compatible."

pprATMismatch :: HsBootOrSig -> BootListMismatch ClassATItem BootATMismatch -> SDoc
pprATMismatch boot_or_sig = \case
  MismatchedLength ->
    text "The number of associated type defaults differs."
  MismatchedThing i at1 at2 err ->
    pprATMismatchErr boot_or_sig i at1 at2 err

pprATMismatchErr :: HsBootOrSig -> Int -> ClassATItem -> ClassATItem -> BootATMismatch -> SDoc
pprATMismatchErr boot_or_sig i (ATI tc1 _) (ATI tc2 _) = \case
  MismatchedTyConAT err ->
    hang (text "The associated types differ:")
       2 $ pprBootTyConMismatch boot_or_sig tc1 tc2 err
  MismatchedATDefaultType ->
    text "The types of the" <+> speakNth (i+1) <+>
    text "associated type default differ."

pprBootClassMethodListMismatch :: BootListMismatch ClassOpItem BootMethodMismatch -> SDoc
pprBootClassMethodListMismatch = \case
  MismatchedLength ->
    text "The number of class methods differs."
  MismatchedThing _ op1 op2 err ->
    pprBootClassMethodMismatch op1 op2 err

pprBootClassMethodMismatch :: ClassOpItem -> ClassOpItem -> BootMethodMismatch -> SDoc
pprBootClassMethodMismatch (op1, _) (op2, _) = \case
  MismatchedMethodNames ->
    text "The method names" <+> quotes pname1 <+> text "and"
                            <+> quotes pname2 <+> text "differ."
  MismatchedMethodTypes {} ->
    text "The types of" <+> pname1 <+> text "are different."
  MismatchedDefaultMethods subtype_check ->
    if subtype_check
    then
      text "The default methods associated with" <+> pname1 <+>
      text "are not compatible."
    else
      text "The default methods associated with" <+> pname1 <+>
      text "are different."
  where
    nm1 = idName op1
    nm2 = idName op2
    pname1 = quotes (ppr nm1)
    pname2 = quotes (ppr nm2)

pprBootDataMismatch :: BootDataMismatch -> SDoc
pprBootDataMismatch = \case
  MismatchedNewtypeVsData ->
    text "Cannot match a" <+> quotes (text "data") <+>
    text "definition with a" <+> quotes (text "newtype") <+>
    text "definition."
  MismatchedConstructors dc_errs ->
    pprBootListMismatches (text "The constructors do not match:")
      pprBootDataConMismatch dc_errs
  MismatchedDatatypeContexts {} ->
    text "The datatype contexts do not match."

pprBootDataConMismatch :: BootListMismatch DataCon BootDataConMismatch
                       -> SDoc
pprBootDataConMismatch = \case
  MismatchedLength ->
    text "The number of constructors differs."
  MismatchedThing _ dc1 dc2 err ->
    pprBootDataConMismatchErr dc1 dc2 err

pprBootDataConMismatchErr :: DataCon -> DataCon -> BootDataConMismatch -> SDoc
pprBootDataConMismatchErr dc1 dc2 = \case
  MismatchedDataConNames ->
    text "The names" <+> pname1 <+> text "and" <+> pname2 <+> text "differ."
  MismatchedDataConFixities ->
    text "The fixities of" <+> pname1 <+> text "differ."
  MismatchedDataConBangs ->
    text "The strictness annotations for" <+> pname1 <+> text "differ."
  MismatchedDataConFieldLabels ->
    text "The record label lists for" <+> pname1 <+> text "differ."
  MismatchedDataConTypes ->
    text "The types for" <+> pname1 <+> text "differ."
  where
     name1 = dataConName dc1
     name2 = dataConName dc2
     pname1 = quotes (ppr name1)
     pname2 = quotes (ppr name2)

--------------------------------------------------------------------------------
-- Illegal instance errors

pprIllegalInstance :: IllegalInstanceReason -> SDoc
pprIllegalInstance = \case
  IllegalClassInstance head_ty reason ->
    pprIllegalClassInstanceReason head_ty reason
  IllegalFamilyInstance reason ->
    pprIllegalFamilyInstance reason
  IllegalFamilyApplicationInInstance inst_ty invis_arg tf_tc tf_args ->
    pprWithInvisibleBitsWhen invis_arg $
      hang (text "Illegal type synonym family application"
              <+> quotes (ppr tf_ty) <+> text "in instance" <> colon)
         2 (ppr inst_ty)
      where
        tf_ty = mkTyConApp tf_tc tf_args

pprIllegalClassInstanceReason :: TypedThing -> IllegalClassInstanceReason -> SDoc
pprIllegalClassInstanceReason head_ty = \case
  IllegalInstanceHead reason ->
    pprIllegalInstanceHeadReason head_ty reason
  IllegalHasFieldInstance has_field_err ->
    with_illegal_instance_header head_ty $
      pprIllegalHasFieldInstance has_field_err
  IllegalSpecialClassInstance cls because_safeHaskell ->
    text "Class" <+> quotes (ppr $ className cls)
    <+> text "does not support user-specified instances"
    <> safeHaskell_msg
      where
        safeHaskell_msg
          | because_safeHaskell
          = text " when Safe Haskell is enabled."
          | otherwise
          = dot
  IllegalInstanceFailsCoverageCondition cls coverage_failure ->
    with_illegal_instance_header head_ty $
      pprNotCovered cls coverage_failure

pprIllegalInstanceHeadReason :: TypedThing
                             -> IllegalInstanceHeadReason -> SDoc
pprIllegalInstanceHeadReason head_ty = \case
  InstHeadTySynArgs -> with_illegal_instance_header head_ty $
    text "All instance types must be of the form (T t1 ... tn)" $$
    text "where T is not a synonym."
  InstHeadNonTyVarArgs -> with_illegal_instance_header head_ty $ vcat [
    text "All instance types must be of the form (T a1 ... an)",
    text "where a1 ... an are *distinct type variables*,",
    text "and each type variable appears at most once in the instance head."]
  InstHeadMultiParam -> with_illegal_instance_header head_ty $ parens $
    text "Only one type can be given in an instance head."
  InstHeadAbstractClass cls ->
    text "Cannot define instance for abstract class" <+>
    quotes (ppr cls)
  InstHeadNonClassHead bad_head ->
    vcat [ text "Illegal" <+> what_illegal <> dot
         , text "Instance heads must be of the form"
         , nest 2 $ text "C ty_1 ... ty_n"
         , text "where" <+> quotes (char 'C') <+> text "is a class."
         ]
    where
      what_illegal = case bad_head of
        InstNonClassTyCon tc_nm flav ->
          text "instance for" <+> ppr flav <+> quotes (ppr tc_nm)
        InstNonTyCon ->
          text "head of an instance declaration:" <+> quotes (ppr head_ty)

with_illegal_instance_header :: TypedThing -> SDoc -> SDoc
with_illegal_instance_header head_ty msg =
  hang (hang (text "Illegal instance declaration for")
           2 (quotes (ppr head_ty)) <> colon)
      2 msg

pprIllegalHasFieldInstance :: IllegalHasFieldInstance -> SDoc
pprIllegalHasFieldInstance = \case
  IllegalHasFieldInstanceNotATyCon
    -> text "Record data type must be specified."
  IllegalHasFieldInstanceFamilyTyCon
    -> text "Record data type may not be a data family."
  IllegalHasFieldInstanceTyConHasField tc lbl
    -> quotes (ppr tc) <+> text "already has a field" <+> quotes (ppr lbl) <> dot
  IllegalHasFieldInstanceTyConHasFields tc lbl
    -> sep [ ppr_tc <+> text "has fields, and the type" <+> quotes (ppr lbl)
           , text "could unify with one of the field labels of" <+> ppr_tc <> dot ]
    where ppr_tc = quotes (ppr tc)

pprNotCovered :: Class -> CoverageProblem -> SDoc
pprNotCovered clas
  CoverageProblem
  { not_covered_fundep        = fd
  , not_covered_fundep_inst   = (ls, rs)
  , not_covered_invis_vis_tvs = undetermined_tvs
  , not_covered_liberal       = which_cc_failed
  } =
  pprWithInvisibleBitsWhen (isEmptyVarSet $ pSnd undetermined_tvs) $
    vcat [ sep [ text "The"
                  <+> ppWhen liberal (text "liberal")
                  <+> text "coverage condition fails in class"
                  <+> quotes (ppr clas)
                , nest 2 $ text "for functional dependency:"
                  <+> quotes (pprFunDep fd) ]
          , sep [ text "Reason: lhs type" <> plural ls <+> pprQuotedList ls
                , nest 2 $
                  (if isSingleton ls
                  then text "does not"
                  else text "do not jointly")
                  <+> text "determine rhs type" <> plural rs
                  <+> pprQuotedList rs ]
          , text "Un-determined variable" <> pluralVarSet undet_set <> colon
                  <+> pprVarSet undet_set (pprWithCommas ppr)
          ]
  where
    liberal = case which_cc_failed of
                   FailedLICC   -> True
                   FailedICC {} -> False
    undet_set = fold undetermined_tvs

illegalInstanceHints :: IllegalInstanceReason -> [GhcHint]
illegalInstanceHints = \case
  IllegalClassInstance _ reason ->
    illegalClassInstanceHints reason
  IllegalFamilyInstance reason ->
    illegalFamilyInstanceHints reason
  IllegalFamilyApplicationInInstance {} ->
    noHints

illegalInstanceReason :: IllegalInstanceReason -> DiagnosticReason
illegalInstanceReason = \case
  IllegalClassInstance _ reason ->
    illegalClassInstanceReason reason
  IllegalFamilyInstance reason ->
    illegalFamilyInstanceReason reason
  IllegalFamilyApplicationInInstance {} ->
    ErrorWithoutFlag

illegalClassInstanceHints :: IllegalClassInstanceReason -> [GhcHint]
illegalClassInstanceHints = \case
  IllegalInstanceHead reason ->
    illegalInstanceHeadHints reason
  IllegalHasFieldInstance has_field_err ->
    illegalHasFieldInstanceHints has_field_err
  IllegalSpecialClassInstance {} -> noHints
  IllegalInstanceFailsCoverageCondition _ coverage_failure ->
    failedCoverageConditionHints coverage_failure


illegalClassInstanceReason :: IllegalClassInstanceReason -> DiagnosticReason
illegalClassInstanceReason = \case
  IllegalInstanceHead reason ->
    illegalInstanceHeadReason reason
  IllegalHasFieldInstance has_field_err ->
    illegalHasFieldInstanceReason has_field_err
  IllegalSpecialClassInstance {} -> ErrorWithoutFlag
  IllegalInstanceFailsCoverageCondition _ coverage_failure ->
    failedCoverageConditionReason coverage_failure

illegalInstanceHeadHints :: IllegalInstanceHeadReason -> [GhcHint]
illegalInstanceHeadHints = \case
  InstHeadTySynArgs ->
    [suggestExtension LangExt.TypeSynonymInstances]
  InstHeadNonTyVarArgs ->
    [suggestExtension LangExt.FlexibleInstances]
  InstHeadMultiParam ->
    [suggestExtension LangExt.MultiParamTypeClasses]
  InstHeadAbstractClass {} ->
    noHints
  InstHeadNonClassHead {} ->
    noHints

illegalInstanceHeadReason :: IllegalInstanceHeadReason -> DiagnosticReason
illegalInstanceHeadReason = \case
  -- These are serious
  InstHeadAbstractClass {} ->
    ErrorWithoutFlag
  InstHeadNonClassHead {} ->
    ErrorWithoutFlag

  -- These are less serious (enable an extension)
  InstHeadTySynArgs ->
    ErrorWithoutFlag
  InstHeadNonTyVarArgs ->
    ErrorWithoutFlag
  InstHeadMultiParam ->
    ErrorWithoutFlag

illegalHasFieldInstanceHints :: IllegalHasFieldInstance -> [GhcHint]
illegalHasFieldInstanceHints = \case
  IllegalHasFieldInstanceNotATyCon
    -> noHints
  IllegalHasFieldInstanceFamilyTyCon
    -> noHints
  IllegalHasFieldInstanceTyConHasField {}
    -> noHints
  IllegalHasFieldInstanceTyConHasFields {}
    -> noHints

illegalHasFieldInstanceReason :: IllegalHasFieldInstance -> DiagnosticReason
illegalHasFieldInstanceReason = \case
  IllegalHasFieldInstanceNotATyCon
    -> ErrorWithoutFlag
  IllegalHasFieldInstanceFamilyTyCon
    -> ErrorWithoutFlag
  IllegalHasFieldInstanceTyConHasField {}
    -> ErrorWithoutFlag
  IllegalHasFieldInstanceTyConHasFields {}
    -> ErrorWithoutFlag

failedCoverageConditionHints :: CoverageProblem -> [GhcHint]
failedCoverageConditionHints (CoverageProblem { not_covered_liberal = failed_cc })
  = case failed_cc of
      FailedLICC -> noHints
      FailedICC { alsoFailedLICC = failed_licc } ->
        -- Turning on UndecidableInstances makes the check liberal,
        -- so if the liberal check passes, suggest enabling UndecidableInstances.
        if failed_licc
        then noHints
        else [suggestExtension LangExt.UndecidableInstances]

failedCoverageConditionReason :: CoverageProblem -> DiagnosticReason
failedCoverageConditionReason _ = ErrorWithoutFlag

pprIllegalFamilyInstance :: IllegalFamilyInstanceReason -> SDoc
pprIllegalFamilyInstance = \case
  InvalidAssoc reason -> pprInvalidAssoc reason
  NotAFamilyTyCon ty_or_data tc ->
    vcat [ text "Illegal family instance for" <+> quotes (ppr tc)
         , nest 2 $ parens (quotes (ppr tc) <+> text "is not a" <+> what) ]
    where
      what = ppr ty_or_data <+> text "family"
  NotAnOpenFamilyTyCon tc ->
    text "Illegal instance for closed family" <+> quotes (ppr tc)
  FamilyCategoryMismatch tc ->
    text "Wrong category of family instance; declaration was for a" <+> what <> dot
    where
      what = case tyConFlavour tc of
        OpenFamilyFlavour (IAmData {}) _ -> text "data family"
        _                                -> text "type family"
  FamilyArityMismatch _ max_args ->
    text "Number of parameters must match family declaration; expected"
    <+> ppr max_args <> dot
  TyFamNameMismatch fam_tc_name eqn_tc_name ->
    hang (text "Mismatched type name in type family instance.")
       2 (vcat [ text "Expected:" <+> ppr fam_tc_name
               , text "  Actual:" <+> ppr eqn_tc_name ])
  FamInstRHSOutOfScopeTyVars mb_dodgy (NE.toList -> tvs) ->
    hang (text "Out of scope type variable" <> plural tvs
         <+> pprWithCommas (quotes . ppr) tvs
         <+> text "in the RHS of a family instance.")
       2 (text "All such variables must be bound on the LHS.")
    $$ mk_extra
    where
    -- mk_extra: #7536: give a decent error message for
    --         type T a = Int
    --         type instance F (T a) = a
    mk_extra = case mb_dodgy of
      Nothing -> empty
      Just (fam_tc, pats, dodgy_tvs) ->
        ppWhen (any (`elemVarSetByKey` dodgy_tvs) (fmap nameUnique tvs)) $
          hang (text "The real LHS (expanding synonyms) is:")
             2 (pprTypeApp fam_tc (map expandTypeSynonyms pats))
  FamInstLHSUnusedBoundTyVars (NE.toList -> bad_qtvs) ->
    vcat [ not_bound_msg, not_used_msg, dodgy_msg ]
    where

      -- Filter to only keep user-written variables,
      -- unless none were user-written in which case we report all of them
      -- (as we need to report an error).
      filter_user tvs
        = map ifiqtv
        $ case filter ifiqtv_user_written tvs of { [] -> tvs ; qvs -> qvs }

      (not_bound, not_used, dodgy)
        = case foldr acc_tv ([], [], []) bad_qtvs of
            (nb, nu, d) -> (filter_user nb, filter_user nu, filter_user d)

      acc_tv tv (nb, nu, d) = case ifiqtv_reason tv of
        InvalidFamInstQTvNotUsedInRHS   -> (nb, tv : nu, d)
        InvalidFamInstQTvNotBoundInPats -> (tv : nb, nu, d)
        InvalidFamInstQTvDodgy          -> (nb, nu, tv : d)

      -- Error message for type variables not bound in LHS patterns.
      not_bound_msg
        | null not_bound
        = empty
        | otherwise
        = vcat [ text "The type variable" <> plural not_bound <+> pprQuotedList not_bound
            <+> isOrAre not_bound <+> text "bound by a forall,"
              , text "but" <+> doOrDoes not_bound <+> text "not appear in any of the LHS patterns of the family instance." ]

      -- Error message for type variables bound by a forall but not used
      -- in the RHS.
      not_used_msg =
        if null not_used
        then empty
        else text "The type variable" <> plural not_used <+> pprQuotedList not_used
             <+> isOrAre not_used <+> text "bound by a forall," $$
             text "but" <+> itOrThey not_used <+>
             isOrAre not_used <> text "n't used in the family instance."

      -- Error message for dodgy type variables.
      -- See Note [Dodgy binding sites in type family instances] in GHC.Tc.Validity.
      dodgy_msg
        | null dodgy
        = empty
        | otherwise
        = hang (text "Dodgy type variable" <> plural dodgy <+> pprQuotedList dodgy
               <+> text "in the LHS of a family instance:")
             2 (text "the type variable" <> plural dodgy <+> pprQuotedList dodgy
                <+> text "syntactically appear" <> singular dodgy <+> text "in LHS patterns,"
               $$ text "but" <+> itOrThey dodgy <+> doOrDoes dodgy <> text "n't appear in an injective position.")


illegalFamilyInstanceHints :: IllegalFamilyInstanceReason -> [GhcHint]
illegalFamilyInstanceHints = \case
  InvalidAssoc rea -> invalidAssocHints rea
  NotAFamilyTyCon {} -> noHints
  NotAnOpenFamilyTyCon {} -> noHints
  FamilyCategoryMismatch {} -> noHints
  FamilyArityMismatch {} -> noHints
  TyFamNameMismatch {} -> noHints
  FamInstRHSOutOfScopeTyVars {} -> noHints
  FamInstLHSUnusedBoundTyVars {} -> noHints

illegalFamilyInstanceReason :: IllegalFamilyInstanceReason -> DiagnosticReason
illegalFamilyInstanceReason = \case
  InvalidAssoc rea -> invalidAssocReason rea
  NotAFamilyTyCon {} -> ErrorWithoutFlag
  NotAnOpenFamilyTyCon {} -> ErrorWithoutFlag
  FamilyCategoryMismatch {} -> ErrorWithoutFlag
  FamilyArityMismatch {} -> ErrorWithoutFlag
  TyFamNameMismatch {} -> ErrorWithoutFlag
  FamInstRHSOutOfScopeTyVars {} -> ErrorWithoutFlag
  FamInstLHSUnusedBoundTyVars {} -> ErrorWithoutFlag

pprInvalidAssoc :: InvalidAssoc -> SDoc
pprInvalidAssoc = \case
  InvalidAssocInstance rea -> pprInvalidAssocInstance rea
  InvalidAssocDefault  rea -> pprInvalidAssocDefault  rea

pprInvalidAssocInstance :: InvalidAssocInstance -> SDoc
pprInvalidAssocInstance = \case
  AssocInstanceMissing name ->
    text "No explicit" <+> text "associated type"
    <+> text "or default declaration for"
    <+> quotes (ppr name)
  AssocInstanceNotInAClass fam_tc ->
    text "Associated type" <+> quotes (ppr fam_tc) <+>
    text "must be inside a class instance"
  AssocNotInThisClass cls fam_tc ->
    hsep [ text "Class", quotes (ppr cls)
         , text "does not have an associated type", quotes (ppr fam_tc) ]
  AssocNoClassTyVar cls fam_tc ->
    sep [ text "The associated type" <+> quotes (ppr fam_tc <+> hsep (map ppr (tyConTyVars fam_tc)))
        , text "mentions none of the type or kind variables of the class" <+>
                quotes (ppr cls <+> hsep (map ppr (classTyVars cls)))]
  AssocTyVarsDontMatch vis fam_tc exp_tys act_tys ->
    pprWithInvisibleBitsWhen (isInvisibleForAllTyFlag vis) $
    vcat [ text "Type indexes must match class instance head"
         , text "Expected:" <+> pp exp_tys
         , text "  Actual:" <+> pp act_tys ]
    where
      pp tys = pprIfaceTypeApp topPrec (toIfaceTyCon fam_tc) $
               toIfaceTcArgs fam_tc tys

pprInvalidAssocDefault :: InvalidAssocDefault -> SDoc
pprInvalidAssocDefault = \case
  AssocDefaultNotAssoc cls tc ->
    hsep [ text "Class", quotes (ppr cls)
         , text "does not have an associated type", quotes (ppr tc) ]
  AssocMultipleDefaults name ->
      text "More than one default declaration for" <+> quotes (ppr name)
  AssocDefaultBadArgs fam_tc pat_tys bad_arg ->
    let (pat_vis, main_msg) = case bad_arg of
          AssocDefaultNonTyVarArg (pat_ty, pat_vis) ->
            (pat_vis,
             text "Illegal argument" <+> quotes (ppr pat_ty) <+> text "in:")
          AssocDefaultDuplicateTyVars dups ->
            let (pat_tv, pat_vis) = NE.head dups
            in (pat_vis,
                text "Illegal duplicate variable" <+> quotes (ppr pat_tv) <+> text "in:")
    in pprWithInvisibleBitsWhen (isInvisibleForAllTyFlag pat_vis) $
         hang main_msg
            2 (vcat [ppr_eqn, suggestion])
    where
      ppr_eqn :: SDoc
      ppr_eqn =
        quotes (text "type" <+> ppr (mkTyConApp fam_tc pat_tys)
                <+> equals <+> text "...")

      suggestion :: SDoc
      suggestion = text "The arguments to" <+> quotes (ppr fam_tc)
               <+> text "must all be distinct type variables."

invalidAssocHints :: InvalidAssoc -> [GhcHint]
invalidAssocHints = \case
  InvalidAssocInstance rea -> invalidAssocInstanceHints rea
  InvalidAssocDefault  rea -> invalidAssocDefaultHints  rea

invalidAssocInstanceHints :: InvalidAssocInstance -> [GhcHint]
invalidAssocInstanceHints = \case
  AssocInstanceMissing {} -> noHints
  AssocInstanceNotInAClass {} -> noHints
  AssocNotInThisClass {} -> noHints
  AssocNoClassTyVar {} -> noHints
  AssocTyVarsDontMatch {} -> noHints

invalidAssocDefaultHints :: InvalidAssocDefault -> [GhcHint]
invalidAssocDefaultHints = \case
  AssocDefaultNotAssoc {} -> noHints
  AssocMultipleDefaults {} -> noHints
  AssocDefaultBadArgs _ _ bad ->
    assocDefaultBadArgHints bad

assocDefaultBadArgHints :: AssocDefaultBadArgs -> [GhcHint]
assocDefaultBadArgHints = \case
  AssocDefaultNonTyVarArg {} -> noHints
  AssocDefaultDuplicateTyVars {} -> noHints

invalidAssocReason :: InvalidAssoc -> DiagnosticReason
invalidAssocReason = \case
  InvalidAssocInstance rea -> invalidAssocInstanceReason rea
  InvalidAssocDefault  rea -> invalidAssocDefaultReason  rea

invalidAssocInstanceReason :: InvalidAssocInstance -> DiagnosticReason
invalidAssocInstanceReason = \case
  AssocInstanceMissing {} -> WarningWithFlag (Opt_WarnMissingMethods)
  AssocInstanceNotInAClass {} -> ErrorWithoutFlag
  AssocNotInThisClass {} -> ErrorWithoutFlag
  AssocNoClassTyVar {} -> ErrorWithoutFlag
  AssocTyVarsDontMatch {} -> ErrorWithoutFlag

invalidAssocDefaultReason :: InvalidAssocDefault -> DiagnosticReason
invalidAssocDefaultReason = \case
  AssocDefaultNotAssoc {} -> ErrorWithoutFlag
  AssocMultipleDefaults {} -> ErrorWithoutFlag
  AssocDefaultBadArgs _ _ rea ->
    assocDefaultBadArgReason rea

assocDefaultBadArgReason :: AssocDefaultBadArgs -> DiagnosticReason
assocDefaultBadArgReason = \case
  AssocDefaultNonTyVarArg {} -> ErrorWithoutFlag
  AssocDefaultDuplicateTyVars {} -> ErrorWithoutFlag

--------------------------------------------------------------------------------
-- Template Haskell quotes and splices

pprTHError :: THError -> DecoratedSDoc
pprTHError = \case
  THSyntaxError err -> pprTHSyntaxError err
  THNameError   err -> pprTHNameError   err
  THReifyError  err -> pprTHReifyError  err
  TypedTHError  err -> pprTypedTHError  err
  THSpliceFailed rea -> pprSpliceFailReason rea
  AddTopDeclsError err -> pprAddTopDeclsError err

  IllegalStaticFormInSplice e ->
    mkSimpleDecorated $
      sep [ text "static forms cannot be used in splices:"
          , nest 2 $ ppr e
          ]

  FailedToLookupThInstName th_type reason ->
    mkSimpleDecorated $
    case reason of
      NoMatchesFound ->
        text "Couldn't find any instances of"
          <+> text (TH.pprint th_type)
          <+> text "to add documentation to"
      CouldNotDetermineInstance ->
        text "Couldn't work out what instance"
          <+> text (TH.pprint th_type)
          <+> text "is supposed to be"

  AddInvalidCorePlugin plugin ->
    mkSimpleDecorated $
      hang (text "addCorePlugin: invalid plugin module" <+> quotes (text plugin) )
         2 (text "Plugins in the current package can't be specified.")

  AddDocToNonLocalDefn doc_loc ->
    mkSimpleDecorated $
      text "Can't add documentation to" <+> ppr_loc doc_loc <> comma <+>
      text "as it isn't inside the current module."
      where
        ppr_loc (TH.DeclDoc n) = text $ TH.pprint n
        ppr_loc (TH.ArgDoc n _) = text $ TH.pprint n
        ppr_loc (TH.InstDoc t) = text $ TH.pprint t
        ppr_loc TH.ModuleDoc = text "the module header"

  ReportCustomQuasiError _ msg -> mkSimpleDecorated $ text msg

pprTHSyntaxError :: THSyntaxError -> DecoratedSDoc
pprTHSyntaxError = mkSimpleDecorated . \case
  IllegalTHQuotes expr ->
    text "Syntax error on" <+> ppr expr
      -- The error message context will say
      -- "In the Template Haskell quotation", so no need to repeat that here.
  BadImplicitSplice ->
    sep [ text "Parse error: module header, import declaration"
        , text "or top-level declaration expected." ]
    -- The compiler should not mention TemplateHaskell, as the common case
    -- is that this is a simple beginner error, for example:
    --
    -- module M where
    --   f :: Int -> Int; f x = x
    --   xyzzy
    --   g y = f y + 1
    --
    -- It's unlikely that 'xyzzy' above was intended to be a Template Haskell
    -- splice; instead it's probably something mistakenly left in the code.
    -- See #12146 for discussion.

  IllegalTHSplice ->
    text "Unexpected top-level splice."
  MismatchedSpliceType splice_type inner_splice_or_bracket ->
    inner <+> text "may not appear in" <+> outer <> dot
      where
        (inner, outer) = case inner_splice_or_bracket of
          IsSplice -> case splice_type of
            Typed   -> (text "Typed splices"  , text "untyped brackets")
            Untyped -> (text "Untyped splices", text "typed brackets")
          IsBracket ->
            case splice_type of
            Typed   -> (text "Untyped brackets", text "typed splices")
            Untyped -> (text "Typed brackets"  , text "untyped splices")
  NestedTHBrackets ->
    text "Template Haskell brackets cannot be nested" <+>
    text "(without intervening splices)"

pprTHNameError :: THNameError -> DecoratedSDoc
pprTHNameError = \case
  NonExactName name ->
    mkSimpleDecorated $
      hang (text "The binder" <+> quotes (ppr name) <+> text "is not a NameU.")
         2 (text "Probable cause: you used mkName instead of newName to generate a binding.")
  QuotedNameWrongStage quote ->
    mkSimpleDecorated $
      sep [ text "Stage error: the non-top-level quoted name" <+> ppr quote
          , text "must be used at the same stage at which it is bound." ]

pprTHReifyError :: THReifyError -> DecoratedSDoc
pprTHReifyError = \case
  CannotReifyInstance ty
    -> mkSimpleDecorated $
       hang (text "reifyInstances:" <+> quotes (ppr ty))
          2 (text "is not a class constraint or type family application")
  CannotReifyOutOfScopeThing th_name
    -> mkSimpleDecorated $
       quotes (text (TH.pprint th_name)) <+>
               text "is not in scope at a reify"
             -- Ugh! Rather an indirect way to display the name
  CannotReifyThingNotInTypeEnv name
    -> mkSimpleDecorated $
       quotes (ppr name) <+> text "is not in the type environment at a reify"
  NoRolesAssociatedWithThing thing
    -> mkSimpleDecorated $
       text "No roles associated with" <+> (ppr thing)
  CannotRepresentType sort ty
    -> mkSimpleDecorated $
       hsep [text "Can't represent" <+> sort_doc <+>
             text "in Template Haskell:",
               nest 2 (ppr ty)]
     where
       sort_doc = text $
         case sort of
           LinearInvisibleArgument -> "linear invisible argument"
           CoercionsInTypes -> "coercions in types"

pprTypedTHError :: TypedTHError -> DecoratedSDoc
pprTypedTHError = \case
  SplicePolymorphicLocalVar ident
    -> mkSimpleDecorated $
         text "Can't splice the polymorphic local variable" <+> quotes (ppr ident)
  TypedTHWithPolyType ty
    -> mkSimpleDecorated $
      vcat [ text "Illegal polytype:" <+> ppr ty
           , text "The type of a Typed Template Haskell expression must" <+>
             text "not have any quantification." ]

pprSpliceFailReason :: SpliceFailReason -> DecoratedSDoc
pprSpliceFailReason = \case
  SpliceThrewException phase _exn exn_msg expr show_code ->
    mkSimpleDecorated $
      vcat [ text "Exception when trying to" <+> text phaseStr <+> text "compile-time code:"
           , nest 2 (text exn_msg)
           , if show_code then text "Code:" <+> ppr expr else empty]
    where phaseStr =
            case phase of
              SplicePhase_Run -> "run"
              SplicePhase_CompileAndLink -> "compile and link"
  RunSpliceFailure err -> pprRunSpliceFailure Nothing err

pprAddTopDeclsError :: AddTopDeclsError -> DecoratedSDoc
pprAddTopDeclsError = \case
  InvalidTopDecl _decl ->
    mkSimpleDecorated $
      sep [ text "Only function, value, annotation, and foreign import declarations"
          , text "may be added with" <+> quotes (text "addTopDecls") <> dot ]
  AddTopDeclsUnexpectedDeclarationSplice {} ->
    mkSimpleDecorated $
      text "Declaration splices are not permitted" <+>
      text "inside top-level declarations added with" <+>
      quotes (text "addTopDecls") <> dot
  AddTopDeclsRunSpliceFailure err ->
    pprRunSpliceFailure (Just "addTopDecls") err

pprRunSpliceFailure :: Maybe String -> RunSpliceFailReason -> DecoratedSDoc
pprRunSpliceFailure mb_calling_fn (ConversionFail what reason) =
  mkSimpleDecorated . add_calling_fn . addSpliceInfo $
    pprConversionFailReason reason
  where
    add_calling_fn rest =
      case mb_calling_fn of
        Just calling_fn ->
          hang (text "Error in a declaration passed to" <+> quotes (text calling_fn) <> colon)
             2 rest
        Nothing -> rest
    addSpliceInfo = case what of
      ConvDec  d -> addSliceInfo' "declaration" d
      ConvExp  e -> addSliceInfo' "expression" e
      ConvPat  p -> addSliceInfo' "pattern" p
      ConvType t -> addSliceInfo' "type" t
    addSliceInfo' what item reasonErr = reasonErr $$ descr
      where
            -- Show the item in pretty syntax normally,
            -- but with all its constructors if you say -dppr-debug
        descr = hang (text "When splicing a TH" <+> text what <> colon)
                   2 (getPprDebug $ \case
                       True  -> text (show item)
                       False -> text (TH.pprint item))

thErrorReason :: THError -> DiagnosticReason
thErrorReason = \case
  THSyntaxError err -> thSyntaxErrorReason err
  THNameError   err -> thNameErrorReason   err
  THReifyError  err -> thReifyErrorReason  err
  TypedTHError  err -> typedTHErrorReason  err
  THSpliceFailed rea -> spliceFailedReason rea
  AddTopDeclsError err -> addTopDeclsErrorReason err

  IllegalStaticFormInSplice {} -> ErrorWithoutFlag
  FailedToLookupThInstName {}  -> ErrorWithoutFlag
  AddInvalidCorePlugin {}      -> ErrorWithoutFlag
  AddDocToNonLocalDefn {}      -> ErrorWithoutFlag
  ReportCustomQuasiError is_error _ ->
    if is_error
    then ErrorWithoutFlag
    else WarningWithoutFlag

thSyntaxErrorReason :: THSyntaxError -> DiagnosticReason
thSyntaxErrorReason = \case
  IllegalTHQuotes{}      -> ErrorWithoutFlag
  BadImplicitSplice      -> ErrorWithoutFlag
  IllegalTHSplice{}      -> ErrorWithoutFlag
  NestedTHBrackets{}     -> ErrorWithoutFlag
  MismatchedSpliceType{} -> ErrorWithoutFlag

thNameErrorReason :: THNameError -> DiagnosticReason
thNameErrorReason = \case
  NonExactName {}         -> ErrorWithoutFlag
  QuotedNameWrongStage {} -> ErrorWithoutFlag

thReifyErrorReason :: THReifyError -> DiagnosticReason
thReifyErrorReason = \case
  CannotReifyInstance {}          -> ErrorWithoutFlag
  CannotReifyOutOfScopeThing {}   -> ErrorWithoutFlag
  CannotReifyThingNotInTypeEnv {} -> ErrorWithoutFlag
  NoRolesAssociatedWithThing {}   -> ErrorWithoutFlag
  CannotRepresentType {}          -> ErrorWithoutFlag

typedTHErrorReason :: TypedTHError -> DiagnosticReason
typedTHErrorReason = \case
  SplicePolymorphicLocalVar {} -> ErrorWithoutFlag
  TypedTHWithPolyType {}       -> ErrorWithoutFlag

spliceFailedReason :: SpliceFailReason -> DiagnosticReason
spliceFailedReason = \case
  SpliceThrewException {} -> ErrorWithoutFlag
  RunSpliceFailure {}     -> ErrorWithoutFlag

addTopDeclsErrorReason :: AddTopDeclsError -> DiagnosticReason
addTopDeclsErrorReason = \case
  InvalidTopDecl {}
    -> ErrorWithoutFlag
  AddTopDeclsUnexpectedDeclarationSplice {}
    -> ErrorWithoutFlag
  AddTopDeclsRunSpliceFailure {}
    -> ErrorWithoutFlag

thErrorHints :: THError -> [GhcHint]
thErrorHints = \case
  THSyntaxError err -> thSyntaxErrorHints err
  THNameError   err -> thNameErrorHints   err
  THReifyError  err -> thReifyErrorHints  err
  TypedTHError  err -> typedTHErrorHints  err
  THSpliceFailed rea -> spliceFailedHints rea
  AddTopDeclsError err -> addTopDeclsErrorHints err

  IllegalStaticFormInSplice {} -> noHints
  FailedToLookupThInstName {}  -> noHints
  AddInvalidCorePlugin {}      -> noHints
  AddDocToNonLocalDefn {}      -> noHints
  ReportCustomQuasiError {}    -> noHints

thSyntaxErrorHints :: THSyntaxError -> [GhcHint]
thSyntaxErrorHints = \case
  IllegalTHQuotes{}
    -> [suggestExtension LangExt.TemplateHaskellQuotes]
  BadImplicitSplice {}
    -> noHints -- NB: don't suggest TemplateHaskell
               -- see comments on BadImplicitSplice in pprTHSyntaxError
  IllegalTHSplice{}
    -> [suggestExtension LangExt.TemplateHaskell]
  NestedTHBrackets{}
    -> noHints
  MismatchedSpliceType{}
    -> noHints

thNameErrorHints :: THNameError -> [GhcHint]
thNameErrorHints = \case
  NonExactName {}         -> noHints
  QuotedNameWrongStage {} -> noHints

thReifyErrorHints :: THReifyError -> [GhcHint]
thReifyErrorHints = \case
  CannotReifyInstance {}          -> noHints
  CannotReifyOutOfScopeThing {}   -> noHints
  CannotReifyThingNotInTypeEnv {} -> noHints
  NoRolesAssociatedWithThing {}   -> noHints
  CannotRepresentType {}          -> noHints

typedTHErrorHints :: TypedTHError -> [GhcHint]
typedTHErrorHints = \case
  SplicePolymorphicLocalVar {} -> noHints
  TypedTHWithPolyType {}       -> noHints

spliceFailedHints :: SpliceFailReason -> [GhcHint]
spliceFailedHints = \case
  SpliceThrewException {} -> noHints
  RunSpliceFailure {}     -> noHints

addTopDeclsErrorHints :: AddTopDeclsError -> [GhcHint]
addTopDeclsErrorHints = \case
  InvalidTopDecl {}
    -> noHints
  AddTopDeclsUnexpectedDeclarationSplice {}
    -> noHints
  AddTopDeclsRunSpliceFailure {}
    -> noHints

--------------------------------------------------------------------------------

pprPatersonCondFailure ::
  PatersonCondFailure -> PatersonCondFailureContext -> Type -> Type -> SDoc
pprPatersonCondFailure (PCF_TyVar tvs) InInstanceDecl lhs rhs =
  hang (occMsg tvs)
    2 (sep [ text "in the constraint" <+> quotes (ppr lhs)
         , text "than in the instance head" <+> quotes (ppr rhs) ])
  where
    occMsg tvs = text "Variable" <> plural tvs <+> quotes (pprWithCommas ppr tvs)
                 <+> pp_occurs <+> text "more often"
    pp_occurs | isSingleton tvs = text "occurs"
              | otherwise       = text "occur"
pprPatersonCondFailure (PCF_TyVar tvs) InTyFamEquation lhs rhs =
  hang (occMsg tvs)
    2 (sep [ text "in the type-family application" <+> quotes (ppr rhs)
         , text "than in the LHS of the family instance" <+> quotes (ppr lhs) ])
  where
    occMsg tvs = text "Variable" <> plural tvs <+> quotes (pprWithCommas ppr tvs)
                 <+> pp_occurs <+> text "more often"
    pp_occurs | isSingleton tvs = text "occurs"
              | otherwise       = text "occur"
pprPatersonCondFailure PCF_Size InInstanceDecl lhs rhs =
  hang (text "The constraint" <+> quotes (ppr lhs))
    2 (sep [ text "is no smaller than", pp_rhs ])
  where pp_rhs = text "the instance head" <+> quotes (ppr rhs)
pprPatersonCondFailure PCF_Size InTyFamEquation lhs rhs =
  hang (text "The type-family application" <+> quotes (ppr rhs))
    2 (sep [ text "is no smaller than", pp_lhs ])
  where pp_lhs = text "the LHS of the family instance" <+> quotes (ppr lhs)
pprPatersonCondFailure  (PCF_TyFam tc) InInstanceDecl lhs _rhs =
  hang (text "Illegal use of type family" <+> quotes (ppr tc))
    2 (text "in the constraint" <+> quotes (ppr lhs))
pprPatersonCondFailure  (PCF_TyFam tc) InTyFamEquation _lhs rhs =
  hang (text "Illegal nested use of type family" <+> quotes (ppr tc))
    2 (text "in the arguments of the type-family application" <+> quotes (ppr rhs))

--------------------------------------------------------------------------------

defaultTypesAndImport :: ClassDefaults -> SDoc
defaultTypesAndImport ClassDefaults{cd_types, cd_module = Just cdm} =
  hang (parens $ pprWithCommas ppr cd_types)
     2 (text "imported from" <+> ppr cdm)
defaultTypesAndImport ClassDefaults{cd_types} = parens (pprWithCommas ppr cd_types)

--------------------------------------------------------------------------------

pprZonkerMessage :: ZonkerMessage -> SDoc
pprZonkerMessage = \case
  ZonkerCannotDefaultConcrete frr ->
    ppr (frr_context frr) $$
    text "cannot be assigned a fixed runtime representation," <+>
    text "not even by defaulting."

zonkerMessageHints :: ZonkerMessage -> [GhcHint]
zonkerMessageHints = \case
  ZonkerCannotDefaultConcrete {} -> [SuggestAddTypeSignatures UnnamedBinding]

zonkerMessageReason :: ZonkerMessage -> DiagnosticReason
zonkerMessageReason = \case
  ZonkerCannotDefaultConcrete {} -> ErrorWithoutFlag

--------------------------------------------------------------------------------

pprTypeSyntaxName :: TypeSyntax -> SDoc
pprTypeSyntaxName TypeKeywordSyntax     = "keyword" <+> quotes "type"
pprTypeSyntaxName ForallTelescopeSyntax = "forall telescope"
pprTypeSyntaxName ContextArrowSyntax    = "context arrow (=>)"
pprTypeSyntaxName FunctionArrowSyntax   = "function type arrow (->)"

--------------------------------------------------------------------------------
-- ErrCtxt

pprTyConInstFlavour :: TyConInstFlavour -> SDoc
pprTyConInstFlavour
  ( TyConInstFlavour
      { tyConInstFlavour   = flav
      , tyConInstIsDefault = is_dflt
      }
  ) = (if is_dflt then text "default" else empty) <+> ppr flav <+> text "instance"

pprErrCtxtMsg :: ErrCtxtMsg -> SDoc
pprErrCtxtMsg = \case
  ExprCtxt expr ->
    hang (text "In the expression:")
       2 (ppr (stripParensHsExpr expr))
  ThetaCtxt ctxt theta ->
    vcat [ text "In the context:" <+> pprTheta theta
         , text "While checking" <+> pprUserTypeCtxt ctxt ]
  QuantifiedCtCtxt pty ->
    text "In the quantified constraint" <+> quotes (ppr pty)
  InferredTypeCtxt poly_name poly_ty ->
    vcat [ text "When checking the inferred type"
         , nest 2 $ ppr poly_name <+> dcolon <+> ppr poly_ty ]
  SigCtxt sig ->
    text "In" <+> ppr sig
  UserSigCtxt ctxt hs_ty
    | Just n <- isSigMaybe ctxt
    -> hang (text "In the type signature:")
          2 (pprPrefixOcc n <+> dcolon <+> ppr hs_ty)
    | otherwise
    -> hang (text "In" <+> pprUserTypeCtxt ctxt <> colon)
          2 (ppr hs_ty)
  RecordUpdCtxt ne_relevant_cons@(relevant_con :| _) upd_fld_names ex_tvs ->
    make_lines_msg $
    (text "In a record update at field" <> plural upd_fld_names <+> pprQuotedList upd_fld_names :)
    $ case relevant_con of
         RealDataCon con ->
            [ text "with type constructor" <+> quotes (ppr (dataConTyCon con))
            , text "data constructor" <+> plural relevant_cons <+> cons ]
         PatSynCon {} ->
            [ text "with pattern synonym" <+> plural relevant_cons <+> cons ]
    ++ if null ex_tvs
       then []
       else [ text "existential variable" <> plural ex_tvs <+> pprQuotedList ex_tvs ]

    where
     cons = pprQuotedList relevant_cons
     relevant_cons = NE.toList ne_relevant_cons
     -- Pretty-print a collection of lines, adding commas at the end of each line,
     -- and adding "and" to the start of the last line.
     make_lines_msg :: [SDoc] -> SDoc
     make_lines_msg []      = empty
     make_lines_msg [last]  = ppr last <> dot
     make_lines_msg [l1,l2] = l1 $$ text "and" <+> l2 <> dot
     make_lines_msg (l:ls)  = l <> comma $$ make_lines_msg ls
  PatSigErrCtxt sig_ty res_ty ->
    vcat [ hang (text "When checking that the pattern signature:")
              4 (ppr sig_ty)
         , nest 2 (hang (text "fits the type of its context:")
                      2 (ppr res_ty)) ]
  PatCtxt pat ->
    hang (text "In the pattern:") 2 (ppr pat)
  PatSynDeclCtxt name ->
    text "In the declaration for pattern synonym" <+> quotes (ppr name)
  ClassOpCtxt meth meth_ty ->
    sep [ text "When checking the class method:"
        , nest 2 (pprPrefixOcc meth <+> dcolon <+> ppr meth_ty)]
  MethSigCtxt sel_name sig_ty meth_ty ->
    hang (text "When checking that instance signature for" <+> quotes (ppr sel_name))
       2 (vcat [ text "is more general than its signature in the class"
               , text "Instance sig:" <+> ppr sig_ty
               , text "   Class sig:" <+> ppr meth_ty ])
  PatMonoBindsCtxt pat grhss ->
    hang (text "In a pattern binding:")
       2 (pprPatBind pat grhss)
  ForeignDeclCtxt fo ->
    hang (text "When checking declaration:")
       2 (ppr fo)
  RuleCtxt rule_name ->
    text "When checking the rewrite rule" <+> doubleQuotes (ftext rule_name)
  FieldCtxt field_name ->
    text "In the" <+> quotes (ppr field_name) <+> text "field of a record"
  TypeCtxt ty ->
    text "In the type" <+> quotes (ppr ty)
  KindCtxt ki ->
    text "In the kind" <+> quotes (ppr ki)
  SubTypeCtxt ty_expected ty_actual ->
    vcat [ hang (text "When checking that:")
              4 (ppr ty_actual)
         , nest 2 (hang (text "is more polymorphic than:")
                      2 (ppr ty_expected)) ]
  AmbiguityCheckCtxt ctxt allow_ambiguous ->
     vcat [ text "In the ambiguity check for" <+> what
          , ppUnless allow_ambiguous ambig_msg ]
    where
      ambig_msg = text "To defer the ambiguity check to use sites, enable AllowAmbiguousTypes"
      what | Just n <- isSigMaybe ctxt = quotes (ppr n)
           | otherwise                 = pprUserTypeCtxt ctxt

  FunAppCtxt fun_arg arg_no ->
    hang (hsep [ text "In the", speakNth arg_no, text "argument of"
               , quotes fun <> text ", namely"])
       2 (quotes arg)
      where
        fun, arg :: SDoc
        (fun, arg) = case fun_arg of
          FunAppCtxtExpr fn a -> (ppr fn, ppr a)
          FunAppCtxtTy   fn a -> (ppr fn, ppr a)
  FunTysCtxt herald fun_ty n_vis_args_in_call n_fun_args
    | n_vis_args_in_call <= n_fun_args  -- Enough args, in the end
    -> text "In the result of a function call"
    | otherwise
    -> hang (full_herald <> comma)
         2 (sep [ text "but its type" <+> quotes (pprSigmaType fun_ty)
                , if n_fun_args == 0 then text "has none"
                  else text "has only" <+> speakN n_fun_args])
    where
      full_herald = pprExpectedFunTyHerald herald
                <+> speakNOf n_vis_args_in_call (text "visible argument")
                 -- What are "visible" arguments? See Note [Visibility and arity] in GHC.Types.Basic
  FunResCtxt fun n_val_args res_fun res_env n_fun n_env
    | -- Check for too few args
      --  fun_tau = a -> b, res_tau = Int
      n_fun > n_env
    , not_fun res_env
    -> text "Probable cause:" <+> quotes (ppr fun)
      <+> text "is applied to too few arguments"

    | -- Check for too many args
      -- fun_tau = a -> Int,   res_tau = a -> b -> c -> d
      -- The final guard suppresses the message when there
      -- aren't enough args to drop; eg. the call is (f e1)
      n_fun < n_env
    , not_fun res_fun
    , n_fun + n_val_args >= n_env
       -- Never suggest that a naked variable is
                        -- applied to too many args!
    -> text "Possible cause:" <+> quotes (ppr fun)
      <+> text "is applied to too many arguments"

    | otherwise
    -> empty
    where
      not_fun ty   -- ty is definitely not an arrow type,
                   -- and cannot conceivably become one
        = case tcSplitTyConApp_maybe ty of
            Just (tc, _) -> isAlgTyCon tc
            Nothing      -> False

  TyConDeclCtxt name flav ->
    hsep [ text "In the", ppr flav
         , text "declaration for", quotes (ppr name) ]
  TyConInstCtxt name flav ->
    hsep [ text "In the" <+> pprTyConInstFlavour flav <+> text "declaration for"
         , quotes (ppr name) ]
  DataConDefCtxt cons ->
    text "In the definition of data constructor" <> plural (NE.toList cons)
      <+> ppr_cons (NE.toList cons)
    where
      ppr_cons :: [LocatedN Name] -> SDoc
      ppr_cons [con] = quotes (ppr con)
      ppr_cons cons  = interpp'SP cons
  DataConResTyCtxt cons ->
    text "In the result type of data constructor" <> plural (NE.toList cons)
     <+> ppr_cons (NE.toList cons)
    where
      ppr_cons :: [LocatedN Name] -> SDoc
      ppr_cons [con] = quotes (ppr con)
      ppr_cons cons  = interpp'SP cons
  ClosedFamEqnCtxt tc ->
    text "In the equations for closed type family" <+>
           quotes (ppr tc)
  TySynErrCtxt tc ->
    text "In the expansion of type synonym" <+> quotes (ppr tc)
  RoleAnnotErrCtxt name ->
    nest 2 $ text "while checking a role annotation for" <+> quotes (ppr name)
  CmdCtxt cmd ->
    text "In the command:" <+> ppr cmd
  InstDeclErrCtxt either_ty_ty ->
    hang (text "In the instance declaration for")
       2 (quotes $ ppr_ty)
   where
    ppr_ty = case either_ty_ty of
      Left  ty -> ppr ty
      Right ty -> ppr ty
  StaticFormCtxt expr ->
    hang (text "In the body of a static form:")
       2 (ppr expr)
  DefaultDeclErrCtxt { ddec_in_type_list = in_type_list } ->
    if in_type_list
    then
      text "When checking the types in a default declaration"
    else
      text "When checking the class at the head of a named default declaration"
  MainCtxt main_name ->
    text "When checking the type of the"
       <+> ppMainFn (nameOccName main_name)

  VDQWarningCtxt tycon ->
    vcat
      [ text "NB: Type" <+> quotes (ppr tycon) <+>
        text "was inferred to use visible dependent quantification."
      , text "Most types with visible dependent quantification are"
      , text "polymorphically recursive and need a standalone kind"
      , text "signature. Perhaps supply one, with StandaloneKindSignatures."
      ]
  TermLevelUseCtxt name ctxt ->
    pprTermLevelUseCtxt name ctxt

  StmtErrCtxt ctxt stmt
    -- For [ e | .. ], do not mutter about "stmts"
    | LastStmt _ e _ _ <- stmt
    , isComprehensionContext ctxt
    -> hang (text "In the expression:") 2 (ppr e)
    | otherwise
    -> hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
    where
      -- For Group and Transform Stmts, don't print the nested stmts!
      ppr_stmt (TransStmt { trS_by = by, trS_using = using
                          , trS_form = form }) = pprTransStmt by using form
      ppr_stmt stmt = pprStmt stmt

  DerivInstCtxt pred ->
    text "When deriving the instance for" <+> parens (ppr pred)
  StandaloneDerivCtxt ty ->
    hang (text "In the stand-alone deriving instance for")
       2 (quotes (ppr ty))
  DerivBindCtxt sel_id clas tys ->
    vcat [ text "When typechecking the code for" <+> quotes (ppr sel_id)
         , nest 2 (text "in a derived instance for"
                   <+> quotes (pprClassPred clas tys) <> colon)
         , nest 2 $ text "To see the code I am typechecking, use -ddump-deriv" ]


  ExportCtxt ie ->
    text "In the export:" <+> ppr ie
  PatSynExportCtxt ps ->
    text "In the pattern synonym:" <+> ppr ps
  PatSynRecSelExportCtxt _ps sel ->
    text "In the pattern synonym record selector:" <+> ppr sel

  SyntaxNameCtxt name orig ty loc ->
    vcat [ text "when checking that" <+> quotes (ppr name)
                    <+> text "(needed by a syntactic construct)"
         , nest 2 (text "has the required type:"
                   <+> ppr ty)
         , nest 2 (sep [ppr orig, text "at" <+> ppr loc])]

  AnnCtxt ann ->
    hang (text "In the annotation:") 2 (ppr ann)
  SpecPragmaCtxt prag ->
    hang (text "In the pragma:") 2 (ppr prag)
  MatchCtxt ctxt ->
    text "In" <+> pprMatchContext ctxt
  MatchInCtxt match ->
    hang (text "In" <+> pprMatchContext (m_ctxt match) <> colon)
       4 (pprMatch match)
  UntypedTHBracketCtxt br ->
    hang (text "In the Template Haskell quotation" <> colon)
       2 (ppr br)
  TypedTHBracketCtxt br_body ->
    hang (text "In the Template Haskell typed quotation" <> colon)
       2 (thTyBrackets . ppr $ br_body)
  UntypedSpliceCtxt splice ->
    hang (text "In the" <+> what) 2 (pprUntypedSplice True Nothing splice)
      where
        what = case splice of
                 HsUntypedSpliceExpr {} -> text "untyped splice:"
                 HsQuasiQuote        {} -> text "quasi-quotation:"
  TypedSpliceCtxt mb_nm expr ->
    hang (text "In the typed Template Haskell splice:")
       2 (pprTypedSplice mb_nm expr)
  TypedSpliceResultCtxt expr ->
    sep [ text "In the result of the splice:"
        , nest 2 (pprTypedSplice Nothing expr)
        , text "To see what the splice expanded to, use -ddump-splices"]

  ReifyInstancesCtxt th_nm th_tys ->
    text "In the argument of" <+> quotes (text "reifyInstances") <> colon
      <+> ppr_th th_nm <+> sep (map ppr_th th_tys)
    where
      ppr_th :: TH.Ppr a => a -> SDoc
      ppr_th x = text (TH.pprint x)

  MergeSignaturesCtxt unit_state mod_name reqs ->
    pprWithUnitState unit_state $
    if null reqs
    then  text "While checking the local signature" <+> ppr mod_name <+>
          text "for consistency"
    else   hang (text "While merging the signatures from" <> colon)
              2 (vcat [ bullet <+> ppr req | req <- reqs ] $$
                 bullet <+> text "...and the local signature for" <+> ppr mod_name)
  CheckImplementsCtxt unit_state impl_mod (Module req_uid req_mod_name) ->
    pprWithUnitState unit_state $
      text "While checking that" <+> quotes (ppr impl_mod) <+>
      text "implements signature" <+> quotes (ppr req_mod_name) <+>
      text "in" <+> quotes (ppr req_uid) <> dot

--------------------------------------------------------------------------------
