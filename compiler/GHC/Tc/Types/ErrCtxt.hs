{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Tc.Types.ErrCtxt
  ( ErrCtxt, ErrCtxtMsg(..)
  , UserSigType(..), FunAppCtxtFunArg(..)
  , TyConInstFlavour(..)
  )
  where

import GHC.Prelude
import GHC.Hs.Expr
import GHC.Hs.Extension

import GHC.Parser.Annotation ( LocatedN, SrcSpanAnnA )

import GHC.Tc.Errors.Types.PromotionErr ( TermLevelUseCtxt )
import GHC.Tc.Types.Origin   ( CtOrigin, UserTypeCtxt, ExpectedFunTyOrigin )
import GHC.Tc.Utils.TcType   ( TcType, TcTyCon )
import GHC.Tc.Zonk.Monad     ( ZonkM )

import GHC.Types.Basic       ( TyConFlavour )
import GHC.Types.Name        ( Name )
import GHC.Types.SrcLoc      ( SrcSpan )
import GHC.Types.Var         ( Id, TyCoVar )
import GHC.Types.Var.Env     ( TidyEnv )

import GHC.Unit.Types ( Module, InstantiatedModule )

import GHC.Core.Class    ( Class )
import GHC.Core.ConLike  ( ConLike )
import GHC.Core.PatSyn   ( PatSyn )
import GHC.Core.TyCon    ( TyCon )
import GHC.Core.TyCo.Rep ( Type, ThetaType, PredType )

import GHC.Unit.State ( UnitState )

import GHC.Data.FastString  ( FastString )
import GHC.Utils.Outputable ( Outputable(..) )

import Language.Haskell.Syntax.Basic ( FieldLabelString(..) )
import Language.Haskell.Syntax
import GHC.Boot.TH.Syntax qualified as TH

import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------

-- | Additional context to include in an error message, e.g.
-- "In the type signature ...", "In the ambiguity check for ...", etc.
type ErrCtxt = (Bool, TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display

--------------------------------------------------------------------------------
-- Error message contexts

data UserSigType p
  = UserLHsSigType !(LHsSigType p)
  | UserLHsType !(LHsType p)

instance OutputableBndrId p => Outputable (UserSigType (GhcPass p)) where
  ppr (UserLHsSigType ty) = ppr ty
  ppr (UserLHsType ty) = ppr ty

data FunAppCtxtFunArg
  = FunAppCtxtExpr !(HsExpr GhcRn) !(HsExpr GhcRn)
  | FunAppCtxtTy   !(LHsType GhcRn) !(LHsType GhcRn)

-- | Like 'TyConFlavour' but for instance declarations, with
-- the additional information of whether this we are dealing with
-- a default declaration.
data TyConInstFlavour
  = TyConInstFlavour
  { tyConInstFlavour :: !(TyConFlavour TyCon)
  , tyConInstIsDefault :: !Bool
  }

-- | The "context" of an error message, e.g. "In the expression <...>",
-- "In the pattern <...>", or "In the equations for closed type family <...>".
data ErrCtxtMsg
  -- | In an expression.
  = ExprCtxt !(HsExpr GhcRn)
  -- | In a user-written context.
  | ThetaCtxt !UserTypeCtxt !ThetaType
  -- | In a quantified constraint.
  | QuantifiedCtCtxt !PredType
  -- | When checking an inferred type.
  | InferredTypeCtxt !Name !TcType
  -- | In an inline pragma, or a fixity signature,
  -- or a type signature, or... (see 'Sig').
  | SigCtxt !(Sig GhcRn)
  -- | In a user-written type signature.
  | UserSigCtxt !UserTypeCtxt !(UserSigType GhcRn)
  -- | In a record update.
  | RecordUpdCtxt !(NE.NonEmpty ConLike) ![Name] ![TyCoVar]
  -- | In a class method.
  | ClassOpCtxt !Id !Type
  -- | In the instance type signature of a class method.
  | MethSigCtxt !Name !TcType !TcType
  -- | In a pattern type signature.
  | PatSigErrCtxt !TcType !TcType
  -- | In a pattern.
  | PatCtxt !(Pat GhcRn)
  -- | In a pattern synonym declaration.
  | PatSynDeclCtxt !Name
  -- | In a pattern matching context, e.g. a equation for a function binding,
  -- or a case alternative, ...
  | MatchCtxt !HsMatchContextRn
  -- | In a match in a pattern matching context,
  -- either for an expression or for an arrow command.
  | forall body. (Outputable body)
  => MatchInCtxt !(Match GhcRn body)
  -- | In a function application.
  | FunAppCtxt !FunAppCtxtFunArg !Int
  -- | In a function call.
  | FunTysCtxt !ExpectedFunTyOrigin !Type !Int !Int
  -- | In the result of a function call.
  | FunResCtxt !(HsExpr GhcTc) !Int !Type !Type !Int !Int
  -- | In the declaration of a type constructor.
  | TyConDeclCtxt !Name !(TyConFlavour TyCon)
  -- | In a type or data family instance (or default instance).
  | TyConInstCtxt !Name !TyConInstFlavour
  -- | In the declaration of a data constructor.
  | DataConDefCtxt !(NE.NonEmpty (LocatedN Name))
  -- | In the result type of a data constructor.
  | DataConResTyCtxt !(NE.NonEmpty (LocatedN Name))
  -- | In the equations for a closed type family.
  | ClosedFamEqnCtxt !TyCon
  -- | In the expansion of a type synonym.
  | TySynErrCtxt !TyCon
  -- | In a role annotation.
  | RoleAnnotErrCtxt !Name
  -- | In an arrow command.
  | CmdCtxt !(HsCmd GhcRn)
  -- | In an instance declaration.
  | InstDeclErrCtxt !(Either (LHsType GhcRn) PredType)
  -- | In a default declaration.
  | DefaultDeclErrCtxt { ddec_in_type_list :: !Bool }
  -- | In the body of a static form.
  | StaticFormCtxt !(LHsExpr GhcRn)
  -- | In a pattern binding.
  | forall p. OutputableBndrId p
  => PatMonoBindsCtxt !(LPat (GhcPass p)) !(GRHSs GhcRn (LHsExpr GhcRn))
  -- | In a foreign import/export declaration.
  | ForeignDeclCtxt !(ForeignDecl GhcRn)
  -- | In a record field.
  | FieldCtxt !FieldLabelString
  -- | In a type.
  | TypeCtxt !(LHsType GhcRn)
  -- | In a kind.
  | KindCtxt !(LHsKind GhcRn)
  -- | In an ambiguity check.
  | AmbiguityCheckCtxt !UserTypeCtxt !Bool

  -- | In a term-level use of a 'Name'.
  | TermLevelUseCtxt !Name !TermLevelUseCtxt

  -- | When checking the type of the @main@ function.
  | MainCtxt !Name
  -- | Warning emitted when inferring use of visible dependent quantification.
  | VDQWarningCtxt !TcTyCon

  -- | In a statement.
  | forall body.
    ( Anno (StmtLR GhcRn GhcRn body) ~ SrcSpanAnnA
    , Outputable body
    ) => StmtErrCtxt !HsStmtContextRn !(StmtLR GhcRn GhcRn body)

  -- | In an rebindable syntax expression.
  | SyntaxNameCtxt !(HsExpr GhcRn) !CtOrigin !TcType !SrcSpan
  -- | In a RULE.
  | RuleCtxt !FastString
  -- | In a subtype check.
  | SubTypeCtxt !TcType !TcType

  -- | In an export.
  | forall p. OutputableBndrId p
  => ExportCtxt (IE (GhcPass p))
  -- | In an export of a pattern synonym.
  | PatSynExportCtxt !PatSyn
  -- | In an export of a pattern synonym record field.
  | PatSynRecSelExportCtxt !PatSyn !Name

  -- | In an annotation.
  | forall p. OutputableBndrId p
  => AnnCtxt (AnnDecl (GhcPass p))

  -- | In a specialise pragma.
  | SpecPragmaCtxt !(Sig GhcRn)

  -- | In a deriving clause.
  | DerivInstCtxt !PredType
  -- | In a standalone deriving clause.
  | StandaloneDerivCtxt !(LHsSigWcType GhcRn)
  -- | When typechecking the body of a derived instance.
  | DerivBindCtxt !Id !Class ![Type]

  -- | In an untyped Template Haskell quote.
  | UntypedTHBracketCtxt !(HsQuote GhcPs)
  -- | In a typed Template Haskell quote.
  | forall p. OutputableBndrId p
  => TypedTHBracketCtxt !(LHsExpr (GhcPass p))
  -- | In an untyped Template Haskell splice or quasi-quote.
  | UntypedSpliceCtxt !(HsUntypedSplice GhcPs)
  -- | In a typed Template Haskell splice.
  | forall p. OutputableBndrId p
  => TypedSpliceCtxt !(Maybe SplicePointName) !(HsTypedSplice (GhcPass p))
  -- | In the result of a typed Template Haskell splice.
  | TypedSpliceResultCtxt !(LHsExpr GhcTc)
  -- | In an argument to the Template Haskell @reifyInstances@ function.
  | ReifyInstancesCtxt !TH.Name ![TH.Type]

  -- | While merging Backpack signatures.
  | MergeSignaturesCtxt !UnitState !ModuleName ![InstantiatedModule]
  -- | While checking that a module implements a Backpack signature.
  | CheckImplementsCtxt !UnitState !Module !InstantiatedModule
