{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

-- | Describes the provenance of types as they flow through the type-checker.
-- The datatypes here are mainly used for error message generation.
module GHC.Tc.Types.Origin (
  -- * UserTypeCtxt
  UserTypeCtxt(..), pprUserTypeCtxt, isSigMaybe,
  ReportRedundantConstraints(..), reportRedundantConstraints,
  redundantConstraintsSpan,

  -- * SkolemInfo
  SkolemInfo(..), SkolemInfoAnon(..), mkSkolemInfo, getSkolemInfo, pprSigSkolInfo, pprSkolInfo,
  unkSkol, unkSkolAnon,

  -- * CtOrigin
  CtOrigin(..), exprCtOrigin, lexprCtOrigin, matchesCtOrigin, grhssCtOrigin,
  isVisibleOrigin, toInvisibleOrigin,
  pprCtOrigin, isGivenOrigin, isWantedWantedFunDepOrigin,
  isWantedSuperclassOrigin,

  TypedThing(..), TyVarBndrs(..),

  -- * CtOrigin and CallStack
  isPushCallStackOrigin, callStackOriginFS,
  -- * FixedRuntimeRep origin
  FixedRuntimeRepOrigin(..), FixedRuntimeRepContext(..),
  pprFixedRuntimeRepContext,
  StmtOrigin(..),

  -- * Arrow command origin
  FRRArrowContext(..), pprFRRArrowContext,
  ExpectedFunTyOrigin(..), pprExpectedFunTyOrigin, pprExpectedFunTyHerald,

  ) where

import GHC.Prelude

import GHC.Tc.Utils.TcType

import GHC.Hs

import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.PatSyn
import GHC.Core.Multiplicity ( scaledThing )

import GHC.Unit.Module
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Basic
import GHC.Types.SrcLoc

import GHC.Data.FastString

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Stack
import GHC.Utils.Monad
import GHC.Types.Unique
import GHC.Types.Unique.Supply

{- *********************************************************************
*                                                                      *
          UserTypeCtxt
*                                                                      *
********************************************************************* -}

-------------------------------------
-- | UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need an expression to have that type
data UserTypeCtxt
  = FunSigCtxt      -- Function type signature, when checking the type
                    -- Also used for types in SPECIALISE pragmas
       Name              -- Name of the function
       ReportRedundantConstraints
         -- This is usually 'WantRCC', but 'NoRCC' for
         --   * Record selectors (not important here)
         --   * Class and instance methods.  Here the code may legitimately
         --     be more polymorphic than the signature generated from the
         --     class declaration

  | InfSigCtxt Name     -- Inferred type for function
  | ExprSigCtxt         -- Expression type signature
      ReportRedundantConstraints
  | KindSigCtxt         -- Kind signature
  | StandaloneKindSigCtxt  -- Standalone kind signature
       Name                -- Name of the type/class
  | TypeAppCtxt         -- Visible type application
  | ConArgCtxt Name     -- Data constructor argument
  | TySynCtxt Name      -- RHS of a type synonym decl
  | PatSynCtxt Name     -- Type sig for a pattern synonym
  | PatSigCtxt          -- Type sig in pattern
                        --   eg  f (x::t) = ...
                        --   or  (x::t, y) = e
  | RuleSigCtxt FastString Name    -- LHS of a RULE forall
                        --    RULE "foo" forall (x :: a -> a). f (Just x) = ...
  | ForSigCtxt Name     -- Foreign import or export signature
  | DefaultDeclCtxt     -- Types in a default declaration
  | InstDeclCtxt Bool   -- An instance declaration
                        --    True:  stand-alone deriving
                        --    False: vanilla instance declaration
  | SpecInstCtxt        -- SPECIALISE instance pragma
  | GenSigCtxt          -- Higher-rank or impredicative situations
                        -- e.g. (f e) where f has a higher-rank type
                        -- We might want to elaborate this
  | GhciCtxt Bool       -- GHCi command :kind <type>
                        -- The Bool indicates if we are checking the outermost
                        -- type application.
                        -- See Note [Unsaturated type synonyms in GHCi] in
                        -- GHC.Tc.Validity.

  | ClassSCCtxt Name    -- Superclasses of a class
  | SigmaCtxt           -- Theta part of a normal for-all type
                        --      f :: <S> => a -> a
  | DataTyCtxt Name     -- The "stupid theta" part of a data decl
                        --      data <S> => T a = MkT a
  | DerivClauseCtxt     -- A 'deriving' clause
  | TyVarBndrKindCtxt Name  -- The kind of a type variable being bound
  | DataKindCtxt Name   -- The kind of a data/newtype (instance)
  | TySynKindCtxt Name  -- The kind of the RHS of a type synonym
  | TyFamResKindCtxt Name   -- The result kind of a type family
  deriving( Eq ) -- Just for checkSkolInfoAnon

-- | Report Redundant Constraints.
data ReportRedundantConstraints
  = NoRRC            -- ^ Don't report redundant constraints
  | WantRRC SrcSpan  -- ^ Report redundant constraints, and here
                     -- is the SrcSpan for the constraints
                     -- E.g. f :: (Eq a, Ord b) => blah
                     -- The span is for the (Eq a, Ord b)
  deriving( Eq )  -- Just for checkSkolInfoAnon

reportRedundantConstraints :: ReportRedundantConstraints -> Bool
reportRedundantConstraints NoRRC        = False
reportRedundantConstraints (WantRRC {}) = True

redundantConstraintsSpan :: UserTypeCtxt -> SrcSpan
redundantConstraintsSpan (FunSigCtxt _ (WantRRC span)) = span
redundantConstraintsSpan (ExprSigCtxt (WantRRC span))  = span
redundantConstraintsSpan _ = noSrcSpan

{-
-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll
-- quantify over them:
--      e.g.    type T = a->a
-- will become  type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain.
-}


pprUserTypeCtxt :: UserTypeCtxt -> SDoc
pprUserTypeCtxt (FunSigCtxt n _)  = text "the type signature for" <+> quotes (ppr n)
pprUserTypeCtxt (InfSigCtxt n)    = text "the inferred type for" <+> quotes (ppr n)
pprUserTypeCtxt (RuleSigCtxt _ n) = text "the type signature for" <+> quotes (ppr n)
pprUserTypeCtxt (ExprSigCtxt _)   = text "an expression type signature"
pprUserTypeCtxt KindSigCtxt       = text "a kind signature"
pprUserTypeCtxt (StandaloneKindSigCtxt n) = text "a standalone kind signature for" <+> quotes (ppr n)
pprUserTypeCtxt TypeAppCtxt       = text "a type argument"
pprUserTypeCtxt (ConArgCtxt c)    = text "the type of the constructor" <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)     = text "the RHS of the type synonym" <+> quotes (ppr c)
pprUserTypeCtxt PatSigCtxt        = text "a pattern type signature"
pprUserTypeCtxt (ForSigCtxt n)    = text "the foreign declaration for" <+> quotes (ppr n)
pprUserTypeCtxt DefaultDeclCtxt   = text "a type in a `default' declaration"
pprUserTypeCtxt (InstDeclCtxt False) = text "an instance declaration"
pprUserTypeCtxt (InstDeclCtxt True)  = text "a stand-alone deriving instance declaration"
pprUserTypeCtxt SpecInstCtxt      = text "a SPECIALISE instance pragma"
pprUserTypeCtxt GenSigCtxt        = text "a type expected by the context"
pprUserTypeCtxt (GhciCtxt {})     = text "a type in a GHCi command"
pprUserTypeCtxt (ClassSCCtxt c)   = text "the super-classes of class" <+> quotes (ppr c)
pprUserTypeCtxt SigmaCtxt         = text "the context of a polymorphic type"
pprUserTypeCtxt (DataTyCtxt tc)   = text "the context of the data type declaration for" <+> quotes (ppr tc)
pprUserTypeCtxt (PatSynCtxt n)    = text "the signature for pattern synonym" <+> quotes (ppr n)
pprUserTypeCtxt (DerivClauseCtxt) = text "a `deriving' clause"
pprUserTypeCtxt (TyVarBndrKindCtxt n) = text "the kind annotation on the type variable" <+> quotes (ppr n)
pprUserTypeCtxt (DataKindCtxt n)  = text "the kind annotation on the declaration for" <+> quotes (ppr n)
pprUserTypeCtxt (TySynKindCtxt n) = text "the kind annotation on the declaration for" <+> quotes (ppr n)
pprUserTypeCtxt (TyFamResKindCtxt n) = text "the result kind for" <+> quotes (ppr n)

isSigMaybe :: UserTypeCtxt -> Maybe Name
isSigMaybe (FunSigCtxt n _) = Just n
isSigMaybe (ConArgCtxt n)   = Just n
isSigMaybe (ForSigCtxt n)   = Just n
isSigMaybe (PatSynCtxt n)   = Just n
isSigMaybe _                = Nothing

{-
************************************************************************
*                                                                      *
                SkolemInfo
*                                                                      *
************************************************************************
-}

-- | 'SkolemInfo' stores the origin of a skolem type variable,
-- so that we can display this information to the user in case of a type error.
--
-- The 'Unique' field allows us to report all skolem type variables bound in the
-- same place in a single report.
data SkolemInfo
  = SkolemInfo
      Unique -- ^ used to common up skolem variables bound at the same location (only used in pprSkols)
      SkolemInfoAnon -- ^ the information about the origin of the skolem type variable

instance Uniquable SkolemInfo where
  getUnique (SkolemInfo u _) = u

-- | 'SkolemInfoAnon' stores the origin of a skolem type variable (e.g. bound by
-- a user-written forall, the header of a data declaration, a deriving clause, ...).
--
-- This information is displayed when reporting an error message, such as
--
--  @"Couldn't match 'k' with 'l'"@
--
-- This allows us to explain where the type variable came from.
--
-- When several skolem type variables are bound at once, prefer using 'SkolemInfo',
-- which stores a 'Unique' which allows these type variables to be reported
data SkolemInfoAnon
  = SigSkol -- A skolem that is created by instantiating
            -- a programmer-supplied type signature
            -- Location of the binding site is on the TyVar
            -- See Note [SigSkol SkolemInfo]
       UserTypeCtxt        -- What sort of signature
       TcType              -- Original type signature (before skolemisation)
       [(Name,TcTyVar)]    -- Maps the original name of the skolemised tyvar
                           -- to its instantiated version

  | SigTypeSkol UserTypeCtxt
                 -- like SigSkol, but when we're kind-checking the *type*
                 -- hence, we have less info

  | ForAllSkol  -- Bound by a user-written "forall".
      TyVarBndrs   -- Shows just the binders, used when reporting a bad telescope
                    -- See Note [Checking telescopes] in GHC.Tc.Types.Constraint

  | DerivSkol Type      -- Bound by a 'deriving' clause;
                        -- the type is the instance we are trying to derive

  | InstSkol            -- Bound at an instance decl

  | FamInstSkol         -- Bound at a family instance decl
  | PatSkol             -- An existential type variable bound by a pattern for
      ConLike           -- a data constructor with an existential type.
      (HsMatchContext GhcTc)
             -- e.g.   data T = forall a. Eq a => MkT a
             --        f (MkT x) = ...
             -- The pattern MkT x will allocate an existential type
             -- variable for 'a'.

  | IPSkol [HsIPName]   -- Binding site of an implicit parameter

  | RuleSkol RuleName   -- The LHS of a RULE

  | InferSkol [(Name,TcType)]
                        -- We have inferred a type for these (mutually-recursivive)
                        -- polymorphic Ids, and are now checking that their RHS
                        -- constraints are satisfied.

  | BracketSkol         -- Template Haskell bracket

  | UnifyForAllSkol     -- We are unifying two for-all types
       TcType           -- The instantiated type *inside* the forall

  | TyConSkol TyConFlavour Name  -- bound in a type declaration of the given flavour

  | DataConSkol Name    -- bound as an existential in a Haskell98 datacon decl or
                        -- as any variable in a GADT datacon decl

  | ReifySkol           -- Bound during Template Haskell reification

  | QuantCtxtSkol       -- Quantified context, e.g.
                        --   f :: forall c. (forall a. c a => c [a]) => blah

  | RuntimeUnkSkol      -- Runtime skolem from the GHCi debugger      #14628

  | ArrowReboundIfSkol  -- Bound by the expected type of the rebound arrow ifThenElse command.

  | UnkSkol CallStack


-- | Use this when you can't specify a helpful origin for
-- some skolem type variable.
--
-- We're hoping to be able to get rid of this entirely, but for the moment
-- it's still needed.
unkSkol :: HasCallStack => SkolemInfo
unkSkol = SkolemInfo (mkUniqueGrimily 0) unkSkolAnon

unkSkolAnon :: HasCallStack => SkolemInfoAnon
unkSkolAnon = UnkSkol callStack

-- | Wrap up the origin of a skolem type variable with a new 'Unique',
-- so that we can common up skolem type variables whose 'SkolemInfo'
-- shares a certain 'Unique'.
mkSkolemInfo :: MonadIO m => SkolemInfoAnon -> m SkolemInfo
mkSkolemInfo sk_anon = do
  u <- liftIO $! uniqFromMask 's'
  return (SkolemInfo u sk_anon)

getSkolemInfo :: SkolemInfo -> SkolemInfoAnon
getSkolemInfo (SkolemInfo _ skol_anon) = skol_anon


instance Outputable SkolemInfo where
  ppr (SkolemInfo _ sk_info ) = ppr sk_info

instance Outputable SkolemInfoAnon where
  ppr = pprSkolInfo

pprSkolInfo :: SkolemInfoAnon -> SDoc
-- Complete the sentence "is a rigid type variable bound by..."
pprSkolInfo (SigSkol cx ty _) = pprSigSkolInfo cx ty
pprSkolInfo (SigTypeSkol cx)  = pprUserTypeCtxt cx
pprSkolInfo (ForAllSkol tvs)  = text "an explicit forall" <+> ppr tvs
pprSkolInfo (IPSkol ips)      = text "the implicit-parameter binding" <> plural ips <+> text "for"
                                 <+> pprWithCommas ppr ips
pprSkolInfo (DerivSkol pred)  = text "the deriving clause for" <+> quotes (ppr pred)
pprSkolInfo InstSkol          = text "the instance declaration"
pprSkolInfo FamInstSkol       = text "a family instance declaration"
pprSkolInfo BracketSkol       = text "a Template Haskell bracket"
pprSkolInfo (RuleSkol name)   = text "the RULE" <+> pprRuleName name
pprSkolInfo (PatSkol cl mc)   = sep [ pprPatSkolInfo cl
                                    , text "in" <+> pprMatchContext mc ]
pprSkolInfo (InferSkol ids)   = hang (text "the inferred type" <> plural ids <+> text "of")
                                   2 (vcat [ ppr name <+> dcolon <+> ppr ty
                                           | (name,ty) <- ids ])
pprSkolInfo (UnifyForAllSkol ty)  = text "the type" <+> ppr ty
pprSkolInfo (TyConSkol flav name) = text "the" <+> ppr flav <+> text "declaration for" <+> quotes (ppr name)
pprSkolInfo (DataConSkol name)    = text "the type signature for" <+> quotes (ppr name)
pprSkolInfo ReifySkol             = text "the type being reified"

pprSkolInfo (QuantCtxtSkol {}) = text "a quantified context"
pprSkolInfo RuntimeUnkSkol     = text "Unknown type from GHCi runtime"
pprSkolInfo ArrowReboundIfSkol = text "the expected type of a rebound if-then-else command"

-- unkSkol
-- For type variables the others are dealt with by pprSkolTvBinding.
-- For Insts, these cases should not happen
pprSkolInfo (UnkSkol cs) = text "UnkSkol (please report this as a bug)" $$ prettyCallStackDoc cs


pprSigSkolInfo :: UserTypeCtxt -> TcType -> SDoc
-- The type is already tidied
pprSigSkolInfo ctxt ty
  = case ctxt of
       FunSigCtxt f _ -> vcat [ text "the type signature for:"
                              , nest 2 (pprPrefixOcc f <+> dcolon <+> ppr ty) ]
       PatSynCtxt {}  -> pprUserTypeCtxt ctxt  -- See Note [Skolem info for pattern synonyms]
       _              -> vcat [ pprUserTypeCtxt ctxt <> colon
                              , nest 2 (ppr ty) ]

pprPatSkolInfo :: ConLike -> SDoc
pprPatSkolInfo (RealDataCon dc)
  = sdocOption sdocLinearTypes (\show_linear_types ->
      sep [ text "a pattern with constructor:"
          , nest 2 $ ppr dc <+> dcolon
            <+> pprType (dataConDisplayType show_linear_types dc) <> comma ])
            -- pprType prints forall's regardless of -fprint-explicit-foralls
            -- which is what we want here, since we might be saying
            -- type variable 't' is bound by ...

pprPatSkolInfo (PatSynCon ps)
  = sep [ text "a pattern with pattern synonym:"
        , nest 2 $ ppr ps <+> dcolon
                   <+> pprPatSynType ps <> comma ]

{- Note [Skolem info for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For pattern synonym SkolemInfo we have
   SigSkol (PatSynCtxt p) ty _
but the type 'ty' is not very helpful.  The full pattern-synonym type
has the provided and required pieces, which it is inconvenient to
record and display here. So we simply don't display the type at all,
contenting ourselves with just the name of the pattern synonym, which
is fine.  We could do more, but it doesn't seem worth it.

Note [SigSkol SkolemInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we skolemise a type
   f :: forall a. Eq a => forall b. b -> a
Then we'll instantiate [a :-> a', b :-> b'], and with the instantiated
      a' -> b' -> a.
But when, in an error message, we report that "b is a rigid type
variable bound by the type signature for f", we want to show the foralls
in the right place.  So we proceed as follows:

* In SigSkol we record
    - the original signature forall a. a -> forall b. b -> a
    - the instantiation mapping [a :-> a', b :-> b']

* Then when tidying in GHC.Tc.Utils.TcMType.tidySkolemInfo, we first tidy a' to
  whatever it tidies to, say a''; and then we walk over the type
  replacing the binder a by the tidied version a'', to give
       forall a''. Eq a'' => forall b''. b'' -> a''
  We need to do this under (=>) arrows, to match what topSkolemise
  does.

* Typically a'' will have a nice pretty name like "a", but the point is
  that the foral-bound variables of the signature we report line up with
  the instantiated skolems lying  around in other types.


************************************************************************
*                                                                      *
            CtOrigin
*                                                                      *
************************************************************************
-}

-- | Some thing which has a type.
--
-- This datatype is used when we want to report to the user
-- that something has an unexpected type.
data TypedThing
  = HsTypeRnThing (HsType GhcRn)
  | TypeThing Type
  | HsExprRnThing (HsExpr GhcRn)
  | NameThing Name

-- | Some kind of type variable binder.
--
-- Used for reporting errors, in 'SkolemInfo' and 'TcSolverReportMsg'.
data TyVarBndrs
  = forall flag. OutputableBndrFlag flag 'Renamed =>
      HsTyVarBndrsRn [HsTyVarBndr flag GhcRn]

instance Outputable TypedThing where
  ppr (HsTypeRnThing ty) = ppr ty
  ppr (TypeThing ty) = ppr ty
  ppr (HsExprRnThing expr) = ppr expr
  ppr (NameThing name) = ppr name

instance Outputable TyVarBndrs where
  ppr (HsTyVarBndrsRn bndrs) = fsep (map ppr bndrs)

data CtOrigin
  = -- | A given constraint from a user-written type signature. The
    -- 'SkolemInfo' inside gives more information.
    GivenOrigin SkolemInfoAnon

  -- The following are other origins for given constraints that cannot produce
  -- new skolems -- hence no SkolemInfo.

  -- | 'InstSCOrigin' is used for a Given constraint obtained by superclass selection
  -- from the context of an instance declaration.  E.g.
  --       instance @(Foo a, Bar a) => C [a]@ where ...
  -- When typechecking the instance decl itself, including producing evidence
  -- for the superclasses of @C@, the superclasses of @(Foo a)@ and @(Bar a)@ will
  -- have 'InstSCOrigin' origin.
  | InstSCOrigin ScDepth      -- ^ The number of superclass selections necessary to
                              -- get this constraint; see Note [Replacement vs keeping]
                              -- and Note [Use only the best local instance], both in
                              -- GHC.Tc.Solver.Interact
                 TypeSize     -- ^ If @(C ty1 .. tyn)@ is the largest class from
                              --    which we made a superclass selection in the chain,
                              --    then @TypeSize = sizeTypes [ty1, .., tyn]@
                              -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance

  -- | 'OtherSCOrigin' is used for a Given constraint obtained by superclass
  -- selection from a constraint /other than/ the context of an instance
  -- declaration. (For the latter we use 'InstSCOrigin'.)  E.g.
  --      f :: Foo a => blah
  --      f = e
  -- When typechecking body of 'f', the superclasses of the Given (Foo a)
  -- will have 'OtherSCOrigin'.
  -- Needed for Note [Replacement vs keeping] and
  -- Note [Use only the best local instance], both in GHC.Tc.Solver.Interact.
  | OtherSCOrigin ScDepth -- ^ The number of superclass selections necessary to
                          -- get this constraint
                  SkolemInfoAnon   -- ^ Where the sub-class constraint arose from
                               -- (used only for printing)

  -- All the others are for *wanted* constraints

  | OccurrenceOf Name              -- Occurrence of an overloaded identifier
  | OccurrenceOfRecSel RdrName     -- Occurrence of a record selector
  | AppOrigin                      -- An application of some kind

  | SpecPragOrigin UserTypeCtxt    -- Specialisation pragma for
                                   -- function or instance


  | TypeEqOrigin { uo_actual   :: TcType
                 , uo_expected :: TcType
                 , uo_thing    :: Maybe TypedThing
                       -- ^ The thing that has type "actual"
                 , uo_visible  :: Bool
                       -- ^ Is at least one of the three elements above visible?
                       -- (Errors from the polymorphic subsumption check are considered
                       -- visible.) Only used for prioritizing error messages.
                 }

  | KindEqOrigin
      TcType TcType             -- A kind equality arising from unifying these two types
      CtOrigin                  -- originally arising from this
      (Maybe TypeOrKind)        -- the level of the eq this arises from

  | IPOccOrigin  HsIPName       -- Occurrence of an implicit parameter
  | OverLabelOrigin FastString  -- Occurrence of an overloaded label

  | LiteralOrigin (HsOverLit GhcRn)     -- Occurrence of a literal
  | NegateOrigin                        -- Occurrence of syntactic negation

  | ArithSeqOrigin (ArithSeqInfo GhcRn) -- [x..], [x..y] etc
  | AssocFamPatOrigin   -- When matching the patterns of an associated
                        -- family instance with that of its parent class
                        -- IMPORTANT: These constraints will never cause errors;
                        -- See Note [Constraints to ignore] in GHC.Tc.Errors
  | SectionOrigin
  | HasFieldOrigin FastString
  | TupleOrigin         -- (..,..)
  | ExprSigOrigin       -- e :: ty
  | PatSigOrigin        -- p :: ty
  | PatOrigin           -- Instantiating a polytyped pattern at a constructor
  | ProvCtxtOrigin      -- The "provided" context of a pattern synonym signature
        (PatSynBind GhcRn GhcRn) -- Information about the pattern synonym, in
                                 -- particular the name and the right-hand side
  | RecordUpdOrigin
  | ViewPatOrigin

  -- | 'ScOrigin' is used only for the Wanted constraints for the
  -- superclasses of an instance declaration.
  -- If the instance head is @C ty1 .. tyn@
  --    then @TypeSize = sizeTypes [ty1, .., tyn]@
  -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
  | ScOrigin TypeSize

  | DerivClauseOrigin   -- Typechecking a deriving clause (as opposed to
                        -- standalone deriving).
  | DerivOriginDC DataCon Int Bool
      -- Checking constraints arising from this data con and field index. The
      -- Bool argument in DerivOriginDC and DerivOriginCoerce is True if
      -- standalong deriving (with a wildcard constraint) is being used. This
      -- is used to inform error messages on how to recommended fixes (e.g., if
      -- the argument is True, then don't recommend "use standalone deriving",
      -- but rather "fill in the wildcard constraint yourself").
      -- See Note [Inferring the instance context] in GHC.Tc.Deriv.Infer
  | DerivOriginCoerce Id Type Type Bool
                        -- DerivOriginCoerce id ty1 ty2: Trying to coerce class method `id` from
                        -- `ty1` to `ty2`.
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving. Useful for
                          -- constraints coming from a wildcard constraint,
                          -- e.g., deriving instance _ => Eq (Foo a)
                          -- See Note [Inferring the instance context]
                          -- in GHC.Tc.Deriv.Infer
  | DefaultOrigin       -- Typechecking a default decl
  | DoOrigin            -- Arising from a do expression
  | DoPatOrigin (LPat GhcRn) -- Arising from a failable pattern in
                             -- a do expression
  | MCompOrigin         -- Arising from a monad comprehension
  | MCompPatOrigin (LPat GhcRn) -- Arising from a failable pattern in a
                                -- monad comprehension
  | ProcOrigin          -- Arising from a proc expression
  | ArrowCmdOrigin      -- Arising from an arrow command
  | AnnOrigin           -- An annotation

  | FunDepOrigin1       -- A functional dependency from combining
        PredType CtOrigin RealSrcSpan      -- This constraint arising from ...
        PredType CtOrigin RealSrcSpan      -- and this constraint arising from ...

  | FunDepOrigin2       -- A functional dependency from combining
        PredType CtOrigin   -- This constraint arising from ...
        PredType SrcSpan    -- and this top-level instance
        -- We only need a CtOrigin on the first, because the location
        -- is pinned on the entire error message

  | InjTFOrigin1    -- injective type family equation combining
      PredType CtOrigin RealSrcSpan    -- This constraint arising from ...
      PredType CtOrigin RealSrcSpan    -- and this constraint arising from ...

  | ExprHoleOrigin (Maybe OccName)   -- from an expression hole
  | TypeHoleOrigin OccName   -- from a type hole (partial type signature)
  | PatCheckOrigin      -- normalisation of a type during pattern-match checking
  | ListOrigin          -- An overloaded list
  | IfThenElseOrigin    -- An if-then-else expression
  | BracketOrigin       -- An overloaded quotation bracket
  | StaticOrigin        -- A static form
  | Shouldn'tHappenOrigin String
                            -- the user should never see this one
  | GhcBug20076             -- see #20076

  -- | Testing whether the constraint associated with an instance declaration
  -- in a signature file is satisfied upon instantiation.
  --
  -- Test cases: backpack/should_fail/bkpfail{11,43}.bkp
  | InstProvidedOrigin
      Module  -- ^ Module in which the instance was declared
      ClsInst -- ^ The declared typeclass instance

  | NonLinearPatternOrigin
  | UsageEnvironmentOf Name

  | CycleBreakerOrigin
      CtOrigin   -- origin of the original constraint
      -- See Detail (7) of Note [Type equality cycles] in GHC.Tc.Solver.Canonical
  | FRROrigin
      FixedRuntimeRepOrigin

  | WantedSuperclassOrigin PredType CtOrigin
        -- From expanding out the superclasses of a Wanted; the PredType
        -- is the subclass predicate, and the origin
        -- of the original Wanted is the CtOrigin

  | InstanceSigOrigin   -- from the sub-type check of an InstanceSig
      Name   -- the method name
      Type   -- the instance-sig type
      Type   -- the instantiated type of the method
  | AmbiguityCheckOrigin UserTypeCtxt

-- | The number of superclass selections needed to get this Given.
-- If @d :: C ty@   has @ScDepth=2@, then the evidence @d@ will look
-- like @sc_sel (sc_sel dg)@, where @dg@ is a Given.
type ScDepth = Int

-- An origin is visible if the place where the constraint arises is manifest
-- in user code. Currently, all origins are visible except for invisible
-- TypeEqOrigins. This is used when choosing which error of
-- several to report
isVisibleOrigin :: CtOrigin -> Bool
isVisibleOrigin (TypeEqOrigin { uo_visible = vis }) = vis
isVisibleOrigin (KindEqOrigin _ _ sub_orig _)       = isVisibleOrigin sub_orig
isVisibleOrigin _                                   = True

-- Converts a visible origin to an invisible one, if possible. Currently,
-- this works only for TypeEqOrigin
toInvisibleOrigin :: CtOrigin -> CtOrigin
toInvisibleOrigin orig@(TypeEqOrigin {}) = orig { uo_visible = False }
toInvisibleOrigin orig                   = orig

isGivenOrigin :: CtOrigin -> Bool
isGivenOrigin (GivenOrigin {})              = True
isGivenOrigin (InstSCOrigin {})             = True
isGivenOrigin (OtherSCOrigin {})            = True
isGivenOrigin (CycleBreakerOrigin o)        = isGivenOrigin o
isGivenOrigin _                             = False

-- See Note [Suppressing confusing errors] in GHC.Tc.Errors
isWantedWantedFunDepOrigin :: CtOrigin -> Bool
isWantedWantedFunDepOrigin (FunDepOrigin1 _ orig1 _ _ orig2 _)
  = not (isGivenOrigin orig1) && not (isGivenOrigin orig2)
isWantedWantedFunDepOrigin (InjTFOrigin1 _ orig1 _ _ orig2 _)
  = not (isGivenOrigin orig1) && not (isGivenOrigin orig2)
isWantedWantedFunDepOrigin _ = False

-- | Did a constraint arise from expanding a Wanted constraint
-- to look at superclasses?
isWantedSuperclassOrigin :: CtOrigin -> Bool
isWantedSuperclassOrigin (WantedSuperclassOrigin {}) = True
isWantedSuperclassOrigin _                           = False

instance Outputable CtOrigin where
  ppr = pprCtOrigin

ctoHerald :: SDoc
ctoHerald = text "arising from"

-- | Extract a suitable CtOrigin from a HsExpr
lexprCtOrigin :: LHsExpr GhcRn -> CtOrigin
lexprCtOrigin (L _ e) = exprCtOrigin e

exprCtOrigin :: HsExpr GhcRn -> CtOrigin
exprCtOrigin (HsVar _ (L _ name)) = OccurrenceOf name
exprCtOrigin (HsGetField _ _ (L _ f)) = HasFieldOrigin (unLoc $ dfoLabel f)
exprCtOrigin (HsUnboundVar {})    = Shouldn'tHappenOrigin "unbound variable"
exprCtOrigin (HsRecSel _ f)       = OccurrenceOfRecSel (unLoc $ foLabel f)
exprCtOrigin (HsOverLabel _ l)    = OverLabelOrigin l
exprCtOrigin (ExplicitList {})    = ListOrigin
exprCtOrigin (HsIPVar _ ip)       = IPOccOrigin ip
exprCtOrigin (HsOverLit _ lit)    = LiteralOrigin lit
exprCtOrigin (HsLit {})           = Shouldn'tHappenOrigin "concrete literal"
exprCtOrigin (HsLam _ matches)    = matchesCtOrigin matches
exprCtOrigin (HsLamCase _ _ ms)   = matchesCtOrigin ms
exprCtOrigin (HsApp _ e1 _)       = lexprCtOrigin e1
exprCtOrigin (HsAppType _ e1 _)   = lexprCtOrigin e1
exprCtOrigin (OpApp _ _ op _)     = lexprCtOrigin op
exprCtOrigin (NegApp _ e _)       = lexprCtOrigin e
exprCtOrigin (HsPar _ _ e _)      = lexprCtOrigin e
exprCtOrigin (HsProjection _ _)   = SectionOrigin
exprCtOrigin (SectionL _ _ _)     = SectionOrigin
exprCtOrigin (SectionR _ _ _)     = SectionOrigin
exprCtOrigin (ExplicitTuple {})   = Shouldn'tHappenOrigin "explicit tuple"
exprCtOrigin ExplicitSum{}        = Shouldn'tHappenOrigin "explicit sum"
exprCtOrigin (HsCase _ _ matches) = matchesCtOrigin matches
exprCtOrigin (HsIf {})           = IfThenElseOrigin
exprCtOrigin (HsMultiIf _ rhs)   = lGRHSCtOrigin rhs
exprCtOrigin (HsLet _ _ _ _ e)   = lexprCtOrigin e
exprCtOrigin (HsDo {})           = DoOrigin
exprCtOrigin (RecordCon {})      = Shouldn'tHappenOrigin "record construction"
exprCtOrigin (RecordUpd {})      = RecordUpdOrigin
exprCtOrigin (ExprWithTySig {})  = ExprSigOrigin
exprCtOrigin (ArithSeq {})       = Shouldn'tHappenOrigin "arithmetic sequence"
exprCtOrigin (HsPragE _ _ e)     = lexprCtOrigin e
exprCtOrigin (HsTypedBracket {}) = Shouldn'tHappenOrigin "TH typed bracket"
exprCtOrigin (HsUntypedBracket {}) = Shouldn'tHappenOrigin "TH untyped bracket"
exprCtOrigin (HsSpliceE {})      = Shouldn'tHappenOrigin "TH splice"
exprCtOrigin (HsProc {})         = Shouldn'tHappenOrigin "proc"
exprCtOrigin (HsStatic {})       = Shouldn'tHappenOrigin "static expression"
exprCtOrigin (XExpr (HsExpanded a _)) = exprCtOrigin a

-- | Extract a suitable CtOrigin from a MatchGroup
matchesCtOrigin :: MatchGroup GhcRn (LHsExpr GhcRn) -> CtOrigin
matchesCtOrigin (MG { mg_alts = alts })
  | L _ [L _ match] <- alts
  , Match { m_grhss = grhss } <- match
  = grhssCtOrigin grhss

  | otherwise
  = Shouldn'tHappenOrigin "multi-way match"

-- | Extract a suitable CtOrigin from guarded RHSs
grhssCtOrigin :: GRHSs GhcRn (LHsExpr GhcRn) -> CtOrigin
grhssCtOrigin (GRHSs { grhssGRHSs = lgrhss }) = lGRHSCtOrigin lgrhss

-- | Extract a suitable CtOrigin from a list of guarded RHSs
lGRHSCtOrigin :: [LGRHS GhcRn (LHsExpr GhcRn)] -> CtOrigin
lGRHSCtOrigin [L _ (GRHS _ _ (L _ e))] = exprCtOrigin e
lGRHSCtOrigin _ = Shouldn'tHappenOrigin "multi-way GRHS"

pprCtOrigin :: CtOrigin -> SDoc
-- "arising from ..."
pprCtOrigin (GivenOrigin sk)     = ctoHerald <+> ppr sk
pprCtOrigin (InstSCOrigin {})    = ctoHerald <+> pprSkolInfo InstSkol   -- keep output in sync
pprCtOrigin (OtherSCOrigin _ si) = ctoHerald <+> pprSkolInfo si

pprCtOrigin (SpecPragOrigin ctxt)
  = case ctxt of
       FunSigCtxt n _ -> text "for" <+> quotes (ppr n)
       SpecInstCtxt   -> text "a SPECIALISE INSTANCE pragma"
       _              -> text "a SPECIALISE pragma"  -- Never happens I think

pprCtOrigin (FunDepOrigin1 pred1 orig1 loc1 pred2 orig2 loc2)
  = hang (ctoHerald <+> text "a functional dependency between constraints:")
       2 (vcat [ hang (quotes (ppr pred1)) 2 (pprCtOrigin orig1 <+> text "at" <+> ppr loc1)
               , hang (quotes (ppr pred2)) 2 (pprCtOrigin orig2 <+> text "at" <+> ppr loc2) ])

pprCtOrigin (FunDepOrigin2 pred1 orig1 pred2 loc2)
  = hang (ctoHerald <+> text "a functional dependency between:")
       2 (vcat [ hang (text "constraint" <+> quotes (ppr pred1))
                    2 (pprCtOrigin orig1 )
               , hang (text "instance" <+> quotes (ppr pred2))
                    2 (text "at" <+> ppr loc2) ])

pprCtOrigin (InjTFOrigin1 pred1 orig1 loc1 pred2 orig2 loc2)
  = hang (ctoHerald <+> text "reasoning about an injective type family using constraints:")
       2 (vcat [ hang (quotes (ppr pred1)) 2 (pprCtOrigin orig1 <+> text "at" <+> ppr loc1)
               , hang (quotes (ppr pred2)) 2 (pprCtOrigin orig2 <+> text "at" <+> ppr loc2) ])

pprCtOrigin AssocFamPatOrigin
  = text "when matching a family LHS with its class instance head"

pprCtOrigin (TypeEqOrigin { uo_actual = t1, uo_expected =  t2, uo_visible = vis })
  = hang (ctoHerald <+> text "a type equality" <> whenPprDebug (brackets (ppr vis)))
       2 (sep [ppr t1, char '~', ppr t2])

pprCtOrigin (KindEqOrigin t1 t2 _ _)
  = hang (ctoHerald <+> text "a kind equality arising from")
       2 (sep [ppr t1, char '~', ppr t2])

pprCtOrigin (DerivOriginDC dc n _)
  = hang (ctoHerald <+> text "the" <+> speakNth n
          <+> text "field of" <+> quotes (ppr dc))
       2 (parens (text "type" <+> quotes (ppr (scaledThing ty))))
  where
    ty = dataConOrigArgTys dc !! (n-1)

pprCtOrigin (DerivOriginCoerce meth ty1 ty2 _)
  = hang (ctoHerald <+> text "the coercion of the method" <+> quotes (ppr meth))
       2 (sep [ text "from type" <+> quotes (ppr ty1)
              , nest 2 $ text "to type" <+> quotes (ppr ty2) ])

pprCtOrigin (DoPatOrigin pat)
    = ctoHerald <+> text "a do statement"
      $$
      text "with the failable pattern" <+> quotes (ppr pat)

pprCtOrigin (MCompPatOrigin pat)
    = ctoHerald <+> hsep [ text "the failable pattern"
           , quotes (ppr pat)
           , text "in a statement in a monad comprehension" ]

pprCtOrigin (Shouldn'tHappenOrigin note)
  = vcat [ text "<< This should not appear in error messages. If you see this"
         , text "in an error message, please report a bug mentioning"
             <+> quotes (text note) <+> text "at"
         , text "https://gitlab.haskell.org/ghc/ghc/wikis/report-a-bug >>"
         ]

pprCtOrigin GhcBug20076
  = vcat [ text "GHC Bug #20076 <https://gitlab.haskell.org/ghc/ghc/-/issues/20076>"
         , text "Assuming you have a partial type signature, you can avoid this error"
         , text "by either adding an extra-constraints wildcard (like `(..., _) => ...`,"
         , text "with the underscore at the end of the constraint), or by avoiding the"
         , text "use of a simplifiable constraint in your partial type signature." ]

pprCtOrigin (ProvCtxtOrigin PSB{ psb_id = (L _ name) })
  = hang (ctoHerald <+> text "the \"provided\" constraints claimed by")
       2 (text "the signature of" <+> quotes (ppr name))

pprCtOrigin (InstProvidedOrigin mod cls_inst)
  = vcat [ text "arising when attempting to show that"
         , ppr cls_inst
         , text "is provided by" <+> quotes (ppr mod)]

pprCtOrigin (CycleBreakerOrigin orig)
  = pprCtOrigin orig

pprCtOrigin (FRROrigin {})
  = ctoHerald <+> text "a representation-polymorphism check"

pprCtOrigin (WantedSuperclassOrigin subclass_pred subclass_orig)
  = sep [ ctoHerald <+> text "a superclass required to satisfy" <+> quotes (ppr subclass_pred) <> comma
        , pprCtOrigin subclass_orig ]

pprCtOrigin (InstanceSigOrigin method_name sig_type orig_method_type)
  = vcat [ ctoHerald <+> text "the check that an instance signature is more general"
         , text "than the type of the method (instantiated for this instance)"
         , hang (text "instance signature:")
              2 (ppr method_name <+> dcolon <+> ppr sig_type)
         , hang (text "instantiated method type:")
              2 (ppr orig_method_type) ]

pprCtOrigin (AmbiguityCheckOrigin ctxt)
  = ctoHerald <+> text "a type ambiguity check for" $$
    pprUserTypeCtxt ctxt

pprCtOrigin simple_origin
  = ctoHerald <+> pprCtO simple_origin

-- | Short one-liners
pprCtO :: HasCallStack => CtOrigin -> SDoc
pprCtO (OccurrenceOf name)   = hsep [text "a use of", quotes (ppr name)]
pprCtO (OccurrenceOfRecSel name) = hsep [text "a use of", quotes (ppr name)]
pprCtO AppOrigin             = text "an application"
pprCtO (IPOccOrigin name)    = hsep [text "a use of implicit parameter", quotes (ppr name)]
pprCtO (OverLabelOrigin l)   = hsep [text "the overloaded label"
                                    ,quotes (char '#' <> ppr l)]
pprCtO RecordUpdOrigin       = text "a record update"
pprCtO ExprSigOrigin         = text "an expression type signature"
pprCtO PatSigOrigin          = text "a pattern type signature"
pprCtO PatOrigin             = text "a pattern"
pprCtO ViewPatOrigin         = text "a view pattern"
pprCtO (LiteralOrigin lit)   = hsep [text "the literal", quotes (ppr lit)]
pprCtO (ArithSeqOrigin seq)  = hsep [text "the arithmetic sequence", quotes (ppr seq)]
pprCtO SectionOrigin         = text "an operator section"
pprCtO (HasFieldOrigin f)    = hsep [text "selecting the field", quotes (ppr f)]
pprCtO AssocFamPatOrigin     = text "the LHS of a family instance"
pprCtO TupleOrigin           = text "a tuple"
pprCtO NegateOrigin          = text "a use of syntactic negation"
pprCtO (ScOrigin n)          = text "the superclasses of an instance declaration"
                               <> whenPprDebug (parens (ppr n))
pprCtO DerivClauseOrigin     = text "the 'deriving' clause of a data type declaration"
pprCtO StandAloneDerivOrigin = text "a 'deriving' declaration"
pprCtO DefaultOrigin         = text "a 'default' declaration"
pprCtO DoOrigin              = text "a do statement"
pprCtO MCompOrigin           = text "a statement in a monad comprehension"
pprCtO ProcOrigin            = text "a proc expression"
pprCtO ArrowCmdOrigin        = text "an arrow command"
pprCtO AnnOrigin             = text "an annotation"
pprCtO (ExprHoleOrigin Nothing)    = text "an expression hole"
pprCtO (ExprHoleOrigin (Just occ)) = text "a use of" <+> quotes (ppr occ)
pprCtO (TypeHoleOrigin occ)  = text "a use of wildcard" <+> quotes (ppr occ)
pprCtO PatCheckOrigin        = text "a pattern-match completeness check"
pprCtO ListOrigin            = text "an overloaded list"
pprCtO IfThenElseOrigin      = text "an if-then-else expression"
pprCtO StaticOrigin          = text "a static form"
pprCtO NonLinearPatternOrigin = text "a non-linear pattern"
pprCtO (UsageEnvironmentOf x) = hsep [text "multiplicity of", quotes (ppr x)]
pprCtO BracketOrigin         = text "a quotation bracket"

-- These ones are handled by pprCtOrigin, but we nevertheless sometimes
-- get here via callStackOriginFS, when doing ambiguity checks
-- A bit silly, but no great harm
pprCtO (GivenOrigin {})             = text "a given constraint"
pprCtO (InstSCOrigin {})            = text "the superclass of an instance constraint"
pprCtO (OtherSCOrigin {})           = text "the superclass of a given constraint"
pprCtO (SpecPragOrigin {})          = text "a SPECIALISE pragma"
pprCtO (FunDepOrigin1 {})           = text "a functional dependency"
pprCtO (FunDepOrigin2 {})           = text "a functional dependency"
pprCtO (InjTFOrigin1 {})            = text "an injective type family"
pprCtO (TypeEqOrigin {})            = text "a type equality"
pprCtO (KindEqOrigin {})            = text "a kind equality"
pprCtO (DerivOriginDC {})           = text "a deriving clause"
pprCtO (DerivOriginCoerce {})       = text "a derived method"
pprCtO (DoPatOrigin {})             = text "a do statement"
pprCtO (MCompPatOrigin {})          = text "a monad comprehension pattern"
pprCtO (Shouldn'tHappenOrigin note) = text note
pprCtO (ProvCtxtOrigin {})          = text "a provided constraint"
pprCtO (InstProvidedOrigin {})      = text "a provided constraint"
pprCtO (CycleBreakerOrigin orig)    = pprCtO orig
pprCtO (FRROrigin {})               = text "a representation-polymorphism check"
pprCtO GhcBug20076                  = text "GHC Bug #20076"
pprCtO (WantedSuperclassOrigin {})  = text "a superclass constraint"
pprCtO (InstanceSigOrigin {})       = text "a type signature in an instance"
pprCtO (AmbiguityCheckOrigin {})    = text "a type ambiguity check"

{- *********************************************************************
*                                                                      *
             CallStacks and CtOrigin

    See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
*                                                                      *
********************************************************************* -}

isPushCallStackOrigin :: CtOrigin -> Bool
-- Do we want to solve this IP constraint directly (return False)
-- or push the call site (return True)
-- See Note [Overview of implicit CallStacks] in GHc.Tc.Types.Evidence
isPushCallStackOrigin (IPOccOrigin {}) = False
isPushCallStackOrigin _                = True


callStackOriginFS :: CtOrigin -> FastString
-- This is the string that appears in the CallStack
callStackOriginFS (OccurrenceOf fun) = occNameFS (getOccName fun)
callStackOriginFS orig               = mkFastString (showSDocUnsafe (pprCtO orig))

{-
************************************************************************
*                                                                      *
            Checking for representation polymorphism
*                                                                      *
************************************************************************

Note [Reporting representation-polymorphism errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete,
to check that (ty :: ki) has a fixed runtime representation, we emit
an equality constraint of the form

  ki ~# concrete_tv

where concrete_tv is a concrete metavariable. In this situation, we attach
a 'FixedRuntimeRepOrigin' to both the equality and the concrete type variable.
The 'FixedRuntimeRepOrigin' consists of two pieces of information:

  - the type 'ty' on which we performed the representation-polymorphism check,
  - a 'FixedRuntimeRepContext' which explains why we needed to perform a check
    (e.g. because 'ty' was the kind of a function argument, or of a bound variable
    in a lambda abstraction, ...).

This information gets passed along as we make progress on solving the constraint,
and if we end up with an unsolved constraint we can report an informative error
message to the user using the 'FixedRuntimeRepOrigin'.

The error reporting goes through two different paths:

  - constraints whose 'CtOrigin' contains a 'FixedRuntimeRepOrigin' are reported
    using 'mkFRRErr' in 'reportWanteds',
  - equality constraints in which one side is a concrete metavariable and the
    other side is not concrete are reported using 'mkTyVarEqErr'. In this case,
    we pass on the type variable and the non-concrete type for error reporting,
    using the 'frr_info_not_concrete' field.

This is why we have the 'FixedRuntimeRepErrorInfo' datatype: so that we can optionally
include this extra message about an unsolved equality between a concrete type variable
and a non-concrete type.
-}

-- | The context for a representation-polymorphism check.
--
-- For example, when typechecking @ \ (a :: k) -> ...@,
-- we are checking the type @a@ because it's the type of
-- a term variable bound in a lambda, so we use 'FRRBinder'.
data FixedRuntimeRepOrigin
  = FixedRuntimeRepOrigin
    { frr_type    :: Type
       -- ^ What type are we checking?
       -- For example, `a[tau]` in `a[tau] :: TYPE rr[tau]`.

    , frr_context :: FixedRuntimeRepContext
      -- ^ What context requires a fixed runtime representation?
    }

-- | The context in which a representation-polymorphism check was performed.
--
-- Does not include the type on which the check was performed; see
-- 'FixedRuntimeRepOrigin' for that.
data FixedRuntimeRepContext

  -- | Record fields in record updates must have a fixed runtime representation.
  --
  -- Test case: RepPolyRecordUpdate.
  = FRRRecordUpdate !RdrName !(HsExpr GhcTc)

  -- | Variable binders must have a fixed runtime representation.
  --
  -- Test cases: LevPolyLet, RepPolyPatBind.
  | FRRBinder !Name

  -- | Pattern binds must have a fixed runtime representation.
  --
  -- Test case: RepPolyInferPatBind.
  | FRRPatBind

  -- | Pattern synonym arguments must have a fixed runtime representation.
  --
  -- Test case: RepPolyInferPatSyn.
  | FRRPatSynArg

  -- | The type of the scrutinee in a case statement must have a
  -- fixed runtime representation.
  --
  -- Test cases: RepPolyCase{1,2}.
  | FRRCase

  -- | An instantiation of a newtype/data constructor in which
  -- an argument type does not have a fixed runtime representation.
  --
  -- The argument can either be an expression or a pattern.
  --
  -- Test cases:
  --  Expression: UnliftedNewtypesLevityBinder.
  --     Pattern: T20363.
  | FRRDataConArg !ExprOrPat !DataCon !Int

  -- | An instantiation of an 'Id' with no binding (e.g. `coerce`, `unsafeCoerce#`)
  -- in which one of the remaining arguments types does not have a fixed runtime representation.
  --
  -- Test cases: RepPolyWrappedVar, T14561, UnliftedNewtypesCoerceFail.
  | FRRNoBindingResArg !Id !Int

  -- | Arguments to unboxed tuples must have fixed runtime representations.
  --
  -- Test case: RepPolyTuple.
  | FRRTupleArg !Int

  -- | Tuple sections must have a fixed runtime representation.
  --
  -- Test case: RepPolyTupleSection.
  | FRRTupleSection !Int

  -- | Unboxed sums must have a fixed runtime representation.
  --
  -- Test cases: RepPolySum.
  | FRRUnboxedSum

  -- | The body of a @do@ expression or a monad comprehension must
  -- have a fixed runtime representation.
  --
  -- Test cases: RepPolyDoBody{1,2}, RepPolyMcBody.
  | FRRBodyStmt !StmtOrigin !Int

  -- | Arguments to a guard in a monad comprehesion must have
  -- a fixed runtime representation.
  --
  -- Test case: RepPolyMcGuard.
  | FRRBodyStmtGuard

  -- | Arguments to `(>>=)` arising from a @do@ expression
  -- or a monad comprehension must have a fixed runtime representation.
  --
  -- Test cases: RepPolyDoBind, RepPolyMcBind.
  | FRRBindStmt !StmtOrigin

  -- | A value bound by a pattern guard must have a fixed runtime representation.
  --
  -- Test cases: none.
  | FRRBindStmtGuard

  -- | A representation-polymorphism check arising from arrow notation.
  --
  -- See 'FRRArrowContext' for more details.
  | FRRArrow !FRRArrowContext

  -- | A representation-polymorphic check arising from a call
  -- to 'matchExpectedFunTys' or 'matchActualFunTySigma'.
  --
  -- See 'ExpectedFunTyOrigin' for more details.
  | FRRExpectedFunTy
      !ExpectedFunTyOrigin
      !Int
        -- ^ argument position (1-indexed)

-- | Print the context for a @FixedRuntimeRep@ representation-polymorphism check.
--
-- Note that this function does not include the specific 'RuntimeRep'
-- which is not fixed. That information is stored in 'FixedRuntimeRepOrigin'
-- and is reported separately.
pprFixedRuntimeRepContext :: FixedRuntimeRepContext -> SDoc
pprFixedRuntimeRepContext (FRRRecordUpdate lbl _arg)
  = sep [ text "The record update at field"
        , quotes (ppr lbl) ]
pprFixedRuntimeRepContext (FRRBinder binder)
  = sep [ text "The binder"
        , quotes (ppr binder) ]
pprFixedRuntimeRepContext FRRPatBind
  = text "The pattern binding"
pprFixedRuntimeRepContext FRRPatSynArg
  = text "The pattern synonym argument pattern"
pprFixedRuntimeRepContext FRRCase
  = text "The scrutinee of the case statement"
pprFixedRuntimeRepContext (FRRDataConArg expr_or_pat con i)
  = text "The" <+> what
  where
    arg, what :: SDoc
    arg = case expr_or_pat of
      Expression -> text "argument"
      Pattern    -> text "pattern"
    what
      | isNewDataCon con
      = text "newtype constructor" <+> arg
      | otherwise
      = text "data constructor" <+> arg <+> text "in" <+> speakNth i <+> text "position"
pprFixedRuntimeRepContext (FRRNoBindingResArg fn i)
  = vcat [ text "Unsaturated use of a representation-polymorphic primitive function."
         , text "The" <+> speakNth i <+> text "argument of" <+> quotes (ppr $ getName fn) ]
pprFixedRuntimeRepContext (FRRTupleArg i)
  = text "The tuple argument in" <+> speakNth i <+> text "position"
pprFixedRuntimeRepContext (FRRTupleSection i)
  = text "The" <+> speakNth i <+> text "component of the tuple section"
pprFixedRuntimeRepContext FRRUnboxedSum
  = text "The unboxed sum"
pprFixedRuntimeRepContext (FRRBodyStmt stmtOrig i)
  = vcat [ text "The" <+> speakNth i <+> text "argument to (>>)" <> comma
         , text "arising from the" <+> ppr stmtOrig <> comma ]
pprFixedRuntimeRepContext FRRBodyStmtGuard
  = vcat [ text "The argument to" <+> quotes (text "guard") <> comma
         , text "arising from the" <+> ppr MonadComprehension <> comma ]
pprFixedRuntimeRepContext (FRRBindStmt stmtOrig)
  = vcat [ text "The first argument to (>>=)" <> comma
         , text "arising from the" <+> ppr stmtOrig <> comma ]
pprFixedRuntimeRepContext FRRBindStmtGuard
  = sep [ text "The body of the bind statement" ]
pprFixedRuntimeRepContext (FRRArrow arrowContext)
  = pprFRRArrowContext arrowContext
pprFixedRuntimeRepContext (FRRExpectedFunTy funTyOrig arg_pos)
  = pprExpectedFunTyOrigin funTyOrig arg_pos

instance Outputable FixedRuntimeRepContext where
  ppr = pprFixedRuntimeRepContext

-- | Are we in a @do@ expression or a monad comprehension?
--
-- This datatype is only used to report this context to the user in error messages.
data StmtOrigin
  = MonadComprehension
  | DoNotation

instance Outputable StmtOrigin where
  ppr MonadComprehension = text "monad comprehension"
  ppr DoNotation         = quotes ( text "do" ) <+> text "statement"

{- *********************************************************************
*                                                                      *
                       FixedRuntimeRep: arrows
*                                                                      *
********************************************************************* -}

-- | While typechecking arrow notation, in which context
-- did a representation polymorphism check arise?
--
-- See 'FixedRuntimeRepContext' for more general origins of
-- representation polymorphism checks.
data FRRArrowContext

  -- | The result of an arrow command does not have a fixed runtime representation.
  --
  -- Test case: RepPolyArrowCmd.
  = ArrowCmdResTy !(HsCmd GhcRn)

  -- | The argument to an arrow in an arrow command application does not have
  -- a fixed runtime representation.
  --
  -- Test cases: none.
  | ArrowCmdApp !(HsCmd GhcRn) !(HsExpr GhcRn)

  -- | A function in an arrow application does not have
  -- a fixed runtime representation.
  --
  -- Test cases: none.
  | ArrowCmdArrApp !(HsExpr GhcRn) !(HsExpr GhcRn) !HsArrAppType

  -- | The scrutinee type in an arrow command case statement does not have a
  -- fixed runtime representation.
  --
  -- Test cases: none.
  | ArrowCmdCase

  -- | The overall type of an arrow proc expression does not have
  -- a fixed runtime representation.
  --
  -- Test case: RepPolyArrowFun.
  | ArrowFun !(HsExpr GhcRn)

pprFRRArrowContext :: FRRArrowContext -> SDoc
pprFRRArrowContext (ArrowCmdResTy cmd)
  = vcat [ hang (text "The arrow command") 2 (quotes (ppr cmd)) ]
pprFRRArrowContext (ArrowCmdApp fun arg)
  = vcat [ text "The argument in the arrow command application of"
         , nest 2 (quotes (ppr fun))
         , text "to"
         , nest 2 (quotes (ppr arg)) ]
pprFRRArrowContext (ArrowCmdArrApp fun arg ho_app)
  = vcat [ text "The function in the" <+> pprHsArrType ho_app <+> text "of"
         , nest 2 (quotes (ppr fun))
         , text "to"
         , nest 2 (quotes (ppr arg)) ]
pprFRRArrowContext ArrowCmdCase
  = text "The scrutinee of the arrow case command"
pprFRRArrowContext (ArrowFun fun)
  = vcat [ text "The return type of the arrow function"
         , nest 2 (quotes (ppr fun)) ]

instance Outputable FRRArrowContext where
  ppr = pprFRRArrowContext

{- *********************************************************************
*                                                                      *
              FixedRuntimeRep: ExpectedFunTy origin
*                                                                      *
********************************************************************* -}

-- | In what context are we calling 'matchExpectedFunTys'
-- or 'matchActualFunTySigma'?
--
-- Used for two things:
--
--  1. Reporting error messages which explain that a function has been
--     given an unexpected number of arguments.
--     Uses 'pprExpectedFunTyHerald'.
--     See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
--
--  2. Reporting representation-polymorphism errors when a function argument
--     doesn't have a fixed RuntimeRep as per Note [Fixed RuntimeRep]
--     in GHC.Tc.Utils.Concrete.
--     Uses 'pprExpectedFunTyOrigin'.
--     See 'FixedRuntimeRepContext' for the situations in which
--     representation-polymorphism checks are performed.
data ExpectedFunTyOrigin

  -- | A rebindable syntax operator is expected to have a function type.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyDoBind, RepPolyDoBody{1,2}, RepPolyMc{Bind,Body,Guard}, RepPolyNPlusK
  = ExpectedFunTySyntaxOp
    !CtOrigin
    !(HsExpr GhcRn)
      -- ^ rebindable syntax operator

  -- | A view pattern must have a function type.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyBinder
  | ExpectedFunTyViewPat
    !(HsExpr GhcRn)
      -- ^ function used in the view pattern

  -- | Need to be able to extract an argument type from a function type.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyApp
  | forall (p :: Pass)
      . (OutputableBndrId p)
      => ExpectedFunTyArg
          !TypedThing
            -- ^ function
          !(HsExpr (GhcPass p))
            -- ^ argument

  -- | Ensure that a function defined by equations indeed has a function type
  -- with the appropriate number of arguments.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyBinder, RepPolyRecordPattern, RepPolyWildcardPattern
  | ExpectedFunTyMatches
      !TypedThing
        -- ^ name of the function
      !(MatchGroup GhcRn (LHsExpr GhcRn))
       -- ^ equations

  -- | Ensure that a lambda abstraction has a function type.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyLambda
  | ExpectedFunTyLam
      !(MatchGroup GhcRn (LHsExpr GhcRn))

  -- | Ensure that a lambda case expression has a function type.
  --
  -- Test cases for representation-polymorphism checks:
  --   RepPolyMatch
  | ExpectedFunTyLamCase
      LamCaseVariant
      !(HsExpr GhcRn)
       -- ^ the entire lambda-case expression

pprExpectedFunTyOrigin :: ExpectedFunTyOrigin
                       -> Int -- ^ argument position (starting at 1)
                       -> SDoc
pprExpectedFunTyOrigin funTy_origin i =
  case funTy_origin of
    ExpectedFunTySyntaxOp orig op ->
      vcat [ sep [ the_arg_of
                 , text "the rebindable syntax operator"
                 , quotes (ppr op) ]
           , nest 2 (ppr orig) ]
    ExpectedFunTyViewPat expr ->
      vcat [ the_arg_of <+> text "the view pattern"
           , nest 2 (ppr expr) ]
    ExpectedFunTyArg fun arg ->
      sep [ text "The argument"
          , quotes (ppr arg)
          , text "of"
          , quotes (ppr fun) ]
    ExpectedFunTyMatches fun (MG { mg_alts = L _ alts })
      | null alts
      -> the_arg_of <+> quotes (ppr fun)
      | otherwise
      -> text "The" <+> speakNth i <+> text "pattern in the equation" <> plural alts
     <+> text "for" <+> quotes (ppr fun)
    ExpectedFunTyLam {} -> binder_of $ text "lambda"
    ExpectedFunTyLamCase lc_variant _ -> binder_of $ lamCaseKeyword lc_variant
  where
    the_arg_of :: SDoc
    the_arg_of = text "The" <+> speakNth i <+> text "argument of"

    binder_of :: SDoc -> SDoc
    binder_of what = text "The binder of the" <+> what <+> text "expression"

pprExpectedFunTyHerald :: ExpectedFunTyOrigin -> SDoc
pprExpectedFunTyHerald (ExpectedFunTySyntaxOp {})
  = text "This rebindable syntax expects a function with"
pprExpectedFunTyHerald (ExpectedFunTyViewPat {})
  = text "A view pattern expression expects"
pprExpectedFunTyHerald (ExpectedFunTyArg fun _)
  = sep [ text "The function" <+> quotes (ppr fun)
        , text "is applied to" ]
pprExpectedFunTyHerald (ExpectedFunTyMatches fun (MG { mg_alts = L _ alts }))
  = text "The equation" <> plural alts <+> text "for" <+> quotes (ppr fun) <+> hasOrHave alts
pprExpectedFunTyHerald (ExpectedFunTyLam match)
  = sep [ text "The lambda expression" <+>
                   quotes (pprSetDepth (PartWay 1) $
                           pprMatches match)
        -- The pprSetDepth makes the lambda abstraction print briefly
        , text "has" ]
pprExpectedFunTyHerald (ExpectedFunTyLamCase _ expr)
  = sep [ text "The function" <+> quotes (ppr expr)
        , text "requires" ]
