
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

-- | Describes the provenance of types as they flow through the type-checker.
-- The datatypes here are mainly used for error message generation.
module GHC.Tc.Types.Origin (
  -- UserTypeCtxt
  UserTypeCtxt(..), pprUserTypeCtxt, isSigMaybe,
  ReportRedundantConstraints(..), reportRedundantConstraints,
  redundantConstraintsSpan,

  -- SkolemInfo
  SkolemInfo(..), pprSigSkolInfo, pprSkolInfo, unkSkol,

  -- CtOrigin
  CtOrigin(..), exprCtOrigin, lexprCtOrigin, matchesCtOrigin, grhssCtOrigin,
  isVisibleOrigin, toInvisibleOrigin,
  pprCtOrigin, isGivenOrigin,

  -- CtOrigin and CallStack
  isPushCallStackOrigin, callStackOriginFS,
  -- FixedRuntimeRep origin
  FRROrigin(..), pprFRROrigin,
  StmtOrigin(..),

  -- Arrow command origin
  FRRArrowOrigin(..), pprFRRArrowOrigin,
  -- HsWrapper WpFun origin
  WpFunOrigin(..), pprWpFunOrigin,

  ) where

import GHC.Prelude
import GHC.Utils.Misc (HasCallStack)

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
import GHC.Utils.Trace
import GHC.Stack

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
  | RuleSigCtxt Name    -- LHS of a RULE forall
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
  deriving (Eq)

-- | Report Redundant Constraints.
data ReportRedundantConstraints
  = NoRRC            -- ^ Don't report redundant constraints
  | WantRRC SrcSpan  -- ^ Report redundant constraints, and here
                     -- is the SrcSpan for the constraints
                     -- E.g. f :: (Eq a, Ord b) => blah
                     -- The span is for the (Eq a, Ord b)
  deriving (Eq)

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
pprUserTypeCtxt (RuleSigCtxt n)   = text "the type signature for" <+> quotes (ppr n)
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

-- SkolemInfo gives the origin of *given* constraints
--   a) type variables are skolemised
--   b) an implication constraint is generated
data SkolemInfo
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
       SDoc        -- Shows just the binders, used when reporting a bad telescope
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

  | TyConSkol CallStack TyConFlavour Name  -- bound in a type declaration of the given flavour

  | DataConSkol Name    -- bound as an existential in a Haskell98 datacon decl or
                        -- as any variable in a GADT datacon decl

  | ReifySkol           -- Bound during Template Haskell reification

  | QuantCtxtSkol       -- Quantified context, e.g.
                        --   f :: forall c. (forall a. c a => c [a]) => blah

  | RuntimeUnkSkol      -- Runtime skolem from the GHCi debugger      #14628

  | UnkSkol CallStack            -- Unhelpful info (until I improve it)



instance Eq SkolemInfo where
  (SigSkol uc t _) == (SigSkol uc' t' _) = uc == uc' && t `eqType` t'
  (SigTypeSkol uc) == (SigTypeSkol uc')  = uc == uc'
  (ForAllSkol {})  == ForAllSkol {}      = True


unkSkol :: HasCallStack => SkolemInfo
unkSkol = UnkSkol callStack

instance Outputable SkolemInfo where
  ppr = pprSkolInfo

pprSkolInfo :: SkolemInfo -> SDoc
-- Complete the sentence "is a rigid type variable bound by..."
pprSkolInfo (SigSkol cx ty _) = pprSigSkolInfo cx ty
pprSkolInfo (SigTypeSkol cx)  = pprUserTypeCtxt cx
pprSkolInfo (ForAllSkol tvs)  = text "an explicit forall" <+> tvs
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
pprSkolInfo (UnifyForAllSkol ty) = text "the type" <+> ppr ty
pprSkolInfo (TyConSkol cs flav name) = text "the" <+> ppr flav <+> text "declaration for" <+> quotes (ppr name) $$ prettyCallStackDoc cs
pprSkolInfo (DataConSkol name)= text "the data constructor" <+> quotes (ppr name)
pprSkolInfo ReifySkol         = text "the type being reified"

pprSkolInfo (QuantCtxtSkol {}) = text "a quantified context"
pprSkolInfo RuntimeUnkSkol     = text "Unknown type from GHCi runtime"

-- unkSkol
-- For type variables the others are dealt with by pprSkolTvBinding.
-- For Insts, these cases should not happen
pprSkolInfo (UnkSkol cs) = warnPprTrace True (text "pprSkolInfo: unkSkol") $ text "UnkSkol" $$ prettyCallStackDoc cs

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

data CtOrigin
  = -- | A given constraint from a user-written type signature. The
    -- 'SkolemInfo' inside gives more information.
    GivenOrigin SkolemInfo

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
                  SkolemInfo   -- ^ Where the sub-class constraint arose from
                               -- (used only for printing)

  -- All the others are for *wanted* constraints

  | OccurrenceOf Name              -- Occurrence of an overloaded identifier
  | OccurrenceOfRecSel RdrName     -- Occurrence of a record selector
  | AppOrigin                      -- An application of some kind

  | SpecPragOrigin UserTypeCtxt    -- Specialisation pragma for
                                   -- function or instance

  | TypeEqOrigin { uo_actual   :: TcType
                 , uo_expected :: TcType
                 , uo_thing    :: Maybe SDoc
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

  | ExprHoleOrigin OccName   -- from an expression hole
  | TypeHoleOrigin OccName   -- from a type hole (partial type signature)
  | PatCheckOrigin      -- normalisation of a type during pattern-match checking
  | ListOrigin          -- An overloaded list
  | IfThenElseOrigin    -- An if-then-else expression
  | BracketOrigin       -- An overloaded quotation bracket
  | StaticOrigin        -- A static form
  | Shouldn'tHappenOrigin String
                            -- the user should never see this one,
                            -- unless ImpredicativeTypes is on, where all
                            -- bets are off

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
      -- See Detail (7) of Note [Type variable cycles] in GHC.Tc.Solver.Canonical
  | FixedRuntimeRepOrigin
      !Type -- ^ The type being checked for representation polymorphism.
            -- We record it here for access in 'GHC.Tc.Errors.mkFRRErr'.
      !FRROrigin

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
isGivenOrigin (FunDepOrigin1 _ o1 _ _ o2 _) = isGivenOrigin o1 && isGivenOrigin o2
isGivenOrigin (FunDepOrigin2 _ o1 _ _)      = isGivenOrigin o1
isGivenOrigin (CycleBreakerOrigin o)        = isGivenOrigin o
isGivenOrigin _                             = False

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
exprCtOrigin (HsLamCase _ ms)     = matchesCtOrigin ms
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
exprCtOrigin (HsBracket {})      = Shouldn'tHappenOrigin "TH bracket"
exprCtOrigin (HsRnBracketOut {})= Shouldn'tHappenOrigin "HsRnBracketOut"
exprCtOrigin (HsTcBracketOut {})= panic "exprCtOrigin HsTcBracketOut"
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
-- Not an instance of Outputable because of the "arising from" prefix
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

pprCtOrigin AssocFamPatOrigin
  = text "when matching a family LHS with its class instance head"

pprCtOrigin (TypeEqOrigin { uo_actual = t1, uo_expected =  t2, uo_visible = vis })
  = text "a type equality" <> brackets (ppr vis) <+> sep [ppr t1, char '~', ppr t2]

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
  = sdocOption sdocImpredicativeTypes $ \case
      True  -> text "a situation created by impredicative types"
      False -> vcat [ text "<< This should not appear in error messages. If you see this"
                    , text "in an error message, please report a bug mentioning"
                        <+> quotes (text note) <+> text "at"
                    , text "https://gitlab.haskell.org/ghc/ghc/wikis/report-a-bug >>"
                    ]

pprCtOrigin (ProvCtxtOrigin PSB{ psb_id = (L _ name) })
  = hang (ctoHerald <+> text "the \"provided\" constraints claimed by")
       2 (text "the signature of" <+> quotes (ppr name))

pprCtOrigin (InstProvidedOrigin mod cls_inst)
  = vcat [ text "arising when attempting to show that"
         , ppr cls_inst
         , text "is provided by" <+> quotes (ppr mod)]

pprCtOrigin (CycleBreakerOrigin orig)
  = pprCtOrigin orig

pprCtOrigin (FixedRuntimeRepOrigin _ frrOrig)
  -- We ignore the type argument, as we would prefer
  -- to report all types that don't have a fixed runtime representation at once,
  -- in 'GHC.Tc.Errors.mkFRRErr'.
  = pprFRROrigin frrOrig

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
pprCtO (ExprHoleOrigin occ)  = text "a use of" <+> quotes (ppr occ)
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
pprCtO (FixedRuntimeRepOrigin {})   = text "a representation polymorphism check"

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
When we emit a 'Concrete#' Wanted constraint using GHC.Tc.Utils.Concrete.hasFixedRuntimeRep,
we provide a 'CtOrigin' using the 'FixedRuntimeRepOrigin' constructor of,
which keeps track of two things:
  - the type which we want to ensure has a fixed runtime representation,
  - the 'FRROrigin' explaining the nature of the check, e.g. a pattern,
    a function application, a record update, ...

If the constraint goes unsolved, we report it as follows:
  - we detect that the unsolved Wanted is a Concrete# constraint in
    GHC.Tc.Errors.reportWanteds using is_FRR,
  - we assemble an error message in GHC.Tc.Errors.mkFRRErr.

For example, if we try to write the program

  foo :: forall r1 r2 (a :: TYPE r1) (b :: TYPE r2). a -> b -> ()
  foo x y = ()

we will get two unsolved Concrete# wanted constraints, namely
'Concrete# r1' and 'Concrete# r2', and their 'CtOrigin's will be:

  FixedRuntimeRepOrigin a (FRRVarPattern x)
  FixedRuntimeRepOrigin b (FRRVarPattern y)

These constraints will be processed in tandem by mkFRRErr,
producing an error message of the form:

  Representation-polymorphic types are not allowed here.
    * The variable 'x' bound by the pattern
      does not have a fixed runtime representation:
        a :: TYPE r1
    * The variable 'y' bound by the pattern
      does not have a fixed runtime representation:
        b :: TYPE r2
-}

-- | Where are we checking that a type has a fixed runtime representation?
-- Equivalently: what is the origin of an emitted 'Concrete#' constraint?
data FRROrigin

  -- | Function arguments must have a fixed runtime representation.
  --
  -- Test case: RepPolyApp.
  = FRRApp !(HsExpr GhcRn)

  -- | Record fields in record updates must have a fixed runtime representation.
  --
  -- Test case: RepPolyRecordUpdate.
  | FRRRecordUpdate !RdrName !(HsExpr GhcRn)

  -- | Variable binders must have a fixed runtime representation.
  --
  -- Test cases: LevPolyLet, RepPolyPatBind.
  | FRRBinder !Name

  -- | The type of a pattern in a match group must have a fixed runtime representation.
  --
  -- This rules out:
  --   - individual patterns which don't have a fixed runtime representation,
  --   - a representation-polymorphic empty case statement,
  --   - representation-polymorphic GADT pattern matches
  --     in which individual pattern types have a fixed runtime representation.
  --
  -- Test cases: RepPolyRecordPattern, RepPolyUnboxedPatterns,
  --             RepPolyBinder, RepPolyWildcardPattern, RepPolyMatch,
  --             RepPolyNPlusK, RepPolyPatBind, T20426.
  | FRRMatch !(HsMatchContext GhcTc) !Int

  -- | An instantiation of a newtype/data constructor in which
  -- one of the remaining arguments types does not have a fixed runtime representation.
  --
  -- Test case: UnliftedNewtypesLevityBinder.
  | FRRDataConArg !DataCon !Int

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
  -- See 'FRRArrowOrigin' for more details.
  | FRRArrow !FRRArrowOrigin

  -- | A representation-polymorphic check arising from an 'HsWrapper'.
  --
  -- See 'WpFunOrigin' for more details.
  | FRRWpFun !WpFunOrigin

-- | Print the context for a @FixedRuntimeRep@ representation-polymorphism check.
--
-- Note that this function does not include the specific 'RuntimeRep'
-- which is not fixed. That information is added by 'GHC.Tc.Errors.mkFRRErr'.
pprFRROrigin :: FRROrigin -> SDoc
pprFRROrigin (FRRApp arg)
  = sep [ text "The function argument"
        , nest 2 $ quotes (ppr arg)
        , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRRecordUpdate lbl _arg)
  = hsep [ text "The record update at field"
         , quotes (ppr lbl)
         , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRBinder binder)
  = hsep [ text "The binder"
         , quotes (ppr binder)
         , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRMatch matchCtxt i)
  = vcat [ text "The type of the" <+> speakNth i <+> text "pattern in the" <+> pprMatchContextNoun matchCtxt
         , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRDataConArg con i)
  = sep [ text "The" <+> what
        , text "does not have a fixed runtime representation"]
  where
    what :: SDoc
    what
      | isNewDataCon con
      = text "newtype constructor argument"
      | otherwise
      = text "data constructor argument in" <+> speakNth i <+> text "position"
pprFRROrigin (FRRNoBindingResArg fn i)
  = vcat [ text "Unsaturated use of a representation-polymorphic primitive function."
         , text "The" <+> speakNth i <+> text "argument of" <+> quotes (ppr $ getName fn)
         , text "does not have a fixed runtime representation" ]
pprFRROrigin (FRRTupleArg i)
  = hsep [ text "The tuple argument in" <+> speakNth i <+> text "position"
         , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRTupleSection i)
  = hsep [ text "The tuple section does not have a fixed runtime representation"
         , text "in the" <+> speakNth i <+> text "position" ]
pprFRROrigin FRRUnboxedSum
  = hsep [ text "The unboxed sum result type"
         , text "does not have a fixed runtime representation"]
pprFRROrigin (FRRBodyStmt stmtOrig i)
  = vcat [ text "The" <+> speakNth i <+> text "argument to (>>)" <> comma
         , text "arising from the" <+> ppr stmtOrig <> comma
         , text "does not have a fixed runtime representation" ]
pprFRROrigin FRRBodyStmtGuard
  = vcat [ text "The argument to" <+> quotes (text "guard") <> comma
         , text "arising from the" <+> ppr MonadComprehension <> comma
         , text "does not have a fixed runtime representation" ]
pprFRROrigin (FRRBindStmt stmtOrig)
  = vcat [ text "The first argument to (>>=)" <> comma
         , text "arising from the" <+> ppr stmtOrig <> comma
         , text "does not have a fixed runtime representation" ]
pprFRROrigin FRRBindStmtGuard
  = hsep [ text "The return type of the bind statement"
         , text "does not have a fixed runtime representation" ]
pprFRROrigin (FRRArrow arrowOrig)
  = pprFRRArrowOrigin arrowOrig
pprFRROrigin (FRRWpFun wpFunOrig)
  = pprWpFunOrigin wpFunOrig

instance Outputable FRROrigin where
  ppr = pprFRROrigin

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
-- See 'FRROrigin' for more general origins of representation polymorphism checks.
data FRRArrowOrigin

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

  -- | A pattern in an arrow command abstraction does not have
  -- a fixed runtime representation.
  --
  -- Test cases: none.
  | ArrowCmdLam !Int

  -- | The overall type of an arrow proc expression does not have
  -- a fixed runtime representation.
  --
  -- Test case: RepPolyArrowFun.
  | ArrowFun !(HsExpr GhcRn)

pprFRRArrowOrigin :: FRRArrowOrigin -> SDoc
pprFRRArrowOrigin (ArrowCmdResTy cmd)
  = vcat [ hang (text "The arrow command") 2 (quotes (ppr cmd))
         , text "does not have a fixed runtime representation" ]
pprFRRArrowOrigin (ArrowCmdApp fun arg)
  = vcat [ text "In the arrow command application of"
         , nest 2 (quotes (ppr fun))
         , text "to"
         , nest 2 (quotes (ppr arg)) <> comma
         , text "the argument does not have a fixed runtime representation" ]
pprFRRArrowOrigin (ArrowCmdArrApp fun arg ho_app)
  = vcat [ text "In the" <+> pprHsArrType ho_app <+> text "of"
         , nest 2 (quotes (ppr fun))
         , text "to"
         , nest 2 (quotes (ppr arg)) <> comma
         , text "the function does not have a fixed runtime representation" ]
pprFRRArrowOrigin (ArrowCmdLam i)
  = vcat [ text "The" <+> speakNth i <+> text "pattern of the arrow command abstraction"
         , text "does not have a fixed runtime representation" ]
pprFRRArrowOrigin (ArrowFun fun)
  = vcat [ text "The return type of the arrow function"
         , nest 2 (quotes (ppr fun))
         , text "does not have a fixed runtime representation" ]

instance Outputable FRRArrowOrigin where
  ppr = pprFRRArrowOrigin

{- *********************************************************************
*                                                                      *
              FixedRuntimeRep: HsWrapper WpFun origin
*                                                                      *
********************************************************************* -}

-- | While typechecking a 'WpFun' 'HsWrapper', in which context
-- did a representation polymorphism check arise?
--
-- See 'FRROrigin' for more general origins of representation polymorphism checks.
data WpFunOrigin
  = WpFunSyntaxOp !CtOrigin
  | WpFunViewPat  !(HsExpr GhcRn)
  | WpFunFunTy    !Type
  | WpFunFunExpTy !ExpType

pprWpFunOrigin :: WpFunOrigin -> SDoc
pprWpFunOrigin (WpFunSyntaxOp orig)
  = vcat [ text "When checking a rebindable syntax operator arising from"
         , nest 2 (ppr orig) ]
pprWpFunOrigin (WpFunViewPat expr)
  = vcat [ text "When checking the view pattern function:"
         , nest 2 (ppr expr) ]
pprWpFunOrigin (WpFunFunTy fun_ty)
  = vcat [ text "When inferring the argument type of a function with type"
         , nest 2 (ppr fun_ty) ]
pprWpFunOrigin (WpFunFunExpTy fun_ty)
  = vcat [ text "When inferring the argument type of a function with expected type"
         , nest 2 (ppr fun_ty) ]

instance Outputable WpFunOrigin where
  ppr = pprWpFunOrigin
