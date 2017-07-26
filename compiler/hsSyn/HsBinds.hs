{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module HsBinds where

import {-# SOURCE #-} HsExpr ( pprExpr, LHsExpr,
                               MatchGroup, pprFunBind,
                               GRHSs, pprPatBind )
import {-# SOURCE #-} HsPat  ( LPat )

import HsExtension
import HsTypes
import PprCore ()
import CoreSyn
import TcEvidence
import Type
import NameSet
import BasicTypes
import Outputable
import SrcLoc
import Var
import Bag
import FastString
import BooleanFormula (LBooleanFormula)
import DynFlags

import Data.Data hiding ( Fixity )
import Data.List hiding ( foldr )
import Data.Ord
import Data.Foldable ( Foldable(..) )

import qualified AST

-- -----------------------------------------------------------------------------
-- * Data Declarations
-- -----------------------------------------------------------------------------

-- ** Bindings: @BindGroup@
-- -----------------------------------------------------------------------------

-- Global bindings (where clauses)

-- During renaming, we need bindings where the left-hand sides
-- have been renamed but the the right-hand sides have not.
-- the ...LR datatypes are parametrized by two id types,
-- one for the left and one for the right.
-- Other than during renaming, these will be the same.

-- | Haskell Local Bindings
type HsLocalBinds  pass = AST.LocalBinds  (GHC pass)

-- | Located Haskell local bindings
type LHsLocalBinds pass = AST.LLocalBinds (GHC pass)

-- -----------------------------------------------------------------------------

type
  HsLocalBindsLR pass pass' = AST.LocalBindsLR (GHC pass) (GHC pass')
  -- ^ Haskell Local Bindings with separate Left and Right identifier types
  --
  -- Bindings in a 'let' expression
  -- or a 'where' clause
pattern
  HsValBinds ::
    HsValBindsLR   pass pass' ->
    HsLocalBindsLR pass pass'
  -- ^ Haskell Value Bindings

  -- There should be no pattern synonyms in the HsValBindsLR
  -- These are *local* (not top level) bindings
  -- The parser accepts them, however, leaving the the
  -- renamer to report them

pattern
  HsIPBinds ::
    (HsIPBinds pass') ->
    HsLocalBindsLR pass pass'
  -- ^ Haskell Implicit Parameter Bindings

pattern
  EmptyLocalBinds ::
    HsLocalBindsLR pass pass'
  -- ^ Empty Local Bindings

pattern
  HsValBinds a
    = AST.ValBindsLocal NoFieldExt a
pattern
  HsIPBinds a
    = AST.IPBindsLocal NoFieldExt a
pattern
  EmptyLocalBinds
    = AST.EmptyLocalBinds NoFieldExt

{-#
  COMPLETE
    HsValBinds,
    HsIPBinds,
    EmptyLocalBinds
  #-}

type instance
  AST.XValBindsLocal   (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XIPBindsLocal    (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XEmptyLocalBinds (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XNewLocalBindsLR (GHC pass) (GHC pass') = NoConExt

type
  LHsLocalBindsLR pass pass' = AST.LLocalBindsLR (GHC pass) (GHC pass')

-- -----------------------------------------------------------------------------

-- | Haskell Value Bindings
type HsValBinds pass = AST.ValBinds (GHC pass)

-- -----------------------------------------------------------------------------

type
  HsValBindsLR pass pass' = AST.ValBindsLR (GHC pass) (GHC pass')
  -- ^ Haskell Value bindings with separate Left and Right identifier types
  -- (not implicit parameters)
  -- Used for both top level and nested bindings
  -- May contain pattern synonym bindings
pattern
  ValBindsIn ::
    (LHsBindsLR pass pass') ->
    [LSig pass'] ->
    HsValBindsLR pass pass'
  -- ^ Value Bindings In
  --
  -- Before renaming RHS; pass' is always RdrName
  -- Not dependency analysed
  -- Recursive by default
pattern
  ValBindsOut ::
    [(RecFlag, LHsBinds pass)] ->
    [LSig GhcRn] ->
    HsValBindsLR pass pass'
  -- ^ Value Bindings Out
  --
  -- After renaming RHS; pass' can be Name or Id Dependency analysed,
  -- later bindings in the list may depend on earlier ones.
pattern
  ValBindsIn a b
    = AST.ValBinds   NoFieldExt a b
pattern
  ValBindsOut a b
    = AST.NewValBindsLR (NValBindsOut a b)

{-#
  COMPLETE
    ValBindsIn,
    ValBindsOut
  #-}

type instance
  AST.XValBinds      (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XNewValBindsLR (GHC pass) (GHC pass') = NewHsValBindsLR pass pass'

data NewHsValBindsLR pass pass'
  = NValBindsOut
      [(RecFlag, LHsBinds pass)]
      [LSig GhcRn]

type
  LHsValBindsLR pass pass' = AST.LValBindsLR (GHC pass) (GHC pass')

-- -----------------------------------------------------------------------------

-- | Haskell Binding
type HsBind   pass = AST.Bind (GHC pass)

-- | Located Haskell Binding
type LHsBind  pass = AST.LBind (GHC pass)

-- -----------------------------------------------------------------------------

-- | Located Haskell Bindings
type LHsBinds pass = AST.LBinds (GHC pass)

-- -----------------------------------------------------------------------------

-- | Located Haskell Bindings with separate Left and Right identifier types
type LHsBindsLR pass pass' = AST.LBindsLR (GHC pass) (GHC pass')

-- -----------------------------------------------------------------------------

type
  HsBindLR pass pass' = AST.BindLR (GHC pass) (GHC pass')
  -- ^ Haskell Binding with separate Left and Right id's
pattern
  FunBind ::
    (LIdP pass) ->               -- Note [fun_id in Match] in HsExpr
    (MatchGroup pass' (LHsExpr pass')) -> -- ^ The payload
    (HsWrapper) ->  -- ^ Coercion from the type of the MatchGroup to the type of
                    -- the Id.  Example:
                    --
                    -- @
                    --      f :: Int -> forall a. a -> a
                    --      f x y = y
                    -- @
                    --
                    -- Then the MatchGroup will have type (Int -> a' -> a')
                    -- (with a free type variable a').  The coercion will take
                    -- a CoreExpr of this type and convert it to a CoreExpr of
                    -- type         Int -> forall a'. a' -> a'
                    -- Notice that the coercion captures the free a'.
    (PostRn pass NameSet) ->
                    -- ^ After the renamer, this contains
                    --  the locally-bound
                    -- free variables of this defn.
                    -- See Note [Bind free vars]
    ([Tickish Id]) -> -- ^ Ticks to put on the rhs, if any
    HsBindLR pass pass'
  -- ^ Function-like Binding
  --
  -- FunBind is used for both functions     @f x = e@
  -- and variables                          @f = \x -> e@
  -- and strict variables                   @!x = x + 1@
  --
  -- Reason 1: Special case for type inference: see 'TcBinds.tcMonoBinds'.
  --
  -- Reason 2: Instance decls can only have FunBinds, which is convenient.
  --           If you change this, you'll need to change e.g. rnMethodBinds
  --
  -- But note that the form                 @f :: a->a = ...@
  -- parses as a pattern binding, just like
  --                                        @(f :: a -> a) = ... @
  --
  -- Strict bindings have their strictness recorded in the 'SrcStrictness' of their
  -- 'MatchContext'. See Note [Varieties of binding pattern matches] for
  -- details about the relationship between FunBind and PatBind.
  --
  --  'ApiAnnotation.AnnKeywordId's
  --
  --  - 'ApiAnnotation.AnnFunId', attached to each element of fun_matches
  --
  --  - 'ApiAnnotation.AnnEqual','ApiAnnotation.AnnWhere',
  --    'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  PatBind ::
    (LPat pass) ->
    (GRHSs pass' (LHsExpr pass')) ->
    (PostTc pass' Type) ->
    -- ^ Type of the GRHSs
    (PostRn pass NameSet) ->
    -- ^ See Note [Bind free vars]
    (([Tickish Id], [[Tickish Id]])) ->
    -- ^ Ticks to put on the rhs, if any, and ticks to put on
    -- the bound variables.
    HsBindLR pass pass'
  -- ^ Pattern Binding
  --
  -- The pattern is never a simple variable;
  -- That case is done by FunBind.
  -- See Note [Varieties of binding pattern matches] for details about the
  -- relationship between FunBind and PatBind.

  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang',
  --       'ApiAnnotation.AnnEqual','ApiAnnotation.AnnWhere',
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  VarBind ::
    (IdP pass) ->
    (LHsExpr pass') ->
    -- ^ Located only for consistency
    Bool ->
    -- ^ True <=> inline this binding regardless
    -- (used for implication constraints only)
    HsBindLR pass pass'
  -- ^ Variable Binding
  --
  -- Dictionary binding and suchlike.
  -- All VarBinds are introduced by the type checker

pattern
  AbsBinds :: -- Binds abstraction; TRANSLATION
    ([TyVar]) ->
    ([EvVar]) -> -- ^ Includes equality constraints
    -- ^ AbsBinds only gets used when pass = pass' after renaming,
    -- but these need to be pass 's for the collect... code in HsUtil
    -- to have the right type
    ([ABExport pass]) ->
    ([TcEvBinds]) ->
    -- ^ Evidence bindings
    -- Why a list? See TcInstDcls
    -- Note [Typechecking plan for instance declarations]
    (LHsBinds pass) ->
    -- ^ Typechecked user bindings
    HsBindLR pass pass'
  -- ^ Abstraction Bindings
pattern
  AbsBindsSig ::
    -- Simpler form of AbsBinds, used with a type sig
    -- in tcPolyCheck. Produces simpler desugaring and
    -- is necessary to avoid #11405, comment:3.
    ([TyVar]) ->
    ([EvVar]) ->
    (IdP pass) ->     -- like abe_poly
    (TcSpecPrags) ->
    (TcEvBinds) ->    -- no list needed here
    (LHsBind pass) -> -- always only one, and it's always a
                      -- FunBind
    HsBindLR pass pass'
  -- | ^ Abstraction Bindings Signature

pattern
  PatSynBind ::
    (PatSynBind pass pass') ->
    HsBindLR pass pass'
  -- ^ Patterns Synonym Binding
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnPattern',
  --          'ApiAnnotation.AnnLarrow','ApiAnnotation.AnnEqual',
  --          'ApiAnnotation.AnnWhere'
  --          'ApiAnnotation.AnnOpen' @'{'@,'ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation


  -- Consider (AbsBinds tvs ds [(ftvs, poly_f, mono_f) binds]
  --
  -- Creates bindings for (polymorphic, overloaded) poly_f
  -- in terms of monomorphic, non-overloaded mono_f
  --
  -- Invariants:
  --      1. 'binds' binds mono_f
  --      2. ftvs is a subset of tvs
  --      3. ftvs includes all tyvars free in ds
  --
  -- See Note [AbsBinds]

-- bind_fvs renamed to bind_fvsf
pattern
  FunBind { fun_id, fun_matches, fun_co_fn, bind_fvsf, fun_tick }
    = AST.FunBind (fun_co_fn, bind_fvsf, fun_tick) fun_id fun_matches
pattern
  PatBind { pat_lhs, pat_rhs, pat_rhs_ty, bind_fvs, pat_ticks }
    = AST.PatBind (pat_rhs_ty, bind_fvs, pat_ticks) pat_lhs pat_rhs
pattern
  VarBind { var_id, var_rhs, var_inline }
    = AST.VarBind NoFieldExt var_id var_rhs var_inline
pattern
  AbsBinds { abs_tvsa, abs_ev_varsa, abs_exports, abs_ev_binds,
             abs_binds } -- abs_tvs --> abs_tvsa
    = AST.NewBindLR
        (NAbsBinds abs_tvsa abs_ev_varsa abs_exports abs_ev_binds abs_binds)
pattern
  AbsBindsSig { abs_tvs, abs_ev_vars, abs_sig_export, abs_sig_prags,
                abs_sig_ev_bind, abs_sig_bind }
    = AST.NewBindLR
        (NAbsBindsSig abs_tvs abs_ev_vars abs_sig_export abs_sig_prags
                abs_sig_ev_bind abs_sig_bind)
pattern
  PatSynBind a
    = AST.PatSynBind NoFieldExt a

get_bind_fvs :: HsBindLR pass pass' -> PostRn pass NameSet
get_bind_fvs b@FunBind{} = bind_fvsf b
get_bind_fvs b@PatBind{} = bind_fvs  b
get_bind_fvs _           = error "field selector applied to a wrong constructor"

{-#
  COMPLETE
    FunBind,
    PatBind,
    VarBind,
    AbsBinds,
    AbsBindsSig,
    PatSynBind
  #-}

type instance
  AST.XFunBind    (GHC pass) (GHC pass') = ( HsWrapper
                                           , PostRn pass NameSet
                                           , [Tickish Id]
                                           )
type instance
  AST.XPatBind    (GHC pass) (GHC pass') = ( PostTc pass' Type
                                           , PostRn pass NameSet
                                           , ([Tickish Id], [[Tickish Id]]))
type instance
  AST.XVarBind    (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XPatSynBind (GHC pass) (GHC pass') = NoFieldExt
type instance
  AST.XNewBindLR  (GHC pass) (GHC pass') = NewHsBindLR pass pass'

data NewHsBindLR pass pass'
  = NAbsBinds
      [TyVar]
      [EvVar]
      [ABExport pass]
      [TcEvBinds]
      (LHsBinds pass)

  | NAbsBindsSig
      [TyVar]
      [EvVar]
      (IdP pass)
      TcSpecPrags
      TcEvBinds
      (LHsBind pass)

-- | Located Haskell Binding with separate Left and Right identifier types
type
  LHsBindLR pass pass' = AST.LBindLR (GHC pass) (GHC pass')


-- -----------------------------------------------------------------------------

-- | Abtraction Bindings Export
data ABExport p
  = ABE { abe_poly      :: IdP p -- ^ Any INLINE pragmas is attached to this Id
        , abe_mono      :: IdP p
        , abe_wrap      :: HsWrapper    -- ^ See Note [ABExport wrapper]
             -- Shape: (forall abs_tvs. abs_ev_vars => abe_mono) ~ abe_poly
        , abe_prags     :: TcSpecPrags  -- ^ SPECIALISE pragmas
  }

-- -----------------------------------------------------------------------------

type
  PatSynBind pass pass' = AST.PatSynBind (GHC pass) (GHC pass')
  -- ^ Pattern Synonym binding
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnPattern',
  --             'ApiAnnotation.AnnEqual','ApiAnnotation.AnnLarrow'
  --             'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen' @'{'@,
  --             'ApiAnnotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  PSB ::
    (Located (IdP pass)) ->  -- ^ Name of the pattern synonym
    (PostRn pass' NameSet) -> -- ^ See Note [Bind free vars]
    (HsPatSynDetails (Located (IdP pass'))) ->
    -- ^ Formal parameter names
    (LPat pass') ->
    -- ^ Right-hand side
    (HsPatSynDir pass') ->
    -- ^ Directionality
    PatSynBind pass pass'

pattern
  PSB { psb_id, psb_fvs, psb_args, psb_def, psb_dir }
    = AST.PSB psb_fvs psb_id psb_args psb_def psb_dir

{-#
  COMPLETE
    PSB
  #-}

type instance
  AST.XPSB           (GHC pass) (GHC pass') = PostRn pass' NameSet
type instance
  AST.XNewPatSynBind (GHC pass) (GHC pass') = NoConExt

type
  LPatSynBind pass pass' = AST.LPatSynBind (GHC pass) (GHC pass')

-- -----------------------------------------------------------------------------

type
  HsIPBinds pass = AST.IPBinds (GHC pass)
  -- ^ Haskell Implicit Parameter Bindings
pattern
  IPBinds ::
    [LIPBind pass] ->
    TcEvBinds ->  -- Only in typechecker output; binds
                  -- uses of the implicit parameters
    HsIPBinds pass

pattern
  IPBinds a b
    = AST.IPBinds b a

{-#
  COMPLETE
    IPBinds
  #-}

type instance
  AST.XIPBinds    (GHC pass) = TcEvBinds
type instance
  AST.XNewIPBinds (GHC pass) = NoConExt

type
  LHsIPBinds pass = AST.LIPBinds (GHC pass)

-- -----------------------------------------------------------------------------

type
  IPBind pass = AST.IPBind (GHC pass)
  -- ^ Implicit parameter bindings.
  --
  -- These bindings start off as (Left "x") in the parser and stay
  -- that way until after type-checking when they are replaced with
  -- (Right d), where "d" is the name of the dictionary holding the
  -- evidence for the implicit parameter.
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  IPBind ::
    (Either (Located HsIPName) (IdP id)) ->
    (LHsExpr id) ->
    IPBind id

pattern
  IPBind a b
    = AST.IPBind a b

{-#
  COMPLETE
    IPBind
  #-}

-- | Located Implicit Parameter Binding
type
  LIPBind pass = AST.LIPBind (GHC pass)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when in a
  --   list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- ** @Sig@: type signatures and value-modifying user pragmas
-- -----------------------------------------------------------------------------
{-
It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.
-}
type
  Sig pass = AST.Sig (GHC pass)
  -- ^ Signatures and pragmas
pattern
  TypeSig ::
    [Located (IdP pass)] -> -- LHS of the signature; e.g.  f,g,h :: blah
    (LHsSigWcType pass) ->  -- RHS of the signature; can have wildcards
    Sig pass
  -- ^ An ordinary type signature
  --
  -- > f :: Num a => a -> a
  --
  -- After renaming, this list of Names contains the named and unnamed
  -- wildcards brought into scope by this signature. For a signature
  -- @_ -> _a -> Bool@, the renamer will give the unnamed wildcard @_@
  -- a freshly generated name, e.g. @_w@. @_w@ and the named wildcard @_a@
  -- are then both replaced with fresh meta vars in the type. Their names
  -- are stored in the type signature that brought them into scope, in
  -- this third field to be more specific.
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon',
  --          'ApiAnnotation.AnnComma'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  PatSynSig ::
    [Located (IdP pass)] ->
    (LHsSigType pass) ->
    Sig pass
  -- P :: forall a b. Req => Prov => ty
  -- ^ A pattern synonym type signature
  --
  -- > pattern Single :: () => (Show a) => a -> [a]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnPattern',
  --           'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnForall'
  --           'ApiAnnotation.AnnDot','ApiAnnotation.AnnDarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  ClassOpSig ::
    Bool ->
    [Located (IdP pass)] ->
    (LHsSigType pass) ->
    Sig pass
  -- ^ A signature for a class method
  --   False: ordinary class-method signature
  --   True:  generic-default class method signature
  -- e.g.   class C a where
  --          op :: a -> a                   -- Ordinary
  --          default op :: Eq a => a -> a   -- Generic default
  -- No wildcards allowed here
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDefault',
  --           'ApiAnnotation.AnnDcolon'
pattern
  IdSig ::
    Id ->
    Sig pass
  -- ^ A type signature in generated code, notably the code
  -- generated for record selectors.  We simply record
  -- the desired Id itself, replete with its name, type
  -- and IdDetails.  Otherwise it's just like a type
  -- signature: there should be an accompanying binding
pattern
  FixSig ::
    (FixitySig pass) ->
    Sig pass
  -- ^ An ordinary fixity declaration
  --
  -- >     infixl 8 ***
  --
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnInfix',
  --           'ApiAnnotation.AnnVal'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  InlineSig ::
    (Located (IdP pass)) -> -- Function name
    InlinePragma ->         -- Never defaultInlinePragma
    Sig pass
  -- ^ An inline pragma
  --
  -- > {#- INLINE f #- }
  --
  --  - 'ApiAnnotation.AnnKeywordId' :
  --       'ApiAnnotation.AnnOpen' @'{ -\# INLINE'@ and @'['@,
  --       'ApiAnnotation.AnnClose','ApiAnnotation.AnnOpen',
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnTilde',
  --       'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  SpecSig ::
    (Located (IdP pass)) ->  -- Specialise a function or datatype  ...
    [LHsSigType pass] ->     -- ... to these types
    InlinePragma ->          -- The pragma on SPECIALISE_INLINE form.
                             -- If it's just defaultInlinePragma, then we said
                             --    SPECIALISE, not SPECIALISE_INLINE
    Sig pass
  -- ^ A specialisation pragma
  --
  -- > {-# SPECIALISE f :: Int -> Int #-}
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --      'ApiAnnotation.AnnOpen' @'{-\# SPECIALISE'@ and @'['@,
  --      'ApiAnnotation.AnnTilde',
  --      'ApiAnnotation.AnnVal',
  --      'ApiAnnotation.AnnClose' @']'@ and @'\#-}'@,
  --      'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  SpecInstSig :: -- Note [Pragma source text] in BasicTypes
    SourceText ->
    (LHsSigType pass) ->
    Sig pass
  -- ^ A specialisation pragma for instance declarations only
  --
  -- > {-# SPECIALISE instance Eq [Int] #-}
  --
  -- (Class tys); should be a specialisation of the
  -- current instance declaration
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --      'ApiAnnotation.AnnInstance','ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  MinimalSig :: -- Note [Pragma source text] in BasicTypes
    SourceText ->
    (LBooleanFormula (Located (IdP pass))) ->
    Sig pass
  -- ^ A minimal complete definition pragma
  --
  -- > {-# MINIMAL a | (b, c | (d | e)) #-}
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --      'ApiAnnotation.AnnVbar','ApiAnnotation.AnnComma',
  --      'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
pattern
  SCCFunSig ::
    SourceText -> -- Note [Pragma source text] in BasicTypes
    (Located (IdP pass)) ->  -- Function name
    (Maybe (Located StringLiteral)) ->
    Sig pass
  -- ^ A "set cost centre" pragma for declarations
  --
  -- > {-# SCC funName #-}
  --
  -- or
  --
  -- > {-# SCC funName "cost_centre_name" #-}
pattern
  CompleteMatchSig ::
    SourceText ->
    (Located [Located (IdP pass)]) ->
    (Maybe (Located (IdP pass))) ->
    Sig pass
  -- ^ A complete match pragma
  --
  -- > {-# COMPLETE C, D [:: T] #-}
  --
  -- Used to inform the pattern match checker about additional
  -- complete matchings which, for example, arise from pattern
  -- synonym definitions.
pattern
  TypeSig a b
    = AST.TypeSig NoFieldExt a b
pattern
  PatSynSig a b
    = AST.PatSynSig NoFieldExt a b
pattern
  ClassOpSig a b c
    = AST.ClassOpSig NoFieldExt a b c
pattern
  IdSig a
    = AST.NewSig (NIdSig a)
pattern
  FixSig a
    = AST.FixSig NoFieldExt a
pattern
  InlineSig a b
    = AST.InlineSig NoFieldExt a b
pattern
  SpecSig a b c
    = AST.SpecSig NoFieldExt a b c
pattern
  SpecInstSig a b
    = AST.SpecInstSig a b
pattern
  MinimalSig a b
    = AST.MinimalSig a b
pattern
  SCCFunSig a b c
    = AST.SCCFunSig a b c
pattern
  CompleteMatchSig a b c
    = AST.CompleteMatchSig a b c

{-#
  COMPLETE
    TypeSig,
    PatSynSig,
    ClassOpSig,
    IdSig,
    FixSig,
    InlineSig,
    SpecSig,
    SpecInstSig,
    MinimalSig,
    SCCFunSig,
    CompleteMatchSig
  #-}

type instance
  AST.XTypeSig          (GHC pass) = NoFieldExt
type instance
  AST.XPatSynSig        (GHC pass) = NoFieldExt
type instance
  AST.XClassOpSig       (GHC pass) = NoFieldExt
type instance
  AST.XFixSig           (GHC pass) = NoFieldExt
type instance
  AST.XInlineSig        (GHC pass) = NoFieldExt
type instance
  AST.XSpecSig          (GHC pass) = NoFieldExt
type instance
  AST.XSpecInstSig      (GHC pass) = SourceText
type instance
  AST.XMinimalSig       (GHC pass) = SourceText
type instance
  AST.XSCCFunSig        (GHC pass) = SourceText
type instance
  AST.XCompleteMatchSig (GHC pass) = SourceText
type instance
  AST.XNewSig           (GHC pass) = NewSig pass

data NewSig pass
  = NIdSig Id

-- | Located Signature
type
  LSig pass = AST.LSig (GHC pass)

-- -----------------------------------------------------------------------------

type
  FixitySig pass = AST.FixitySig (GHC pass)
  -- ^ Fixity Signature
pattern
  FixitySig ::
    [Located (IdP pass)] ->
    Fixity ->
    FixitySig pass

pattern
  FixitySig a b
    = AST.FixitySig a b

{-#
  COMPLETE
    FixitySig
  #-}

-- | Located Fixity Signature
type
  LFixitySig pass = AST.LFixitySig (GHC pass)

-- -----------------------------------------------------------------------------

-- | Type checker Specialisation Pragmas
--
-- 'TcSpecPrags' conveys @SPECIALISE@ pragmas from the type checker to the desugarer
data TcSpecPrags
  = IsDefaultMethod     -- ^ Super-specialised: a default method should
                        -- be macro-expanded at every call site
  | SpecPrags [LTcSpecPrag]

-- | Located Type checker Specification Pragmas
type LTcSpecPrag = Located TcSpecPrag

-- | Type checker Specification Pragma
data TcSpecPrag
  = SpecPrag
        Id
        HsWrapper
        InlinePragma
  -- ^ The Id to be specialised, an wrapper that specialises the
  -- polymorphic function, and inlining spec for the specialised function

-- ** A pattern synonym definition
-- -----------------------------------------------------------------------------

type
  HsPatSynDetails arg = AST.PatSynDetails arg -- arg is not an extension var
  -- ^ Haskell Pattern Synonym Details
pattern
  InfixPatSyn ::
    a ->
    a ->
    HsPatSynDetails a
  -- ^ Infix Pattern Synonym
pattern
  PrefixPatSyn ::
    [a] ->
    HsPatSynDetails a
  -- ^ Prefix Pattern Synonym
pattern
  RecordPatSyn ::
    [RecordPatSynField a] ->
    HsPatSynDetails a
  -- ^ Record Pattern Synonym
pattern
  InfixPatSyn a b
    = AST.InfixPatSyn a b
pattern
  PrefixPatSyn a
    = AST.PrefixPatSyn a
pattern
  RecordPatSyn a
    = AST.RecordPatSyn a

{-#
  COMPLETE
    InfixPatSyn,
    PrefixPatSyn,
    RecordPatSyn
  #-}

type
  LHsPatSynDetails a = AST.LPatSynDetails a

-- -----------------------------------------------------------------------------

type
  RecordPatSynField arg = AST.RecordPatSynField arg
  -- See Note [Record PatSyn Fields]
  -- ^ Record Pattern Synonym Field
pattern
  RecordPatSynField ::
    arg -> -- Selector name visible in rest of the file
    arg -> -- Filled in by renamer, the name used internally by the pattern
    RecordPatSynField arg

pattern
  RecordPatSynField { recordPatSynSelectorId, recordPatSynPatVar }
    = AST.RecordPatSynField recordPatSynSelectorId recordPatSynPatVar

{-#
  COMPLETE
    RecordPatSynField
  #-}

type
  LRecordPatSynField arg = AST.LRecordPatSynField arg

-- -----------------------------------------------------------------------------

type
  HsPatSynDir pass = AST.PatSynDir (GHC pass)
  -- ^ Haskell Pattern Synonym Direction
pattern
  Unidirectional ::
    HsPatSynDir pass

pattern
  ImplicitBidirectional ::
    HsPatSynDir pass

pattern
  ExplicitBidirectional ::
    (MatchGroup pass (LHsExpr pass)) ->
    HsPatSynDir pass

pattern
  Unidirectional
    = AST.Unidirectional
pattern
  ImplicitBidirectional
    = AST.ImplicitBidirectional
pattern
  ExplicitBidirectional a
    = AST.ExplicitBidirectional a

{-#
  COMPLETE
    Unidirectional,
    ImplicitBidirectional,
    ExplicitBidirectional
  #-}

type
  LHsPatSynDir pass = AST.LPatSynDir (GHC pass)

-- -----------------------------------------------------------------------------
-- * Utilities
-- -----------------------------------------------------------------------------

deriving instance
  (DataId pass) => Data (NewSig pass)
deriving instance
  (DataId pass, DataId pass') => Data (NewHsBindLR     pass pass')
deriving instance
  (DataId pass, DataId pass') => Data (NewHsValBindsLR pass pass')

deriving instance
  (DataId idL, DataId idR) => Data (HsLocalBindsLR idL idR)

deriving instance
  (DataId idL, DataId idR) => Data (HsValBindsLR idL idR)

deriving instance
  (DataId idL, DataId idR) => Data (HsBindLR idL idR)

deriving instance
  (DataId pass) => Data (ABExport pass)

deriving instance
  (DataId idL, DataId idR) => Data (PatSynBind idL idR)

deriving instance
  (DataId id) => Data (HsIPBinds id)

deriving instance
  (DataId name) => Data (IPBind name)

deriving instance
  (DataId pass) => Data (Sig pass)

deriving instance
  (DataId pass) => Data (FixitySig pass)

deriving instance
  Data TcSpecPrags

deriving instance
  Data TcSpecPrag

deriving instance
  Data a => Data (HsPatSynDetails a)

deriving instance
  Data a => Data (RecordPatSynField a)

deriving instance
  (DataId id) => Data (HsPatSynDir id)

-- -----------------------------------------------------------------------------

emptyLocalBinds :: HsLocalBindsLR a b
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

eqEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
eqEmptyLocalBinds EmptyLocalBinds = True
eqEmptyLocalBinds _               = False

isEmptyValBinds :: HsValBindsLR a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR a b
emptyValBindsIn  = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut []      []

emptyLHsBinds :: LHsBindsLR idL idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR idL idR -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1 sigs1) (ValBindsOut ds2 sigs2)
  = ValBindsOut (ds1 ++ ds2) (sigs1 ++ sigs2)
plusHsValBinds _ _
  = panic "HsBinds.plusHsValBinds"

-- -----------------------------------------------------------------------------

isEmptyIPBinds :: HsIPBinds id -> Bool
isEmptyIPBinds (IPBinds is ds) = null is && isEmptyTcEvBinds ds

-- -----------------------------------------------------------------------------

noSpecPrags :: TcSpecPrags
noSpecPrags = SpecPrags []

hasSpecPrags :: TcSpecPrags -> Bool
hasSpecPrags (SpecPrags ps) = not (null ps)
hasSpecPrags IsDefaultMethod = False

isDefaultMethod :: TcSpecPrags -> Bool
isDefaultMethod IsDefaultMethod = True
isDefaultMethod (SpecPrags {})  = False


isFixityLSig :: LSig name -> Bool
isFixityLSig (L _ (FixSig {})) = True
isFixityLSig _                 = False

isTypeLSig :: LSig name -> Bool  -- Type signatures
isTypeLSig (L _(TypeSig {}))    = True
isTypeLSig (L _(ClassOpSig {})) = True
isTypeLSig (L _(IdSig {}))      = True
isTypeLSig _                    = False

isSpecLSig :: LSig name -> Bool
isSpecLSig (L _(SpecSig {})) = True
isSpecLSig _                 = False

isSpecInstLSig :: LSig name -> Bool
isSpecInstLSig (L _ (SpecInstSig {})) = True
isSpecInstLSig _                      = False

isPragLSig :: LSig name -> Bool
-- Identifies pragmas
isPragLSig (L _ (SpecSig {}))   = True
isPragLSig (L _ (InlineSig {})) = True
isPragLSig (L _ (SCCFunSig {})) = True
isPragLSig (L _ (CompleteMatchSig {})) = True
isPragLSig _                    = False

isInlineLSig :: LSig name -> Bool
-- Identifies inline pragmas
isInlineLSig (L _ (InlineSig {})) = True
isInlineLSig _                    = False

isMinimalLSig :: LSig name -> Bool
isMinimalLSig (L _ (MinimalSig {})) = True
isMinimalLSig _                     = False

isSCCFunSig :: LSig name -> Bool
isSCCFunSig (L _ (SCCFunSig {})) = True
isSCCFunSig _                    = False

isCompleteMatchSig :: LSig name -> Bool
isCompleteMatchSig (L _ (CompleteMatchSig {} )) = True
isCompleteMatchSig _                            = False

-- -----------------------------------------------------------------------------

instance Functor AST.RecordPatSynField where
    fmap f (RecordPatSynField { recordPatSynSelectorId = visible
                              , recordPatSynPatVar = hidden })
      = RecordPatSynField { recordPatSynSelectorId = f visible
                          , recordPatSynPatVar = f hidden }

instance Foldable AST.RecordPatSynField  where
    foldMap f (RecordPatSynField { recordPatSynSelectorId = visible
                                 , recordPatSynPatVar = hidden })
      = f visible `mappend` f hidden

instance Traversable AST.RecordPatSynField where
    traverse f (RecordPatSynField { recordPatSynSelectorId =visible
                                  , recordPatSynPatVar = hidden })
      = (\ sel_id pat_var -> RecordPatSynField { recordPatSynSelectorId = sel_id
                                               , recordPatSynPatVar = pat_var })
          <$> f visible <*> f hidden


instance Functor AST.PatSynDetails where
    fmap f (InfixPatSyn left right) = InfixPatSyn (f left) (f right)
    fmap f (PrefixPatSyn args) = PrefixPatSyn (fmap f args)
    fmap f (RecordPatSyn args) = RecordPatSyn (map (fmap f) args)

instance Foldable AST.PatSynDetails where
    foldMap f (InfixPatSyn left right) = f left `mappend` f right
    foldMap f (PrefixPatSyn args) = foldMap f args
    foldMap f (RecordPatSyn args) = foldMap (foldMap f) args

    foldl1 f (InfixPatSyn left right) = left `f` right
    foldl1 f (PrefixPatSyn args) = Data.List.foldl1 f args
    foldl1 f (RecordPatSyn args) =
      Data.List.foldl1 f (map (Data.Foldable.foldl1 f) args)

    foldr1 f (InfixPatSyn left right) = left `f` right
    foldr1 f (PrefixPatSyn args) = Data.List.foldr1 f args
    foldr1 f (RecordPatSyn args) =
      Data.List.foldr1 f (map (Data.Foldable.foldr1 f) args)

    length (InfixPatSyn _ _) = 2
    length (PrefixPatSyn args) = Data.List.length args
    length (RecordPatSyn args) = Data.List.length args

    null (InfixPatSyn _ _) = False
    null (PrefixPatSyn args) = Data.List.null args
    null (RecordPatSyn args) = Data.List.null args

    toList (InfixPatSyn left right) = [left, right]
    toList (PrefixPatSyn args) = args
    toList (RecordPatSyn args) = foldMap toList args

instance Traversable AST.PatSynDetails where
    traverse f (InfixPatSyn left right) = InfixPatSyn <$> f left <*> f right
    traverse f (PrefixPatSyn args) = PrefixPatSyn <$> traverse f args
    traverse f (RecordPatSyn args) = RecordPatSyn <$> traverse (traverse f) args


-- -----------------------------------------------------------------------------
-- * Pretty Printing
-- -----------------------------------------------------------------------------

instance Outputable a => Outputable (AST.RecordPatSynField a) where
    ppr (RecordPatSynField { recordPatSynSelectorId = v }) = ppr v

-- -----------------------------------------------------------------------------

instance (SourceTextX idL, SourceTextX idR,
          OutputableBndrId idL, OutputableBndrId idR)
        => Outputable (HsLocalBindsLR idL idR) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance (SourceTextX idL, SourceTextX idR,
          OutputableBndrId idL, OutputableBndrId idR)
        => Outputable (HsValBindsLR idL idR) where
  ppr (ValBindsIn binds sigs)
   = pprDeclList (pprLHsBindsForUser binds sigs)

  ppr (ValBindsOut sccs sigs)
    = getPprStyle $ \ sty ->
      if debugStyle sty then    -- Print with sccs showing
        vcat (map ppr sigs) $$ vcat (map ppr_scc sccs)
     else
        pprDeclList (pprLHsBindsForUser (unionManyBags (map snd sccs)) sigs)
   where
     ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
     pp_rec Recursive    = text "rec"
     pp_rec NonRecursive = text "nonrec"

pprLHsBinds :: (SourceTextX idL, SourceTextX idR,
                OutputableBndrId idL, OutputableBndrId idR)
            => LHsBindsLR idL idR -> SDoc
pprLHsBinds binds
  | isEmptyLHsBinds binds = empty
  | otherwise = pprDeclList (map ppr (bagToList binds))

pprLHsBindsForUser :: (SourceTextX idL, SourceTextX idR,
                       OutputableBndrId idL, OutputableBndrId idR,
                       SourceTextX id2, OutputableBndrId id2)
                   => LHsBindsLR idL idR -> [LSig id2] -> [SDoc]
--  pprLHsBindsForUser is different to pprLHsBinds because
--  a) No braces: 'let' and 'where' include a list of HsBindGroups
--     and we don't want several groups of bindings each
--     with braces around
--  b) Sort by location before printing
--  c) Include signatures
pprLHsBindsForUser binds sigs
  = map snd (sort_by_loc decls)
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
            [(loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortBy (comparing fst) decls

pprDeclList :: [SDoc] -> SDoc   -- Braces with a space
-- Print a bunch of declarations
-- One could choose  { d1; d2; ... }, using 'sep'
-- or      d1
--         d2
--         ..
--    using vcat
-- At the moment we chose the latter
-- Also we do the 'pprDeeperList' thing.
pprDeclList ds = pprDeeperList vcat ds

-- -----------------------------------------------------------------------------

instance (SourceTextX idL, SourceTextX idR,
          OutputableBndrId idL, OutputableBndrId idR)
         => Outputable (HsBindLR idL idR) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: (SourceTextX idL, SourceTextX idR,
                 OutputableBndrId idL, OutputableBndrId idR)
             => HsBindLR idL idR -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })
  = sep [pprBndr CasePatBind var, nest 2 $ equals <+> pprExpr (unLoc rhs)]
ppr_monobind (FunBind { fun_id = fun,
                        fun_co_fn = wrap,
                        fun_matches = matches,
                        fun_tick = ticks })
  = pprTicks empty (if null ticks then empty
                    else text "-- ticks = " <> ppr ticks)
    $$  ifPprDebug (pprBndr LetBind (unLoc fun))
    $$  pprFunBind  matches
    $$  ifPprDebug (ppr wrap)
ppr_monobind (PatSynBind psb) = ppr psb
ppr_monobind (AbsBinds { abs_tvsa = tyvars, abs_ev_varsa = dictvars
                       , abs_exports = exports, abs_binds = val_binds
                       , abs_ev_binds = ev_binds })
  = sdocWithDynFlags $ \ dflags ->
    if gopt Opt_PrintTypecheckerElaboration dflags then
      -- Show extra information (bug number: #10662)
      hang (text "AbsBinds" <+> brackets (interpp'SP tyvars)
                                    <+> brackets (interpp'SP dictvars))
         2 $ braces $ vcat
      [ text "Exports:" <+>
          brackets (sep (punctuate comma (map ppr exports)))
      , text "Exported types:" <+>
          vcat [pprBndr LetBind (abe_poly ex) | ex <- exports]
      , text "Binds:" <+> pprLHsBinds val_binds
      , text "Evidence:" <+> ppr ev_binds ]
    else
      pprLHsBinds val_binds
ppr_monobind (AbsBindsSig { abs_tvs         = tyvars
                          , abs_ev_vars     = dictvars
                          , abs_sig_export  = poly_id
                          , abs_sig_ev_bind = ev_bind
                          , abs_sig_bind    = bind })
  = sdocWithDynFlags $ \ dflags ->
    if gopt Opt_PrintTypecheckerElaboration dflags then
      hang (text "AbsBindsSig" <+> brackets (interpp'SP tyvars)
                               <+> brackets (interpp'SP dictvars))
         2 $ braces $ vcat
      [ text "Exported type:" <+> pprBndr LetBind poly_id
      , text "Bind:"     <+> ppr bind
      , text "Evidence:" <+> ppr ev_bind ]
    else
      ppr bind

instance (OutputableBndrId p) => Outputable (ABExport p) where
  ppr (ABE { abe_wrap = wrap, abe_poly = gbl, abe_mono = lcl, abe_prags = prags })
    = vcat [ ppr gbl <+> text "<=" <+> ppr lcl
           , nest 2 (pprTcSpecPrags prags)
           , nest 2 (text "wrap:" <+> ppr wrap)]

instance (SourceTextX idR,
          OutputableBndrId idL, OutputableBndrId idR)
          => Outputable (PatSynBind idL idR) where
  ppr (PSB{ psb_id = (L _ psyn), psb_args = details, psb_def = pat,
            psb_dir = dir })
      = ppr_lhs <+> ppr_rhs
    where
      ppr_lhs = text "pattern" <+> ppr_details
      ppr_simple syntax = syntax <+> ppr pat

      ppr_details = case details of
          InfixPatSyn v1 v2 -> hsep [ppr v1, pprInfixOcc psyn, ppr v2]
          PrefixPatSyn vs   -> hsep (pprPrefixOcc psyn : map ppr vs)
          RecordPatSyn vs   ->
            pprPrefixOcc psyn
                      <> braces (sep (punctuate comma (map ppr vs)))

      ppr_rhs = case dir of
          Unidirectional           -> ppr_simple (text "<-")
          ImplicitBidirectional    -> ppr_simple equals
          ExplicitBidirectional mg -> ppr_simple (text "<-") <+> ptext (sLit "where") $$
                                      (nest 2 $ pprFunBind mg)

pprTicks :: SDoc -> SDoc -> SDoc
-- Print stuff about ticks only when -dppr-debug is on, to avoid
-- them appearing in error messages (from the desugarer); see Trac # 3263
-- Also print ticks in dumpStyle, so that -ddump-hpc actually does
-- something useful.
pprTicks pp_no_debug pp_when_debug
  = getPprStyle (\ sty -> if debugStyle sty || dumpStyle sty
                             then pp_when_debug
                             else pp_no_debug)

-- -----------------------------------------------------------------------------

instance (SourceTextX p, OutputableBndrId p) => Outputable (HsIPBinds p) where
  ppr (IPBinds bs ds) = pprDeeperList vcat (map ppr bs)
                        $$ ifPprDebug (ppr ds)

instance (SourceTextX p, OutputableBndrId p ) => Outputable (IPBind p) where
  ppr (IPBind lr rhs) = name <+> equals <+> pprExpr (unLoc rhs)
    where name = case lr of
                   Left (L _ ip) -> pprBndr LetBind ip
                   Right     id  -> pprBndr LetBind id

-- -----------------------------------------------------------------------------

hsSigDoc :: Sig name -> SDoc
hsSigDoc (TypeSig {})           = text "type signature"
hsSigDoc (PatSynSig {})         = text "pattern synonym signature"
hsSigDoc (ClassOpSig is_deflt _ _)
 | is_deflt                     = text "default type signature"
 | otherwise                    = text "class method signature"
hsSigDoc (IdSig {})             = text "id signature"
hsSigDoc (SpecSig {})           = text "SPECIALISE pragma"
hsSigDoc (InlineSig _ prag)     = ppr (inlinePragmaSpec prag) <+> text "pragma"
hsSigDoc (SpecInstSig {})       = text "SPECIALISE instance pragma"
hsSigDoc (FixSig {})            = text "fixity declaration"
hsSigDoc (MinimalSig {})        = text "MINIMAL pragma"
hsSigDoc (SCCFunSig {})         = text "SCC pragma"
hsSigDoc (CompleteMatchSig {})  = text "COMPLETE pragma"

{-
Check if signatures overlap; this is used when checking for duplicate
signatures. Since some of the signatures contain a list of names, testing for
equality is not enough -- we have to check if they overlap.
-}

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (Sig pass) where
    ppr sig = ppr_sig sig

ppr_sig :: (SourceTextX pass, OutputableBndrId pass ) => Sig pass -> SDoc
ppr_sig (TypeSig vars ty)    = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (ClassOpSig is_deflt vars ty)
  | is_deflt                 = text "default" <+> pprVarSig (map unLoc vars) (ppr ty)
  | otherwise                = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (IdSig id)           = pprVarSig [id] (ppr (varType id))
ppr_sig (FixSig fix_sig)     = ppr fix_sig
ppr_sig (SpecSig var ty inl@(InlinePragma { inl_inline = spec }))
  = pragSrcBrackets (inl_src inl) pragmaSrc (pprSpec (unLoc var)
                                             (interpp'SP ty) inl)
    where
      pragmaSrc = case spec of
        EmptyInlineSpec -> "{-# SPECIALISE"
        _               -> "{-# SPECIALISE_INLINE"
ppr_sig (InlineSig var inl)
  = pragSrcBrackets (inl_src inl) "{-# INLINE"  (pprInline inl
                                   <+> pprPrefixOcc (unLoc var))
ppr_sig (SpecInstSig src ty)
  = pragSrcBrackets src "{-# SPECIALISE" (text "instance" <+> ppr ty)
ppr_sig (MinimalSig src bf)
  = pragSrcBrackets src "{-# MINIMAL" (pprMinimalSig bf)
ppr_sig (PatSynSig names sig_ty)
  = text "pattern" <+> pprVarSig (map unLoc names) (ppr sig_ty)
ppr_sig (SCCFunSig src fn mlabel)
  = pragSrcBrackets src "{-# SCC" (ppr fn <+> maybe empty ppr mlabel )
ppr_sig (CompleteMatchSig src cs mty)
  = pragSrcBrackets src "{-# COMPLETE"
      ((hsep (punctuate comma (map ppr (unLoc cs))))
        <+> opt_sig)
  where
    opt_sig = maybe empty ((\t -> dcolon <+> ppr t) . unLoc) mty

instance OutputableBndrId pass => Outputable (FixitySig pass) where
  ppr (FixitySig names fixity) = sep [ppr fixity, pprops]
    where
      pprops = hsep $ punctuate comma (map (pprInfixOcc . unLoc) names)

pragBrackets :: SDoc -> SDoc
pragBrackets doc = text "{-#" <+> doc <+> text "#-}"

-- | Using SourceText in case the pragma was spelled differently or used mixed
-- case
pragSrcBrackets :: SourceText -> String -> SDoc -> SDoc
pragSrcBrackets (SourceText src) _   doc = text src <+> doc <+> text "#-}"
pragSrcBrackets NoSourceText     alt doc = text alt <+> doc <+> text "#-}"

pprVarSig :: (OutputableBndr id) => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where
    pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

pprSpec :: (OutputableBndr id) => id -> SDoc -> InlinePragma -> SDoc
pprSpec var pp_ty inl = pp_inl <+> pprVarSig [var] pp_ty
  where
    pp_inl | isDefaultInlinePragma inl = empty
           | otherwise = pprInline inl

pprTcSpecPrags :: TcSpecPrags -> SDoc
pprTcSpecPrags IsDefaultMethod = text "<default method>"
pprTcSpecPrags (SpecPrags ps)  = vcat (map (ppr . unLoc) ps)

instance Outputable TcSpecPrag where
  ppr (SpecPrag var _ inl)
    = text "SPECIALIZE" <+> pprSpec var (text "<type>") inl

pprMinimalSig :: (OutputableBndr name)
              => LBooleanFormula (Located name) -> SDoc
pprMinimalSig (L _ bf) = ppr (fmap unLoc bf)


-- -----------------------------------------------------------------------------
-- Notes
-- -----------------------------------------------------------------------------
{-
Note [Varieties of binding pattern matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The distinction between FunBind and PatBind is a bit subtle. FunBind covers
patterns which resemble function bindings and simple variable bindings.

    f x = e
    f !x = e
    f = e
    !x = e          -- FunRhs has SrcStrict
    x `f` y = e     -- FunRhs has Infix

The actual patterns and RHSs of a FunBind are encoding in fun_matches.
The m_ctxt field of Match will be FunRhs and carries two bits of information
about the match,

  * the mc_strictness field describes whether the match is decorated with a bang
    (e.g. `!x = e`)
  * the mc_fixity field describes the fixity of the function binder

By contrast, PatBind represents data constructor patterns, as well as a few
other interesting cases. Namely,

    Just x = e
    (x) = e
    x :: Ty = e

Note [AbsBinds]
~~~~~~~~~~~~~~~
The AbsBinds constructor is used in the output of the type checker, to
record *typechecked* and *generalised* bindings.  Specifically

         AbsBinds { abs_tvs      = tvs
                  , abs_ev_vars  = [d1,d2]
                  , abs_exports  = [ABE { abe_poly = fp, abe_mono = fm
                                        , abe_wrap = fwrap }
                                    ABE { slly for g } ]
                  , abs_ev_binds = DBINDS
                  , abs_binds    = BIND[fm,gm] }

where 'BIND' binds the monomorphic Ids 'fm' and 'gm', means

        fp = fwrap [/\ tvs. \d1 d2. letrec { DBINDS        ]
                   [                       ; BIND[fm,gm] } ]
                   [                 in fm                 ]

        gp = ...same again, with gm instead of fm

The 'fwrap' is an impedence-matcher that typically does nothing; see
Note [ABExport wrapper].

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

        fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
                                        (fm,gm) -> fm
        ..ditto for gp..

        tp = /\ [a,b] -> \ [d1,d2] -> letrec { DBINDS; BIND }
                                      in (fm,gm)

In general:

  * abs_tvs are the type variables over which the binding group is
    generalised
  * abs_ev_var are the evidence variables (usually dictionaries)
    over which the binding group is generalised
  * abs_binds are the monomorphic bindings
  * abs_ex_binds are the evidence bindings that wrap the abs_binds
  * abs_exports connects the monomorphic Ids bound by abs_binds
    with the polymorphic Ids bound by the AbsBinds itself.

For example, consider a module M, with this top-level binding, where
there is no type signature for M.reverse,
    M.reverse []     = []
    M.reverse (x:xs) = M.reverse xs ++ [x]

In Hindley-Milner, a recursive binding is typechecked with the
*recursive* uses being *monomorphic*.  So after typechecking *and*
desugaring we will get something like this

    M.reverse :: forall a. [a] -> [a]
      = /\a. letrec
                reverse :: [a] -> [a] = \xs -> case xs of
                                                []     -> []
                                                (x:xs) -> reverse xs ++ [x]
             in reverse

Notice that 'M.reverse' is polymorphic as expected, but there is a local
definition for plain 'reverse' which is *monomorphic*.  The type variable
'a' scopes over the entire letrec.

That's after desugaring.  What about after type checking but before
desugaring?  That's where AbsBinds comes in.  It looks like this:

   AbsBinds { abs_tvs     = [a]
            , abs_ev_vars = []
            , abs_exports = [ABE { abe_poly = M.reverse :: forall a. [a] -> [a],
                                 , abe_mono = reverse :: [a] -> [a]}]
            , abs_ev_binds = {}
            , abs_binds = { reverse :: [a] -> [a]
                               = \xs -> case xs of
                                            []     -> []
                                            (x:xs) -> reverse xs ++ [x] } }

Here,

  * abs_tvs says what type variables are abstracted over the binding
    group, just 'a' in this case.
  * abs_binds is the *monomorphic* bindings of the group
  * abs_exports describes how to get the polymorphic Id 'M.reverse'
    from the monomorphic one 'reverse'

Notice that the *original* function (the polymorphic one you thought
you were defining) appears in the abe_poly field of the
abs_exports. The bindings in abs_binds are for fresh, local, Ids with
a *monomorphic* Id.

If there is a group of mutually recursive (see Note [Polymorphic
recursion]) functions without type signatures, we get one AbsBinds
with the monomorphic versions of the bindings in abs_binds, and one
element of abe_exports for each variable bound in the mutually
recursive group.  This is true even for pattern bindings.  Example:
        (f,g) = (\x -> x, f)
After type checking we get
   AbsBinds { abs_tvs     = [a]
            , abs_exports = [ ABE { abe_poly = M.f :: forall a. a -> a
                                  , abe_mono = f :: a -> a }
                            , ABE { abe_poly = M.g :: forall a. a -> a
                                  , abe_mono = g :: a -> a }]
            , abs_binds = { (f,g) = (\x -> x, f) }

Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   Rec { f x = ...(g ef)...

       ; g :: forall a. [a] -> [a]
       ; g y = ...(f eg)...  }

These bindings /are/ mutually recursive (f calls g, and g calls f).
But we can use the type signature for g to break the recursion,
like this:

  1. Add g :: forall a. [a] -> [a] to the type environment

  2. Typecheck the definition of f, all by itself,
     including generalising it to find its most general
     type, say f :: forall b. b -> b -> [b]

  3. Extend the type environment with that type for f

  4. Typecheck the definition of g, all by itself,
     checking that it has the type claimed by its signature

Steps 2 and 4 each generate a separate AbsBinds, so we end
up with
   Rec { AbsBinds { ...for f ... }
       ; AbsBinds { ...for g ... } }

This approach allows both f and to call each other
polymorphically, even though only g has a signature.

We get an AbsBinds that encompasses multiple source-program
bindings only when
 * Each binding in the group has at least one binder that
   lacks a user type signature
 * The group forms a strongly connected component

Note [ABExport wrapper]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
   (f,g) = (\x.x, \y.y)
This ultimately desugars to something like this:
   tup :: forall a b. (a->a, b->b)
   tup = /\a b. (\x:a.x, \y:b.y)
   f :: forall a. a -> a
   f = /\a. case tup a Any of
               (fm::a->a,gm:Any->Any) -> fm
   ...similarly for g...

The abe_wrap field deals with impedance-matching between
    (/\a b. case tup a b of { (f,g) -> f })
and the thing we really want, which may have fewer type
variables.  The action happens in TcBinds.mkExport.

Note [Bind free vars]
~~~~~~~~~~~~~~~~~~~~~
The bind_fvs field of FunBind and PatBind records the free variables
of the definition.  It is used for the following purposes

a) Dependency analysis prior to type checking
    (see TcBinds.tc_group)

b) Deciding whether we can do generalisation of the binding
    (see TcBinds.decideGeneralisationPlan)

c) Deciding whether the binding can be used in static forms
    (see TcExpr.checkClosedInStaticForm for the HsStatic case and
     TcBinds.isClosedBndrGroup).

Specifically,

  * bind_fvs includes all free vars that are defined in this module
    (including top-level things and lexically scoped type variables)

  * bind_fvs excludes imported vars; this is just to keep the set smaller

  * Before renaming, and after typechecking, the field is unused;
    it's just an error thunk

Note [Record PatSyn Fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following two pattern synonyms.

pattern P x y = ([x,True], [y,'v'])
pattern Q{ x, y } =([x,True], [y,'v'])

In P, we just have two local binders, x and y.

In Q, we have local binders but also top-level record selectors
x :: ([Bool], [Char]) -> Bool and similarly for y.

It would make sense to support record-like syntax

pattern Q{ x=x1, y=y1 } = ([x1,True], [y1,'v'])

when we have a different name for the local and top-level binder
the distinction between the two names clear

-}
