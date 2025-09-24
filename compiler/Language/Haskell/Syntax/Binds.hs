{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns #-}


{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.
-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Binds where

import {-# SOURCE #-} Language.Haskell.Syntax.Expr
  ( LHsExpr
  , MatchGroup
  , GRHSs,
{-
    RuleMatchInfo(..), isConLike, isFunLike,
    InlineSpec(..), noUserInlineSpec,
    InlinePragma(..), defaultInlinePragma, alwaysInlinePragma,
    neverInlinePragma, dfunInlinePragma,
    isDefaultInlinePragma,
    isInlinePragma, isInlinablePragma, isNoInlinePragma, isOpaquePragma,
    isAnyInlinePragma, alwaysInlineConLikePragma,
    inlinePragmaSource,
    inlinePragmaName, inlineSpecSource,
    inlinePragmaSpec, inlinePragmaSat,
    inlinePragmaActivation, inlinePragmaRuleMatchInfo,
    setInlinePragmaActivation, setInlinePragmaRuleMatchInfo
--        pprInline, pprInlineDebug,
-}

  )

import Language.Haskell.Syntax.Basic ( Fixity )
import Language.Haskell.Syntax.BooleanFormula (LBooleanFormula)
import Language.Haskell.Syntax.Extension
import {-# SOURCE #-} Language.Haskell.Syntax.Pat( LPat )
import Language.Haskell.Syntax.Type
import Language.Haskell.Textual.Source qualified as Source
--import Language.Haskell.Textual.UTF8

--import GHC.Types.Basic (InlinePragma)
--import GHC.Types.SourceText (StringLiteral)

import Control.DeepSeq
import Data.Bool
import Data.Data (Data)
import Data.Maybe
--import Data.String (IsString(..))

import Prelude

{-
************************************************************************
*                                                                      *
\subsection{Bindings: @BindGroup@}
*                                                                      *
************************************************************************

Global bindings (where clauses)
-}

-- During renaming, we need bindings where the left-hand sides
-- have been renamed but the right-hand sides have not.
-- Other than during renaming, these will be the same.

-- | Haskell Local Bindings
type HsLocalBinds id = HsLocalBindsLR id id

-- | Located Haskell local bindings
type LHsLocalBinds id = XRec id (HsLocalBinds id)

-- | Haskell Local Bindings with separate Left and Right identifier types
--
-- Bindings in a 'let' expression
-- or a 'where' clause
data HsLocalBindsLR idL idR
  = HsValBinds
        (XHsValBinds idL idR)
        (HsValBindsLR idL idR)
      -- ^ Haskell Value Bindings

         -- There should be no pattern synonyms in the HsValBindsLR
         -- These are *local* (not top level) bindings
         -- The parser accepts them, however, leaving the
         -- renamer to report them

  | HsIPBinds
        (XHsIPBinds idL idR)
        (HsIPBinds idR)
      -- ^ Haskell Implicit Parameter Bindings

  | EmptyLocalBinds (XEmptyLocalBinds idL idR)
      -- ^ Empty Local Bindings

  | XHsLocalBindsLR
        !(XXHsLocalBindsLR idL idR)

type LHsLocalBindsLR idL idR = XRec idL (HsLocalBindsLR idL idR)


-- | Haskell Value Bindings
type HsValBinds id = HsValBindsLR id id

-- | Haskell Value bindings with separate Left and Right identifier types
-- (not implicit parameters)
-- Used for both top level and nested bindings
-- May contain pattern synonym bindings
data HsValBindsLR idL idR
  = -- | Value Bindings In
    --
    -- Before renaming RHS; idR is always RdrName
    -- Not dependency analysed
    -- Recursive by default
    ValBinds
        (XValBinds idL idR)
        (LHsBindsLR idL idR) [LSig idR]

    -- | Value Bindings Out
    --
    -- After renaming RHS; idR can be Name or Id Dependency analysed,
    -- later bindings in the list may depend on earlier ones.
  | XValBindsLR
      !(XXValBindsLR idL idR)

-- ---------------------------------------------------------------------

-- | Located Haskell Binding
type LHsBind  id = LHsBindLR  id id

-- | Located Haskell Bindings
type LHsBinds id = LHsBindsLR id id

-- | Haskell Binding
type HsBind   id = HsBindLR   id id

-- | Located Haskell Bindings with separate Left and Right identifier types
type LHsBindsLR idL idR = [LHsBindLR idL idR]

-- | Located Haskell Binding with separate Left and Right identifier types
type LHsBindLR  idL idR = XRec idL (HsBindLR idL idR)

{- Note [FunBind vs PatBind]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
The distinction between FunBind and PatBind is a bit subtle. FunBind covers
patterns which resemble function bindings and simple variable bindings.

    f x = e
    f !x = e
    f = e
    !x = e          -- FunRhs has SrcStrict
    x `f` y = e     -- FunRhs has Infix

The actual patterns and RHSs of a FunBind are encoding in fun_matches.
The m_ctxt field of each Match in fun_matches will be FunRhs and carries
two bits of information about the match,

  * The mc_fixity field on each Match describes the fixity of the
    function binder in that match.  E.g. this is legal:
         f True False  = e1
         True `f` True = e2

  * The mc_strictness field is used /only/ for nullary FunBinds: ones
    with one Match, which has no pats. For these, it describes whether
    the match is decorated with a bang (e.g. `!x = e`).

By contrast, PatBind represents data constructor patterns, as well as a few
other interesting cases. Namely,

    Just x = e
    (x) = e
    x :: Ty = e

Note [Multiplicity annotations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Multiplicity annotations are stored in the pat_mult field on PatBinds,
represented by the HsMultAnn data type

  HsNoMultAnn <=> no annotation in the source file
  HsPct1Ann   <=> the %1 annotation
  HsMultAnn   <=> the %t annotation, where `t` is some type

In case of HsNoMultAnn the typechecker infers a multiplicity.

We don't need to store a multiplicity on FunBinds:
- let %1 x = … is parsed as a PatBind. So we don't need an annotation before
  typechecking.
- the multiplicity that the typechecker infers is stored in the binder's Var for
  the desugarer to use. It's only relevant for strict FunBinds, see Wrinkle 1 in
  Note [Desugar Strict binds] in GHC.HsToCore.Binds as, in Core, let expressions
  don't have multiplicity annotations.
-}

-- | Haskell Binding with separate Left and Right id's
data HsBindLR idL idR
  = -- | Function-like Binding
    --
    -- FunBind is used for both functions     @f x = e@
    -- and variables                          @f = \x -> e@
    -- and strict variables                   @!x = x + 1@
    --
    -- Reason 1: Special case for type inference: see 'GHC.Tc.Gen.Bind.tcMonoBinds'.
    --
    -- Reason 2: Instance decls can only have FunBinds, which is convenient.
    --           If you change this, you'll need to change e.g. rnMethodBinds
    --
    -- But note that the form                 @f :: a->a = ...@
    -- parses as a pattern binding, just like
    --                                        @(f :: a -> a) = ... @
    --
    -- Strict bindings have their strictness recorded in the 'SrcStrictness' of their
    -- 'MatchContext'. See Note [FunBind vs PatBind] for
    -- details about the relationship between FunBind and PatBind.
    FunBind {

        fun_ext :: XFunBind idL idR,

        fun_id :: LIdP idL, -- Note [fun_id in Match] in GHC.Hs.Expr

        fun_matches :: MatchGroup idR (LHsExpr idR)  -- ^ The payload

    }

  -- | Pattern Binding
  --
  -- The pattern is never a simple variable;
  -- That case is done by FunBind.
  -- See Note [FunBind vs PatBind] for details about the
  -- relationship between FunBind and PatBind.
  | PatBind {
        pat_ext    :: XPatBind idL idR,
        pat_lhs    :: LPat idL,
        pat_mult   :: HsMultAnn idL,
        -- ^ See Note [Multiplicity annotations].
        pat_rhs    :: GRHSs idR (LHsExpr idR)
    }

  -- | Variable Binding
  --
  -- Dictionary binding and suchlike.
  -- All VarBinds are introduced by the type checker
  | VarBind {
        var_ext    :: XVarBind idL idR,
        var_id     :: IdP idL,
        var_rhs    :: LHsExpr idR    -- ^ Located only for consistency
    }

  -- | Patterns Synonym Binding
  | PatSynBind
        (XPatSynBind idL idR)
        (PatSynBind idL idR)

  | XHsBindsLR !(XXHsBindsLR idL idR)


-- | Pattern Synonym binding
data PatSynBind idL idR
  = PSB { psb_ext  :: XPSB idL idR,
          psb_id   :: LIdP idL,                -- ^ Name of the pattern synonym
          psb_args :: HsPatSynDetails idR,     -- ^ Formal parameter names
          psb_def  :: LPat idR,                -- ^ Right-hand side
          psb_dir  :: HsPatSynDir idR          -- ^ Directionality
     }
   | XPatSynBind !(XXPatSynBind idL idR)

{-
************************************************************************
*                                                                      *
                Implicit parameter bindings
*                                                                      *
************************************************************************
-}

-- | Haskell Implicit Parameter Bindings
data HsIPBinds id
  = IPBinds
        (XIPBinds id)
        [LIPBind id]
        -- TcEvBinds       -- Only in typechecker output; binds
        --                 -- uses of the implicit parameters
  | XHsIPBinds !(XXHsIPBinds id)


-- | Located Implicit Parameter Binding
type LIPBind id = XRec id (IPBind id)

-- | Implicit parameter bindings.
data IPBind id
  = IPBind
        (XCIPBind id)
        (XRec id HsIPName)
        (LHsExpr id)
  | XIPBind !(XXIPBind id)

{-
************************************************************************
*                                                                      *
\subsection{@Sig@: type signatures and value-modifying user pragmas}
*                                                                      *
************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.
-}

-- | Located Signature
type LSig pass = XRec pass (Sig pass)

-- | Signatures and pragmas
data Sig pass
  =   -- | An ordinary type signature
      --
      -- > f :: Num a => a -> a
      --
      -- After renaming, this list of Names contains the named
      -- wildcards brought into scope by this signature. For a signature
      -- @_ -> _a -> Bool@, the renamer will leave the unnamed wildcard @_@
      -- untouched, and the named wildcard @_a@ is then replaced with
      -- fresh meta vars in the type. Their names are stored in the type
      -- signature that brought them into scope, in this third field to be
      -- more specific.
    TypeSig
       (XTypeSig pass)
       [LIdP pass]           -- LHS of the signature; e.g.  f,g,h :: blah
       (LHsSigWcType pass)   -- RHS of the signature; can have wildcards

      -- | A pattern synonym type signature
      --
      -- > pattern Single :: () => (Show a) => a -> [a]
  | PatSynSig (XPatSynSig pass) [LIdP pass] (LHsSigType pass)
      -- P :: forall a b. Req => Prov => ty

      -- | A signature for a class method
      --   False: ordinary class-method signature
      --   True:  generic-default class method signature
      -- e.g.   class C a where
      --          op :: a -> a                   -- Ordinary
      --          default op :: Eq a => a -> a   -- Generic default
      -- No wildcards allowed here
  | ClassOpSig (XClassOpSig pass) Bool [LIdP pass] (LHsSigType pass)

        -- | An ordinary fixity declaration
        --
        -- >     infixl 8 ***
  | FixSig (XFixSig pass) (FixitySig pass)

        -- | An inline pragma
        --
        -- > {#- INLINE f #-}
  | InlineSig   (XInlineSig pass)
                (LIdP pass)        -- Function name
                InlinePragma       -- Never defaultInlinePragma

        -- | An old-form specialisation pragma
        --
        -- > {-# SPECIALISE f :: Int -> Int #-}
        --
        -- NB: this constructor is deprecated and will be removed in GHC 9.18 (#25540)
  | SpecSig     (XSpecSig pass)
                (LIdP pass)        -- Specialise a function or datatype  ...
                [LHsSigType pass]  -- ... to these types
                InlinePragma       -- The pragma on SPECIALISE_INLINE form.
                                   -- If it's just defaultInlinePragma, then we said
                                   --    SPECIALISE, not SPECIALISE_INLINE

        -- | A new-form specialisation pragma (see GHC Proposal #493)
        --   e.g.  {-# SPECIALISE f @Int 1 :: Int -> Int #-}
        --   See Note [Overview of SPECIALISE pragmas]
  | SpecSigE    (XSpecSigE pass)
                (RuleBndrs pass)
                (LHsExpr pass)     -- Expression to specialise
                InlinePragma
                -- The expression should be of form
                --     f a1 ... an [ :: sig ]
                -- with an optional type signature

        -- | A specialisation pragma for instance declarations only
        --
        -- > {-# SPECIALISE instance Eq [Int] #-}
        --
        -- (Class tys); should be a specialisation of the
        -- current instance declaration
  | SpecInstSig (XSpecInstSig pass) (LHsSigType pass)

        -- | A minimal complete definition pragma
        --
        -- > {-# MINIMAL a | (b, c | (d | e)) #-}
  | MinimalSig (XMinimalSig pass) (LBooleanFormula pass)

        -- | A "set cost centre" pragma for declarations
        --
        -- > {-# SCC funName #-}
        --
        -- or
        --
        -- > {-# SCC funName "cost_centre_name" #-}

  | SCCFunSig  (XSCCFunSig pass)
               (LIdP pass)    -- Function name
               (Maybe (XRec pass Source.StringLiteral))
       -- | A complete match pragma
       --
       -- > {-# COMPLETE C, D [:: T] #-}
       --
       -- Used to inform the pattern match checker about additional
       -- complete matchings which, for example, arise from pattern
       -- synonym definitions.
  | CompleteMatchSig (XCompleteMatchSig pass)
                     [LIdP pass]
                     (Maybe (LIdP pass))
  | XSig !(XXSig pass)

-- | Located Fixity Signature
type LFixitySig pass = XRec pass (FixitySig pass)

-- | Fixity Signature
data FixitySig pass = FixitySig (XFixitySig pass) [LIdP pass] Fixity
                    | XFixitySig !(XXFixitySig pass)

isFixityLSig :: forall p. UnXRec p => LSig p -> Bool
isFixityLSig (unXRec @p -> FixSig {}) = True
isFixityLSig _                 = False

isTypeLSig :: forall p. UnXRec p => LSig p -> Bool  -- Type signatures
isTypeLSig (unXRec @p -> TypeSig {})    = True
isTypeLSig (unXRec @p -> ClassOpSig {}) = True
isTypeLSig (unXRec @p -> XSig {})       = True
isTypeLSig _                    = False

isSpecLSig :: forall p. UnXRec p => LSig p -> Bool
isSpecLSig (unXRec @p -> SpecSig {})  = True
isSpecLSig (unXRec @p -> SpecSigE {}) = True
isSpecLSig _                          = False

isSpecInstLSig :: forall p. UnXRec p => LSig p -> Bool
isSpecInstLSig (unXRec @p -> SpecInstSig {}) = True
isSpecInstLSig _                      = False

isPragLSig :: forall p. UnXRec p => LSig p -> Bool
-- Identifies pragmas
isPragLSig (unXRec @p -> SpecSig {})   = True
isPragLSig (unXRec @p -> SpecSigE {})  = True
isPragLSig (unXRec @p -> InlineSig {}) = True
isPragLSig (unXRec @p -> SCCFunSig {}) = True
isPragLSig (unXRec @p -> CompleteMatchSig {}) = True
isPragLSig _                    = False

isInlineLSig :: forall p. UnXRec p => LSig p -> Bool
-- Identifies inline pragmas
isInlineLSig (unXRec @p -> InlineSig {}) = True
isInlineLSig _                    = False

isMinimalLSig :: forall p. UnXRec p => LSig p -> Bool
isMinimalLSig (unXRec @p -> MinimalSig {}) = True
isMinimalLSig _                               = False

isSCCFunSig :: forall p. UnXRec p => LSig p -> Bool
isSCCFunSig (unXRec @p -> SCCFunSig {}) = True
isSCCFunSig _                    = False

isCompleteMatchSig :: forall p. UnXRec p => LSig p -> Bool
isCompleteMatchSig (unXRec @p -> CompleteMatchSig {} ) = True
isCompleteMatchSig _                            = False

{- *********************************************************************
*                                                                      *
                   Rule binders
*                                                                      *
********************************************************************* -}

data RuleBndrs pass = RuleBndrs
       { rb_ext  :: XCRuleBndrs pass
           --   After typechecking rb_ext contains /all/ the quantified variables
           --   both term variables and type varibles
       , rb_tyvs :: Maybe [LHsTyVarBndr () (NoGhcTc pass)]
           -- ^ User-written forall'd type vars; preserved for pretty-printing
       , rb_tmvs :: [LRuleBndr (NoGhcTc pass)]
           -- ^ User-written forall'd term vars; preserved for pretty-printing
       }
  | XRuleBndrs !(XXRuleBndrs pass)

-- | Located Rule Binder
type LRuleBndr pass = XRec pass (RuleBndr pass)

-- | Rule Binder
data RuleBndr pass
  = RuleBndr    (XCRuleBndr pass)   (LIdP pass)
  | RuleBndrSig (XRuleBndrSig pass) (LIdP pass) (HsPatSigType pass)
  | XRuleBndr !(XXRuleBndr pass)
        -- ^
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --     'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnClose'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

collectRuleBndrSigTys :: [RuleBndr pass] -> [HsPatSigType pass]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ _ ty <- bndrs]

{-
************************************************************************
*                                                                      *
\subsection[PatSynBind]{A pattern synonym definition}
*                                                                      *
************************************************************************
-}

-- | Haskell Pattern Synonym Details
type HsPatSynDetails pass = HsConDetails (LIdP pass) [RecordPatSynField pass]

-- See Note [Record PatSyn Fields]
-- | Record Pattern Synonym Field
data RecordPatSynField pass
  = RecordPatSynField
      { recordPatSynField :: FieldOcc pass
      -- ^ Field label visible in rest of the file
      , recordPatSynPatVar :: LIdP pass
      -- ^ Filled in by renamer, the name used internally by the pattern
      }


{-
Note [Record PatSyn Fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the following two pattern synonyms.

  pattern P x y = ([x,True], [y,'v'])
  pattern Q{ x, y } =([x,True], [y,'v'])

In P, we just have two local binders, x and y.

In Q, we have local binders but also top-level record selectors
  x :: ([Bool], [Char]) -> Bool
  y :: ([Bool], [Char]) -> Char

Both are recorded in the `RecordPatSynField`s for `x` and `y`:
* recordPatSynField: the top-level record selector
* recordPatSynPatVar: the local `x`, bound only in the RHS of the pattern synonym.

It would make sense to support record-like syntax

  pattern Q{ x=x1, y=y1 } = ([x1,True], [y1,'v'])

when we have a different name for the local and top-level binder,
making the distinction between the two names clear.

-}

-- | Haskell Pattern Synonym Direction
data HsPatSynDir id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup id (LHsExpr id))


{-
************************************************************************
*                                                                      *
\subsection[Arity]{Arity}
*                                                                      *
************************************************************************
-}

-- | The number of value arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100     has arity 0
--  \x -> fib x has arity 1
-- See also Note [Definition of arity] in "GHC.Core.Opt.Arity"
type Arity = Int

-- | Syntactic (visibility) arity, i.e. the number of visible arguments.
-- See Note [Visibility and arity]
type VisArity = Int

-- | Representation Arity
--
-- The number of represented arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100                    has representation arity 0
--  \x -> fib x                has representation arity 1
--  \(# x, y #) -> fib (x + y) has representation arity 2
type RepArity = Int

-- | The number of arguments that a join point takes. Unlike the arity of a
-- function, this is a purely syntactic property and is fixed when the join
-- point is created (or converted from a value). Both type and value arguments
-- are counted.
type JoinArity = Int

-- | FullArgCount is the number of type or value arguments in an application,
-- or the number of type or value binders in a lambda.  Note: it includes
-- both type and value arguments!
type FullArgCount = Int

{- Note [Visibility and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arity is the number of arguments that a function expects. In a curried language
like Haskell, there is more than one way to count those arguments.

* `Arity` is the classic notion of arity, concerned with evalution, so it counts
  the number of /value/ arguments that need to be supplied before evaluation can
  take place, as described in notes
    Note [Definition of arity]      in GHC.Core.Opt.Arity
    Note [Arity and function types] in GHC.Types.Id.Info

  Examples:
    Int                       has arity == 0
    Int -> Int                has arity <= 1
    Int -> Bool -> Int        has arity <= 2
  We write (<=) rather than (==) as sometimes evaluation can occur before all
  value arguments are supplied, depending on the actual function definition.

  This evaluation-focused notion of arity ignores type arguments, so:
    forall a.   a             has arity == 0
    forall a.   a -> a        has arity <= 1
    forall a b. a -> b -> a   has arity <= 2
  This is true regardless of ForAllTyFlag, so the arity is also unaffected by
  (forall {a}. ty) or (forall a -> ty).

  Class dictionaries count towards the arity, as they are passed at runtime
    forall a.   (Num a)        => a            has arity <= 1
    forall a.   (Num a)        => a -> a       has arity <= 2
    forall a b. (Num a, Ord b) => a -> b -> a  has arity <= 4

* `VisArity` is the syntactic notion of arity. It is the number of /visible/
  arguments, i.e. arguments that occur visibly in the source code.

  In a function call `f x y z`, we can confidently say that f's vis-arity >= 3,
  simply because we see three arguments [x,y,z]. We write (>=) rather than (==)
  as this could be a partial application.

  At definition sites, we can acquire an underapproximation of vis-arity by
  counting the patterns on the LHS, e.g. `f a b = rhs` has vis-arity >= 2.
  The actual vis-arity can be higher if there is a lambda on the RHS,
  e.g. `f a b = \c -> rhs`.

  If we look at the types, we can observe the following
    * function arrows   (a -> b)        add to the vis-arity
    * visible foralls   (forall a -> b) add to the vis-arity
    * constraint arrows (a => b)        do not affect the vis-arity
    * invisible foralls (forall a. b)   do not affect the vis-arity

  This means that ForAllTyFlag matters for VisArity (in contrast to Arity),
  while the type/value distinction is unimportant (again in contrast to Arity).

  Examples:
    Int                         -- vis-arity == 0   (no args)
    Int -> Int                  -- vis-arity == 1   (1 funarg)
    forall a. a -> a            -- vis-arity == 1   (1 funarg)
    forall a. Num a => a -> a   -- vis-arity == 1   (1 funarg)
    forall a -> Num a => a      -- vis-arity == 1   (1 req tyarg, 0 funargs)
    forall a -> a -> a          -- vis-arity == 2   (1 req tyarg, 1 funarg)
    Int -> forall a -> Int      -- vis-arity == 2   (1 funarg, 1 req tyarg)

  Wrinkle: with TypeApplications and TypeAbstractions, it is possible to visibly
  bind and pass invisible arguments, e.g. `f @a x = ...` or `f @Int 42`. Those
  @-prefixed arguments are ignored for the purposes of vis-arity.
-}

{- *********************************************************************
*                                                                      *
                 InlinePragma, InlineSpec, RuleMatchInfo
*                                                                      *
********************************************************************* -}


data InlinePragma            -- Note [InlinePragma]
  = InlinePragma
      { inl_src    :: Source.CodeSnippet -- See Note [Pragma source text]
      , inl_inline :: InlineSpec -- See Note [inl_inline and inl_act]

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the Unfolding, and don't look at inl_sat further

      , inl_act    :: Activation     -- Says during which phases inlining is allowed
                                     -- See Note [inl_inline and inl_act]

      , inl_rule   :: RuleMatchInfo  -- Should the function be treated like a constructor?
    } deriving( Eq, Data )

instance NFData InlinePragma where
  rnf (InlinePragma s a b c d) = rnf s `seq` rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- | Rule Match Information
data RuleMatchInfo = ConLike                    -- See Note [CONLIKE pragma]
                   | FunLike
                   deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

instance NFData RuleMatchInfo where
  rnf = \case
    ConLike -> ()
    FunLike -> ()

-- | Inline Specification
data InlineSpec   -- What the user's INLINE pragma looked like
  = Inline    Source.CodeSnippet -- User wrote INLINE
  | Inlinable Source.CodeSnippet -- User wrote INLINABLE
  | NoInline  Source.CodeSnippet -- User wrote NOINLINE
  | Opaque    Source.CodeSnippet -- User wrote OPAQUE
                               -- Each of the above keywords is accompanied with
                               -- a string of type Source.CodeSnippet written by the user
  | NoUserInlinePrag -- User did not write any of INLINE/INLINABLE/NOINLINE
                     -- e.g. in `defaultInlinePragma` or when created by CSE
  deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

instance NFData InlineSpec where
  rnf = \case
    Inline s -> rnf s
    NoInline s -> rnf s
    Inlinable s -> rnf s
    Opaque s -> rnf s
    NoUserInlinePrag -> ()

{- Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = NoUserInlinePrag
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = Inline or Inlineable, then the Id should have a stable unfolding.

If you want to know where InlinePragmas take effect: Look in GHC.HsToCore.Binds.makeCorePair

Note [inl_inline and inl_act]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* inl_inline says what the user wrote: did they say INLINE, NOINLINE,
  INLINABLE, OPAQUE, or nothing at all

* inl_act says in what phases the unfolding is active or inactive
  E.g  If you write INLINE[1]    then inl_act will be set to ActiveAfter 1
       If you write NOINLINE[1]  then inl_act will be set to ActiveBefore 1
       If you write NOINLINE[~1] then inl_act will be set to ActiveAfter 1
  So note that inl_act does not say what pragma you wrote: it just
  expresses its consequences

* inl_act just says when the unfolding is active; it doesn't say what
  to inline.  If you say INLINE f, then f's inl_act will be AlwaysActive,
  but in addition f will get a "stable unfolding" with UnfoldingGuidance
  that tells the inliner to be pretty eager about it.

Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through"
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function GHC.Core.Utils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in GHC.Core.Rules.

Note [OPAQUE pragma]
~~~~~~~~~~~~~~~~~~~~
Suppose a function `f` is marked {-# OPAQUE f #-}.  Then every call of `f`
should remain a call of `f` throughout optimisation; it should not be turned
into a call of a name-mangled variant of `f` (e.g by worker/wrapper).

The motivation for the OPAQUE pragma is discussed in GHC proposal 0415:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0415-opaque-pragma.rst
Basically it boils down to the desire of GHC API users and GHC RULE writers for
calls to certain binders to be left completely untouched by GHCs optimisations.

What this entails at the time of writing, is that for every binder annotated
with the OPAQUE pragma we:

* Do not do worker/wrapper via cast W/W:
  See the guard in GHC.Core.Opt.Simplify.tryCastWorkerWrapper

* Do not any worker/wrapper after demand/CPR analysis. To that end add a guard
  in GHC.Core.Opt.WorkWrap.tryWW to disable worker/wrapper

* It is important that the demand signature and CPR signature do not lie, else
  clients of the function will believe that it has the CPR property etc. But it
  won't, because we've disabled worker/wrapper. To avoid the signatures lying:
  * Strip boxity information from the demand signature
    in GHC.Core.Opt.DmdAnal.finaliseArgBoxities
    See Note [The OPAQUE pragma and avoiding the reboxing of arguments]
  * Strip CPR information from the CPR signature
    in GHC.Core.Opt.CprAnal.cprAnalBind
    See Note [The OPAQUE pragma and avoiding the reboxing of results]

* Do create specialised versions of the function in
  * Specialise: see GHC.Core.Opt.Specialise.specCalls
  * SpecConstr: see GHC.Core.Opt.SpecConstr.specialise
  Both are accomplished easily: these passes already skip NOINLINE
  functions with NeverActive activation, and an OPAQUE function is
  also NeverActive.

At the moment of writing, the major difference between the NOINLINE pragma and
the OPAQUE pragma is that binders annoted with the NOINLINE pragma _are_ W/W
transformed (see also Note [Worker/wrapper for NOINLINE functions]) where
binders annoted with the OPAQUE pragma are _not_ W/W transformed.

Future "name-mangling" optimisations should respect the OPAQUE pragma and
update the list of moving parts referenced in this note.

-}

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _       = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _       = False

noUserInlineSpec :: InlineSpec -> Bool
noUserInlineSpec NoUserInlinePrag = True
noUserInlineSpec _                = False

defaultInlinePragma, alwaysInlinePragma, neverInlinePragma, dfunInlinePragma
  :: InlinePragma
defaultInlinePragma = InlinePragma { inl_src = Source.mkCodeSnippet "{-# INLINE"
                                   , inl_act = AlwaysActive
                                   , inl_rule = FunLike
                                   , inl_inline = NoUserInlinePrag
                                   , inl_sat = Nothing }

alwaysInlinePragma = defaultInlinePragma { inl_inline = Inline (inlinePragmaSource defaultInlinePragma) }
neverInlinePragma  = defaultInlinePragma { inl_act    = NeverActive }

alwaysInlineConLikePragma :: InlinePragma
alwaysInlineConLikePragma = alwaysInlinePragma { inl_rule = ConLike }

inlinePragmaSpec :: InlinePragma -> InlineSpec
inlinePragmaSpec = inl_inline

inlinePragmaSource :: InlinePragma -> Source.CodeSnippet
inlinePragmaSource prag = inlineSpecSource (inl_inline prag)

inlineSpecSource :: InlineSpec -> Source.CodeSnippet
inlineSpecSource spec = case spec of
                            Inline    x      -> x
                            Inlinable y      -> y
                            NoInline  z      -> z
                            Opaque    q      -> q
                            NoUserInlinePrag -> Source.CodeSnippetAbsent

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma   = defaultInlinePragma { inl_act  = AlwaysActive
                                         , inl_rule = ConLike }

isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = noUserInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma -> Bool
isInlinePragma prag = case inl_inline prag of
                        Inline _  -> True
                        _         -> False

isInlinablePragma :: InlinePragma -> Bool
isInlinablePragma prag = case inl_inline prag of
                           Inlinable _  -> True
                           _            -> False

isNoInlinePragma :: InlinePragma -> Bool
isNoInlinePragma prag = case inl_inline prag of
                          NoInline _   -> True
                          _            -> False

isAnyInlinePragma :: InlinePragma -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma prag = case inl_inline prag of
                        Inline    _   -> True
                        Inlinable _   -> True
                        _             -> False

isOpaquePragma :: InlinePragma -> Bool
isOpaquePragma prag = case inl_inline prag of
                        Opaque _ -> True
                        _        -> False

inlinePragmaSat :: InlinePragma -> Maybe Arity
inlinePragmaSat = inl_sat

inlinePragmaActivation :: InlinePragma -> Activation
inlinePragmaActivation (InlinePragma { inl_act = activation }) = activation

inlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma { inl_rule = info }) = info

setInlinePragmaActivation :: InlinePragma -> Activation -> InlinePragma
setInlinePragmaActivation prag activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo -> InlinePragma
setInlinePragmaRuleMatchInfo prag info = prag { inl_rule = info }


{-
************************************************************************
*                                                                      *
\subsection{Activation}
*                                                                      *
************************************************************************

When a rule or inlining is active

Note [Compiler phases]
~~~~~~~~~~~~~~~~~~~~~~
The CompilerPhase says which phase the simplifier is running in:

* InitialPhase: before all user-visible phases

* Phase 2,1,0: user-visible phases; the phase number
  controls rule ordering an inlining.

* FinalPhase: used for all subsequent simplifier
  runs. By delaying inlining of wrappers to FinalPhase we can
  ensure that RULE have a good chance to fire. See
  Note [Wrapper activation] in GHC.Core.Opt.WorkWrap

  NB: FinalPhase is run repeatedly, not just once.

  NB: users don't have access to InitialPhase or FinalPhase.
  They write {-# INLINE[n] f #-}, meaning (Phase n)

The phase sequencing is done by GHC.Opt.Simplify.Driver
-}


-- Does not include GHC internal "initial" and "final" phases; see 'CompilerPhase'.
type PhaseNum = Int

-- | Compilation phase number, including the user-specifiable 'PhaseNum'
-- and the GHC internal "initial" and "final" phases.
data CompilerPhase
  = InitialPhase    -- ^ The first phase; number = infinity!
  | Phase PhaseNum  -- ^ User-specifiable phases
  | FinalPhase      -- ^ The last phase; number = -infinity!
  deriving (Eq, Data)

instance NFData CompilerPhase where
  rnf = \case
    InitialPhase -> ()
    FinalPhase -> ()
    Phase i -> rnf i

-- | An activation is a range of phases throughout which something is active
-- (like an INLINE pragma, SPECIALISE pragma, or RULE).
data Activation
  = AlwaysActive
  -- | Active only *strictly before* this phase
  | ActiveBefore PhaseNum
  -- | Active in this phase and later phases
  | ActiveAfter  PhaseNum
  -- | Active in the final phase only
  | FinalActive
  | NeverActive
  deriving( Eq, Data )
    -- Eq used in comparing rules in GHC.Hs.Decls

instance NFData Activation where
  rnf = \case
    AlwaysActive -> ()
    NeverActive -> ()
    ActiveBefore aa -> rnf aa
    ActiveAfter ab -> rnf ab
    FinalActive -> ()

beginPhase :: Activation -> CompilerPhase
-- ^ First phase in which the 'Activation' is active,
-- or 'FinalPhase' if it is never active
beginPhase AlwaysActive      = InitialPhase
beginPhase (ActiveBefore {}) = InitialPhase
beginPhase (ActiveAfter n)   = Phase n
beginPhase FinalActive       = FinalPhase
beginPhase NeverActive       = FinalPhase

endPhase :: Activation -> CompilerPhase
-- ^ Last phase in which the 'Activation' is active,
-- or 'InitialPhase' if it is never active
endPhase AlwaysActive       = FinalPhase
endPhase (ActiveBefore n)   =
  if nextPhase InitialPhase == Phase n
  then InitialPhase
  else Phase $ n + 1
endPhase (ActiveAfter {})   = FinalPhase
endPhase FinalActive        = FinalPhase
endPhase NeverActive        = InitialPhase

activeAfter :: CompilerPhase -> Activation
-- ^ @activeAfter p@ makes an 'Activation' that is active in phase @p@ and after
--
-- Invariant: @beginPhase (activeAfter p) = p@
activeAfter InitialPhase = AlwaysActive
activeAfter (Phase n)    = ActiveAfter n
activeAfter FinalPhase   = FinalActive

nextPhase :: CompilerPhase -> CompilerPhase
-- ^ Tells you the next phase after this one
--
-- Currently we have just phases @[2,1,0,FinalPhase,FinalPhase,...]@,
-- where FinalPhase means GHC's internal simplification steps
-- after all rules have run
nextPhase InitialPhase = Phase 2
nextPhase (Phase 0)    = FinalPhase
nextPhase (Phase n)    = Phase (n-1)
nextPhase FinalPhase   = FinalPhase

laterPhase :: CompilerPhase -> CompilerPhase -> CompilerPhase
-- ^ Returns the later of two phases
laterPhase (Phase n1)   (Phase n2)   = Phase (n1 `min` n2)
laterPhase InitialPhase p2           = p2
laterPhase FinalPhase   _            = FinalPhase
laterPhase p1           InitialPhase = p1
laterPhase _            FinalPhase   = FinalPhase

-- | @p1 `laterThanOrEqualPhase` p2@ computes whether @p1@ happens (strictly)
-- after @p2@.
laterThanPhase :: CompilerPhase -> CompilerPhase -> Bool
p1 `laterThanPhase` p2 = toNum p1 < toNum p2
  where
    toNum :: CompilerPhase -> Int
    toNum InitialPhase = maxBound
    toNum (Phase i)    = i
    toNum FinalPhase   = minBound

activateAfterInitial :: Activation
-- ^ Active in the first phase after the initial phase
activateAfterInitial = activeAfter (nextPhase InitialPhase)

activateDuringFinal :: Activation
-- ^ Active in the final simplification phase (which is repeated)
activateDuringFinal = FinalActive

isActiveInPhase :: CompilerPhase -> Activation -> Bool
isActiveInPhase InitialPhase act = activeInInitialPhase act
isActiveInPhase (Phase p)    act = activeInPhase p act
isActiveInPhase FinalPhase   act = activeInFinalPhase act

activeInInitialPhase :: Activation -> Bool
activeInInitialPhase act = beginPhase act == InitialPhase

activeInPhase :: PhaseNum -> Activation -> Bool
activeInPhase _ AlwaysActive     = True
activeInPhase _ NeverActive      = False
activeInPhase _ FinalActive      = False
activeInPhase p (ActiveAfter  n) = p <= n
activeInPhase p (ActiveBefore n) = p >  n

activeInFinalPhase :: Activation -> Bool
activeInFinalPhase AlwaysActive     = True
activeInFinalPhase FinalActive      = True
activeInFinalPhase (ActiveAfter {}) = True
activeInFinalPhase _                = False

isNeverActive, isAlwaysActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

-- | @act1 `competesWith` act2@ returns whether @act1@ is active in the phase
-- when @act2@ __becomes__ active.
--
-- This answers the question: might @act1@ fire first?
--
-- NB: this is not the same as computing whether @act1@ and @act2@ are
-- ever active at the same time.
--
-- See Note [Competing activations]
competesWith :: Activation -> Activation -> Bool
competesWith NeverActive  _           = False
competesWith _            NeverActive = False -- See Wrinkle [Never active rules]
competesWith act1         act2        = isActiveInPhase (beginPhase act2) act1

{- Note [Competing activations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes a RULE and an inlining may compete, or two RULES.
See Note [Rules and inlining/other rules] in GHC.HsToCore.

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.

  Wrinkle [Never active rules]

    Rules can be declared as "never active" by users, using the syntax:

      {-# RULE "blah" [~] ... #-}

        (This feature exists solely for compiler plugins, by making it possible
        to def~ine a RULE that is never run by GHC, but is nevertheless parsed,
        typechecked etc, so that it is available to the plugin.)

    We should not warn about competing rules, so make sure that 'competesWith'
    always returns 'False' when its second argument is 'NeverActive'.
-}
