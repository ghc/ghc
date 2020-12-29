{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import GHC.Prelude

import {-# SOURCE #-} Language.Haskell.Syntax.Expr
  ( LHsExpr
  , MatchGroup
  , GRHSs )
import {-# SOURCE #-} Language.Haskell.Syntax.Pat
  ( LPat )

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Type
import GHC.Tc.Types.Evidence
import GHC.Core.Type
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Tickish
import GHC.Types.Var
import GHC.Types.Fixity
import GHC.Data.Bag
import GHC.Data.BooleanFormula (LBooleanFormula)

import GHC.Utils.Outputable

import Data.Data hiding ( Fixity )
import Data.Void

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
type LHsBindsLR idL idR = Bag (LHsBindLR idL idR)

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
    --
    --  'GHC.Parser.Annotation.AnnKeywordId's
    --
    --  - 'GHC.Parser.Annotation.AnnFunId', attached to each element of fun_matches
    --
    --  - 'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnWhere',
    --    'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose',

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
    FunBind {

        fun_ext :: XFunBind idL idR,

          -- ^ After the renamer (but before the type-checker), this contains the
          -- locally-bound free variables of this defn. See Note [Bind free vars]
          --
          -- After the type-checker, this contains a coercion from the type of
          -- the MatchGroup to the type of the Id. Example:
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

        fun_id :: LIdP idL, -- Note [fun_id in Match] in GHC.Hs.Expr

        fun_matches :: MatchGroup idR (LHsExpr idR),  -- ^ The payload

        fun_tick :: [CoreTickish] -- ^ Ticks to put on the rhs, if any
    }

  -- | Pattern Binding
  --
  -- The pattern is never a simple variable;
  -- That case is done by FunBind.
  -- See Note [FunBind vs PatBind] for details about the
  -- relationship between FunBind and PatBind.

  --
  --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnBang',
  --       'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnWhere',
  --       'GHC.Parser.Annotation.AnnOpen','GHC.Parser.Annotation.AnnClose',

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | PatBind {
        pat_ext    :: XPatBind idL idR, -- ^ See Note [Bind free vars]
        pat_lhs    :: LPat idL,
        pat_rhs    :: GRHSs idR (LHsExpr idR),
        pat_ticks  :: ([CoreTickish], [[CoreTickish]])
               -- ^ Ticks to put on the rhs, if any, and ticks to put on
               -- the bound variables.
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

  -- | Abstraction Bindings
  | AbsBinds {                      -- Binds abstraction; TRANSLATION
        abs_ext     :: XAbsBinds idL idR,
        abs_tvs     :: [TyVar],
        abs_ev_vars :: [EvVar],  -- ^ Includes equality constraints

       -- | AbsBinds only gets used when idL = idR after renaming,
       -- but these need to be idL's for the collect... code in HsUtil
       -- to have the right type
        abs_exports :: [ABExport idL],

        -- | Evidence bindings
        -- Why a list? See "GHC.Tc.TyCl.Instance"
        -- Note [Typechecking plan for instance declarations]
        abs_ev_binds :: [TcEvBinds],

        -- | Typechecked user bindings
        abs_binds    :: LHsBinds idL,

        abs_sig :: Bool  -- See Note [The abs_sig field of AbsBinds]
    }

  -- | Patterns Synonym Binding
  | PatSynBind
        (XPatSynBind idL idR)
        (PatSynBind idL idR)
        -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnPattern',
        --          'GHC.Parser.Annotation.AnnLarrow','GHC.Parser.Annotation.AnnEqual',
        --          'GHC.Parser.Annotation.AnnWhere'
        --          'GHC.Parser.Annotation.AnnOpen' @'{'@,'GHC.Parser.Annotation.AnnClose' @'}'@

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

  | XHsBindsLR !(XXHsBindsLR idL idR)


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

-- | Abstraction Bindings Export
data ABExport p
  = ABE { abe_ext       :: XABE p
        , abe_poly      :: IdP p -- ^ Any INLINE pragma is attached to this Id
        , abe_mono      :: IdP p
        , abe_wrap      :: HsWrapper    -- ^ See Note [ABExport wrapper]
             -- Shape: (forall abs_tvs. abs_ev_vars => abe_mono) ~ abe_poly
        , abe_prags     :: TcSpecPrags  -- ^ SPECIALISE pragmas
        }
   | XABExport !(XXABExport p)


-- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnPattern',
--             'GHC.Parser.Annotation.AnnEqual','GHC.Parser.Annotation.AnnLarrow',
--             'GHC.Parser.Annotation.AnnWhere','GHC.Parser.Annotation.AnnOpen' @'{'@,
--             'GHC.Parser.Annotation.AnnClose' @'}'@,

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- | Pattern Synonym binding
data PatSynBind idL idR
  = PSB { psb_ext  :: XPSB idL idR,            -- ^ Post renaming, FVs.
                                               -- See Note [Bind free vars]
          psb_id   :: LIdP idL,                -- ^ Name of the pattern synonym
          psb_args :: HsPatSynDetails idR,     -- ^ Formal parameter names
          psb_def  :: LPat idR,                -- ^ Right-hand side
          psb_dir  :: HsPatSynDir idR          -- ^ Directionality
     }
   | XPatSynBind !(XXPatSynBind idL idR)

{-
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

The 'fwrap' is an impedance-matcher that typically does nothing; see
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


Note [The abs_sig field of AbsBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The abs_sig field supports a couple of special cases for bindings.
Consider

  x :: Num a => (# a, a #)
  x = (# 3, 4 #)

The general desugaring for AbsBinds would give

  x = /\a. \ ($dNum :: Num a) ->
      letrec xm = (# fromInteger $dNum 3, fromInteger $dNum 4 #) in
      xm

But that has an illegal let-binding for an unboxed tuple.  In this
case we'd prefer to generate the (more direct)

  x = /\ a. \ ($dNum :: Num a) ->
     (# fromInteger $dNum 3, fromInteger $dNum 4 #)

A similar thing happens with representation-polymorphic defns
(#11405):

  undef :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a
  undef = error "undef"

Again, the vanilla desugaring gives a local let-binding for a
representation-polymorphic (undefm :: a), which is illegal.  But
again we can desugar without a let:

  undef = /\ a. \ (d:HasCallStack) -> error a d "undef"

The abs_sig field supports this direct desugaring, with no local
let-binding.  When abs_sig = True

 * the abs_binds is single FunBind

 * the abs_exports is a singleton

 * we have a complete type sig for binder
   and hence the abs_binds is non-recursive
   (it binds the mono_id but refers to the poly_id

These properties are exploited in GHC.HsToCore.Binds.dsAbsBinds to
generate code without a let-binding.

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
variables.  The action happens in GHC.Tc.Gen.Bind.mkExport.

Note [Bind free vars]
~~~~~~~~~~~~~~~~~~~~~
The bind_fvs field of FunBind and PatBind records the free variables
of the definition.  It is used for the following purposes

a) Dependency analysis prior to type checking
    (see GHC.Tc.Gen.Bind.tc_group)

b) Deciding whether we can do generalisation of the binding
    (see GHC.Tc.Gen.Bind.decideGeneralisationPlan)

c) Deciding whether the binding can be used in static forms
    (see GHC.Tc.Gen.Expr.checkClosedInStaticForm for the HsStatic case and
     GHC.Tc.Gen.Bind.isClosedBndrGroup).

Specifically,

  * bind_fvs includes all free vars that are defined in this module
    (including top-level things and lexically scoped type variables)

  * bind_fvs excludes imported vars; this is just to keep the set smaller

  * Before renaming, and after typechecking, the field is unused;
    it's just an error thunk
-}


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
-- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnSemi' when in a
--   list

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation

-- | Implicit parameter bindings.
--
-- These bindings start off as (Left "x") in the parser and stay
-- that way until after type-checking when they are replaced with
-- (Right d), where "d" is the name of the dictionary holding the
-- evidence for the implicit parameter.
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnEqual'

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation
data IPBind id
  = IPBind
        (XCIPBind id)
        (Either (XRec id HsIPName) (IdP id))
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
      --
      --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon',
      --          'GHC.Parser.Annotation.AnnComma'

      -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
    TypeSig
       (XTypeSig pass)
       [LIdP pass]           -- LHS of the signature; e.g.  f,g,h :: blah
       (LHsSigWcType pass)   -- RHS of the signature; can have wildcards

      -- | A pattern synonym type signature
      --
      -- > pattern Single :: () => (Show a) => a -> [a]
      --
      --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnPattern',
      --           'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnForall'
      --           'GHC.Parser.Annotation.AnnDot','GHC.Parser.Annotation.AnnDarrow'

      -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | PatSynSig (XPatSynSig pass) [LIdP pass] (LHsSigType pass)
      -- P :: forall a b. Req => Prov => ty

      -- | A signature for a class method
      --   False: ordinary class-method signature
      --   True:  generic-default class method signature
      -- e.g.   class C a where
      --          op :: a -> a                   -- Ordinary
      --          default op :: Eq a => a -> a   -- Generic default
      -- No wildcards allowed here
      --
      --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDefault',
      --           'GHC.Parser.Annotation.AnnDcolon'
  | ClassOpSig (XClassOpSig pass) Bool [LIdP pass] (LHsSigType pass)

        -- | A type signature in generated code, notably the code
        -- generated for record selectors.  We simply record
        -- the desired Id itself, replete with its name, type
        -- and IdDetails.  Otherwise it's just like a type
        -- signature: there should be an accompanying binding
  | IdSig (XIdSig pass) Id

        -- | An ordinary fixity declaration
        --
        -- >     infixl 8 ***
        --
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnInfix',
        --           'GHC.Parser.Annotation.AnnVal'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | FixSig (XFixSig pass) (FixitySig pass)

        -- | An inline pragma
        --
        -- > {#- INLINE f #-}
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' :
        --       'GHC.Parser.Annotation.AnnOpen' @'{-\# INLINE'@ and @'['@,
        --       'GHC.Parser.Annotation.AnnClose','GHC.Parser.Annotation.AnnOpen',
        --       'GHC.Parser.Annotation.AnnVal','GHC.Parser.Annotation.AnnTilde',
        --       'GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | InlineSig   (XInlineSig pass)
                (LIdP pass)        -- Function name
                InlinePragma       -- Never defaultInlinePragma

        -- | A specialisation pragma
        --
        -- > {-# SPECIALISE f :: Int -> Int #-}
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --      'GHC.Parser.Annotation.AnnOpen' @'{-\# SPECIALISE'@ and @'['@,
        --      'GHC.Parser.Annotation.AnnTilde',
        --      'GHC.Parser.Annotation.AnnVal',
        --      'GHC.Parser.Annotation.AnnClose' @']'@ and @'\#-}'@,
        --      'GHC.Parser.Annotation.AnnDcolon'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | SpecSig     (XSpecSig pass)
                (LIdP pass)        -- Specialise a function or datatype  ...
                [LHsSigType pass]  -- ... to these types
                InlinePragma       -- The pragma on SPECIALISE_INLINE form.
                                   -- If it's just defaultInlinePragma, then we said
                                   --    SPECIALISE, not SPECIALISE_INLINE

        -- | A specialisation pragma for instance declarations only
        --
        -- > {-# SPECIALISE instance Eq [Int] #-}
        --
        -- (Class tys); should be a specialisation of the
        -- current instance declaration
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --      'GHC.Parser.Annotation.AnnInstance','GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | SpecInstSig (XSpecInstSig pass) SourceText (LHsSigType pass)
                  -- Note [Pragma source text] in GHC.Types.SourceText

        -- | A minimal complete definition pragma
        --
        -- > {-# MINIMAL a | (b, c | (d | e)) #-}
        --
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --      'GHC.Parser.Annotation.AnnVbar','GHC.Parser.Annotation.AnnComma',
        --      'GHC.Parser.Annotation.AnnClose'

        -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | MinimalSig (XMinimalSig pass)
               SourceText (LBooleanFormula (LIdP pass))
               -- Note [Pragma source text] in GHC.Types.SourceText

        -- | A "set cost centre" pragma for declarations
        --
        -- > {-# SCC funName #-}
        --
        -- or
        --
        -- > {-# SCC funName "cost_centre_name" #-}

  | SCCFunSig  (XSCCFunSig pass)
               SourceText     -- Note [Pragma source text] in GHC.Types.SourceText
               (LIdP pass)    -- Function name
               (Maybe (XRec pass StringLiteral))
       -- | A complete match pragma
       --
       -- > {-# COMPLETE C, D [:: T] #-}
       --
       -- Used to inform the pattern match checker about additional
       -- complete matchings which, for example, arise from pattern
       -- synonym definitions.
  | CompleteMatchSig (XCompleteMatchSig pass)
                     SourceText
                     (XRec pass [LIdP pass])
                     (Maybe (LIdP pass))
  | XSig !(XXSig pass)

-- | Located Fixity Signature
type LFixitySig pass = XRec pass (FixitySig pass)

-- | Fixity Signature
data FixitySig pass = FixitySig (XFixitySig pass) [LIdP pass] Fixity
                    | XFixitySig !(XXFixitySig pass)

-- | Type checker Specialisation Pragmas
--
-- 'TcSpecPrags' conveys @SPECIALISE@ pragmas from the type checker to the desugarer
data TcSpecPrags
  = IsDefaultMethod     -- ^ Super-specialised: a default method should
                        -- be macro-expanded at every call site
  | SpecPrags [LTcSpecPrag]
  deriving Data

-- | Located Type checker Specification Pragmas
type LTcSpecPrag = Located TcSpecPrag

-- | Type checker Specification Pragma
data TcSpecPrag
  = SpecPrag
        Id
        HsWrapper
        InlinePragma
  -- ^ The Id to be specialised, a wrapper that specialises the
  -- polymorphic function, and inlining spec for the specialised function
  deriving Data

noSpecPrags :: TcSpecPrags
noSpecPrags = SpecPrags []

hasSpecPrags :: TcSpecPrags -> Bool
hasSpecPrags (SpecPrags ps) = not (null ps)
hasSpecPrags IsDefaultMethod = False

isDefaultMethod :: TcSpecPrags -> Bool
isDefaultMethod IsDefaultMethod = True
isDefaultMethod (SpecPrags {})  = False

isFixityLSig :: forall p. UnXRec p => LSig p -> Bool
isFixityLSig (unXRec @p -> FixSig {}) = True
isFixityLSig _                 = False

isTypeLSig :: forall p. UnXRec p => LSig p -> Bool  -- Type signatures
isTypeLSig (unXRec @p -> TypeSig {})    = True
isTypeLSig (unXRec @p -> ClassOpSig {}) = True
isTypeLSig (unXRec @p -> IdSig {})      = True
isTypeLSig _                    = False

isSpecLSig :: forall p. UnXRec p => LSig p -> Bool
isSpecLSig (unXRec @p -> SpecSig {}) = True
isSpecLSig _                 = False

isSpecInstLSig :: forall p. UnXRec p => LSig p -> Bool
isSpecInstLSig (unXRec @p -> SpecInstSig {}) = True
isSpecInstLSig _                      = False

isPragLSig :: forall p. UnXRec p => LSig p -> Bool
-- Identifies pragmas
isPragLSig (unXRec @p -> SpecSig {})   = True
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

hsSigDoc :: Sig name -> SDoc
hsSigDoc (TypeSig {})           = text "type signature"
hsSigDoc (PatSynSig {})         = text "pattern synonym signature"
hsSigDoc (ClassOpSig _ is_deflt _ _)
 | is_deflt                     = text "default type signature"
 | otherwise                    = text "class method signature"
hsSigDoc (IdSig {})             = text "id signature"
hsSigDoc (SpecSig _ _ _ inl)
                                = ppr inl <+> text "pragma"
hsSigDoc (InlineSig _ _ prag)   = ppr (inlinePragmaSpec prag) <+> text "pragma"
hsSigDoc (SpecInstSig _ src _)
                                = pprWithSourceText src empty <+> text "instance pragma"
hsSigDoc (FixSig {})            = text "fixity declaration"
hsSigDoc (MinimalSig {})        = text "MINIMAL pragma"
hsSigDoc (SCCFunSig {})         = text "SCC pragma"
hsSigDoc (CompleteMatchSig {})  = text "COMPLETE pragma"
hsSigDoc (XSig {})              = text "XSIG TTG extension"

{-
************************************************************************
*                                                                      *
\subsection[PatSynBind]{A pattern synonym definition}
*                                                                      *
************************************************************************
-}

-- | Haskell Pattern Synonym Details
type HsPatSynDetails pass = HsConDetails Void (LIdP pass) [RecordPatSynField pass]

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
instance Outputable (RecordPatSynField a) where
    ppr (RecordPatSynField { recordPatSynField = v }) = ppr v


-- | Haskell Pattern Synonym Direction
data HsPatSynDir id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup id (LHsExpr id))
