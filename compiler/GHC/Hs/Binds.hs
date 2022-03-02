{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @BindGroup@, @Bind@, @Sig@, @Bind@.
-}

module GHC.Hs.Binds
  ( module Language.Haskell.Syntax.Binds
  , module GHC.Hs.Binds
  ) where

import GHC.Prelude

import Language.Haskell.Syntax.Binds

import {-# SOURCE #-} GHC.Hs.Expr ( pprExpr, pprFunBind, pprPatBind )
import {-# SOURCE #-} GHC.Hs.Pat  (pprLPat )

import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Hs.Type
import GHC.Tc.Types.Evidence
import GHC.Core.Type
import GHC.Types.Name.Set
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Var
import GHC.Data.Bag
import GHC.Data.BooleanFormula (LBooleanFormula)
import GHC.Types.Name.Reader
import GHC.Types.Name

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Function
import Data.List (sortBy)
import Data.Data (Data)

{-
************************************************************************
*                                                                      *
\subsection{Bindings: @BindGroup@}
*                                                                      *
************************************************************************

Global bindings (where clauses)
-}

-- the ...LR datatypes are parametrized by two id types,
-- one for the left and one for the right.

type instance XHsValBinds      (GhcPass pL) (GhcPass pR) = EpAnn AnnList
type instance XHsIPBinds       (GhcPass pL) (GhcPass pR) = EpAnn AnnList
type instance XEmptyLocalBinds (GhcPass pL) (GhcPass pR) = NoExtField
type instance XXHsLocalBindsLR (GhcPass pL) (GhcPass pR) = DataConCantHappen

-- ---------------------------------------------------------------------
-- Deal with ValBindsOut

-- TODO: make this the only type for ValBinds
data NHsValBindsLR idL
  = NValBinds
      [(RecFlag, LHsBinds idL)]
      [LSig GhcRn]

type instance XValBinds    (GhcPass pL) (GhcPass pR) = AnnSortKey
type instance XXValBindsLR (GhcPass pL) pR
            = NHsValBindsLR (GhcPass pL)

-- ---------------------------------------------------------------------

type instance XFunBind    (GhcPass pL) GhcPs = NoExtField
type instance XFunBind    (GhcPass pL) GhcRn = NameSet
-- ^ After the renamer (but before the type-checker), the FunBind
-- extension field contains the locally-bound free variables of this
-- defn. See Note [Bind free vars]

type instance XFunBind    (GhcPass pL) GhcTc = HsWrapper
-- ^ After the type-checker, the FunBind extension field contains a
-- coercion from the type of the MatchGroup to the type of the Id.
-- Example:
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

type instance XPatBind    GhcPs (GhcPass pR) = EpAnn [AddEpAnn]
type instance XPatBind    GhcRn (GhcPass pR) = NameSet -- See Note [Bind free vars]
type instance XPatBind    GhcTc (GhcPass pR) = Type    -- Type of the GRHSs

type instance XVarBind    (GhcPass pL) (GhcPass pR) = NoExtField
type instance XPatSynBind (GhcPass pL) (GhcPass pR) = NoExtField

type instance XXHsBindsLR GhcPs pR = DataConCantHappen
type instance XXHsBindsLR GhcRn pR = DataConCantHappen
type instance XXHsBindsLR GhcTc pR = AbsBinds

type instance XPSB         (GhcPass idL) GhcPs = EpAnn [AddEpAnn]
type instance XPSB         (GhcPass idL) GhcRn = NameSet -- Post renaming, FVs. See Note [Bind free vars]
type instance XPSB         (GhcPass idL) GhcTc = NameSet

type instance XXPatSynBind (GhcPass idL) (GhcPass idR) = DataConCantHappen

-- ---------------------------------------------------------------------

-- | Typechecked, generalised bindings, used in the output to the type checker.
-- See Note [AbsBinds].
data AbsBinds = AbsBinds {
      abs_tvs     :: [TyVar],
      abs_ev_vars :: [EvVar],  -- ^ Includes equality constraints

     -- | AbsBinds only gets used when idL = idR after renaming,
     -- but these need to be idL's for the collect... code in HsUtil
     -- to have the right type
      abs_exports :: [ABExport],

      -- | Evidence bindings
      -- Why a list? See "GHC.Tc.TyCl.Instance"
      -- Note [Typechecking plan for instance declarations]
      abs_ev_binds :: [TcEvBinds],

      -- | Typechecked user bindings
      abs_binds    :: LHsBinds GhcTc,

      abs_sig :: Bool  -- See Note [The abs_sig field of AbsBinds]
  }


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
data ABExport
  = ABE { abe_poly      :: Id           -- ^ Any INLINE pragma is attached to this Id
        , abe_mono      :: Id
        , abe_wrap      :: HsWrapper    -- ^ See Note [ABExport wrapper]
             -- Shape: (forall abs_tvs. abs_ev_vars => abe_mono) ~ abe_poly
        , abe_prags     :: TcSpecPrags  -- ^ SPECIALISE pragmas
        }

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
The extension fields of FunBind, PatBind and PatSynBind at GhcRn records the free
variables of the definition.  It is used for the following purposes:

a) Dependency analysis prior to type checking
    (see GHC.Tc.Gen.Bind.tc_group)

b) Deciding whether we can do generalisation of the binding
    (see GHC.Tc.Gen.Bind.decideGeneralisationPlan)

c) Deciding whether the binding can be used in static forms
    (see GHC.Tc.Gen.Expr.checkClosedInStaticForm for the HsStatic case and
     GHC.Tc.Gen.Bind.isClosedBndrGroup).

Specifically,

  * it includes all free vars that are defined in this module
    (including top-level things and lexically scoped type variables)

  * it excludes imported vars; this is just to keep the set smaller

  * Before renaming, and after typechecking, the field is unused;
    it's just an error thunk
-}

instance (OutputableBndrId pl, OutputableBndrId pr)
        => Outputable (HsLocalBindsLR (GhcPass pl) (GhcPass pr)) where
  ppr (HsValBinds _ bs)   = ppr bs
  ppr (HsIPBinds _ bs)    = ppr bs
  ppr (EmptyLocalBinds _) = empty

instance (OutputableBndrId pl, OutputableBndrId pr)
        => Outputable (HsValBindsLR (GhcPass pl) (GhcPass pr)) where
  ppr (ValBinds _ binds sigs)
   = pprDeclList (pprLHsBindsForUser binds sigs)

  ppr (XValBindsLR (NValBinds sccs sigs))
    = getPprDebug $ \case
        -- Print with sccs showing
        True  -> vcat (map ppr sigs) $$ vcat (map ppr_scc sccs)
        False -> pprDeclList (pprLHsBindsForUser (unionManyBags (map snd sccs)) sigs)
   where
     ppr_scc (rec_flag, binds) = pp_rec rec_flag <+> pprLHsBinds binds
     pp_rec Recursive    = text "rec"
     pp_rec NonRecursive = text "nonrec"

pprLHsBinds :: (OutputableBndrId idL, OutputableBndrId idR)
            => LHsBindsLR (GhcPass idL) (GhcPass idR) -> SDoc
pprLHsBinds binds
  | isEmptyLHsBinds binds = empty
  | otherwise = pprDeclList (map ppr (bagToList binds))

pprLHsBindsForUser :: (OutputableBndrId idL,
                       OutputableBndrId idR,
                       OutputableBndrId id2)
     => LHsBindsLR (GhcPass idL) (GhcPass idR) -> [LSig (GhcPass id2)] -> [SDoc]
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
    decls = [(locA loc, ppr sig)  | L loc sig <- sigs] ++
            [(locA loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortBy (SrcLoc.leftmost_smallest `on` fst) decls

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

------------
emptyLocalBinds :: HsLocalBindsLR (GhcPass a) (GhcPass b)
emptyLocalBinds = EmptyLocalBinds noExtField

eqEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
eqEmptyLocalBinds (EmptyLocalBinds _) = True
eqEmptyLocalBinds _                   = False

isEmptyValBinds :: HsValBindsLR (GhcPass a) (GhcPass b) -> Bool
isEmptyValBinds (ValBinds _ ds sigs)  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (XValBindsLR (NValBinds ds sigs)) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR (GhcPass a) (GhcPass b)
emptyValBindsIn  = ValBinds NoAnnSortKey emptyBag []
emptyValBindsOut = XValBindsLR (NValBinds [] [])

emptyLHsBinds :: LHsBindsLR (GhcPass idL) idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR (GhcPass idL) idR -> Bool
isEmptyLHsBinds = isEmptyBag

------------
plusHsValBinds :: HsValBinds (GhcPass a) -> HsValBinds (GhcPass a)
               -> HsValBinds(GhcPass a)
plusHsValBinds (ValBinds _ ds1 sigs1) (ValBinds _ ds2 sigs2)
  = ValBinds NoAnnSortKey (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (XValBindsLR (NValBinds ds1 sigs1))
               (XValBindsLR (NValBinds ds2 sigs2))
  = XValBindsLR (NValBinds (ds1 ++ ds2) (sigs1 ++ sigs2))
plusHsValBinds _ _
  = panic "HsBinds.plusHsValBinds"

instance (OutputableBndrId pl, OutputableBndrId pr)
         => Outputable (HsBindLR (GhcPass pl) (GhcPass pr)) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: forall idL idR.
                (OutputableBndrId idL, OutputableBndrId idR)
             => HsBindLR (GhcPass idL) (GhcPass idR) -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = pprPatBind pat grhss
ppr_monobind (VarBind { var_id = var, var_rhs = rhs })
  = sep [pprBndr CasePatBind var, nest 2 $ equals <+> pprExpr (unLoc rhs)]
ppr_monobind (FunBind { fun_id = fun,
                        fun_matches = matches,
                        fun_tick = ticks,
                        fun_ext = wrap })
  = pprTicks empty (if null ticks then empty
                    else text "-- ticks = " <> ppr ticks)
    $$  whenPprDebug (pprBndr LetBind (unLoc fun))
    $$  pprFunBind  matches
    $$  whenPprDebug (pprIfTc @idR $ ppr wrap)

ppr_monobind (PatSynBind _ psb) = ppr psb
ppr_monobind (XHsBindsLR b) = case ghcPass @idL of
#if __GLASGOW_HASKELL__ <= 900
  GhcPs -> dataConCantHappen b
  GhcRn -> dataConCantHappen b
#endif
  GhcTc -> ppr_absbinds b
    where
      ppr_absbinds (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dictvars
                             , abs_exports = exports, abs_binds = val_binds
                             , abs_ev_binds = ev_binds })
        = sdocOption sdocPrintTypecheckerElaboration $ \case
          False -> pprLHsBinds val_binds
          True  -> -- Show extra information (bug number: #10662)
                   hang (text "AbsBinds"
                         <+> sep [ brackets (interpp'SP tyvars)
                                 , brackets (interpp'SP dictvars) ])
                      2 $ braces $ vcat
                   [ text "Exports:" <+>
                       brackets (sep (punctuate comma (map ppr exports)))
                   , text "Exported types:" <+>
                       vcat [pprBndr LetBind (abe_poly ex) | ex <- exports]
                   , text "Binds:" <+> pprLHsBinds val_binds
                   , pprIfTc @idR (text "Evidence:" <+> ppr ev_binds)
                   ]

instance Outputable ABExport where
  ppr (ABE { abe_wrap = wrap, abe_poly = gbl, abe_mono = lcl, abe_prags = prags })
    = vcat [ sep [ ppr gbl, nest 2 (text "<=" <+> ppr lcl) ]
           , nest 2 (pprTcSpecPrags prags)
           , ppr $ nest 2 (text "wrap:" <+> ppr wrap) ]

instance (OutputableBndrId l, OutputableBndrId r)
          => Outputable (PatSynBind (GhcPass l) (GhcPass r)) where
  ppr (PSB{ psb_id = (L _ psyn), psb_args = details, psb_def = pat,
            psb_dir = dir })
      = ppr_lhs <+> ppr_rhs
    where
      ppr_lhs = text "pattern" <+> ppr_details
      ppr_simple syntax = syntax <+> pprLPat pat

      ppr_details = case details of
          InfixCon v1 v2 -> hsep [ppr_v v1, pprInfixOcc psyn, ppr_v  v2]
            where
                ppr_v v = case ghcPass @r of
                    GhcPs -> ppr v
                    GhcRn -> ppr v
                    GhcTc -> ppr v
          PrefixCon _ vs -> hsep (pprPrefixOcc psyn : map ppr_v vs)
            where
                ppr_v v = case ghcPass @r of
                    GhcPs -> ppr v
                    GhcRn -> ppr v
                    GhcTc -> ppr v
          RecCon vs      -> pprPrefixOcc psyn
                            <> braces (sep (punctuate comma (map ppr_v vs)))
            where
                ppr_v v = case ghcPass @r of
                    GhcPs -> ppr v
                    GhcRn -> ppr v
                    GhcTc -> ppr v

      ppr_rhs = case dir of
          Unidirectional           -> ppr_simple (text "<-")
          ImplicitBidirectional    -> ppr_simple equals
          ExplicitBidirectional mg -> ppr_simple (text "<-") <+> text "where" $$
                                      (nest 2 $ pprFunBind mg)

pprTicks :: SDoc -> SDoc -> SDoc
-- Print stuff about ticks only when -dppr-debug is on, to avoid
-- them appearing in error messages (from the desugarer); see # 3263
-- Also print ticks in dumpStyle, so that -ddump-hpc actually does
-- something useful.
pprTicks pp_no_debug pp_when_debug
  = getPprStyle $ \sty ->
    getPprDebug $ \debug ->
      if debug || dumpStyle sty
         then pp_when_debug
         else pp_no_debug

{-
************************************************************************
*                                                                      *
                Implicit parameter bindings
*                                                                      *
************************************************************************
-}

type instance XIPBinds       GhcPs = NoExtField
type instance XIPBinds       GhcRn = NoExtField
type instance XIPBinds       GhcTc = TcEvBinds -- binds uses of the
                                               -- implicit parameters


type instance XXHsIPBinds    (GhcPass p) = DataConCantHappen

isEmptyIPBindsPR :: HsIPBinds (GhcPass p) -> Bool
isEmptyIPBindsPR (IPBinds _ is) = null is

isEmptyIPBindsTc :: HsIPBinds GhcTc -> Bool
isEmptyIPBindsTc (IPBinds ds is) = null is && isEmptyTcEvBinds ds

-- EPA annotations in GhcPs, dictionary Id in GhcTc
type instance XCIPBind GhcPs = EpAnn [AddEpAnn]
type instance XCIPBind GhcRn = NoExtField
type instance XCIPBind GhcTc = Id
type instance XXIPBind    (GhcPass p) = DataConCantHappen

instance OutputableBndrId p
       => Outputable (HsIPBinds (GhcPass p)) where
  ppr (IPBinds ds bs) = pprDeeperList vcat (map ppr bs)
                        $$ whenPprDebug (pprIfTc @p $ ppr ds)

instance OutputableBndrId p => Outputable (IPBind (GhcPass p)) where
  ppr (IPBind x (L _ ip) rhs) = name <+> equals <+> pprExpr (unLoc rhs)
    where name = case ghcPass @p of
            GhcPs -> pprBndr LetBind ip
            GhcRn -> pprBndr LetBind ip
            GhcTc -> pprBndr LetBind x

{-
************************************************************************
*                                                                      *
\subsection{@Sig@: type signatures and value-modifying user pragmas}
*                                                                      *
************************************************************************
-}

type instance XTypeSig          (GhcPass p) = EpAnn AnnSig
type instance XPatSynSig        (GhcPass p) = EpAnn AnnSig
type instance XClassOpSig       (GhcPass p) = EpAnn AnnSig
type instance XIdSig            (GhcPass p) = NoExtField -- No anns, generated
type instance XFixSig           (GhcPass p) = EpAnn [AddEpAnn]
type instance XInlineSig        (GhcPass p) = EpAnn [AddEpAnn]
type instance XSpecSig          (GhcPass p) = EpAnn [AddEpAnn]
type instance XSpecInstSig      (GhcPass p) = EpAnn [AddEpAnn]
type instance XMinimalSig       (GhcPass p) = EpAnn [AddEpAnn]
type instance XSCCFunSig        (GhcPass p) = EpAnn [AddEpAnn]
type instance XCompleteMatchSig (GhcPass p) = EpAnn [AddEpAnn]

type instance XXSig             (GhcPass p) = DataConCantHappen

type instance XFixitySig  (GhcPass p) = NoExtField
type instance XXFixitySig (GhcPass p) = DataConCantHappen

data AnnSig
  = AnnSig {
      asDcolon :: AddEpAnn, -- Not an EpaAnchor to capture unicode option
      asRest   :: [AddEpAnn]
      } deriving Data


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

instance OutputableBndrId p => Outputable (Sig (GhcPass p)) where
    ppr sig = ppr_sig sig

ppr_sig :: forall p. OutputableBndrId p
        => Sig (GhcPass p) -> SDoc
ppr_sig (TypeSig _ vars ty)  = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (ClassOpSig _ is_deflt vars ty)
  | is_deflt                 = text "default" <+> pprVarSig (map unLoc vars) (ppr ty)
  | otherwise                = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (IdSig _ id)         = pprVarSig [id] (ppr (varType id))
ppr_sig (FixSig _ fix_sig)   = ppr fix_sig
ppr_sig (SpecSig _ var ty inl@(InlinePragma { inl_inline = spec }))
  = pragSrcBrackets (inlinePragmaSource inl) pragmaSrc (pprSpec (unLoc var)
                                             (interpp'SP ty) inl)
    where
      pragmaSrc = case spec of
        NoUserInlinePrag -> "{-# " ++ extractSpecPragName (inl_src inl)
        _                -> "{-# " ++ extractSpecPragName (inl_src inl)  ++ "_INLINE"
ppr_sig (InlineSig _ var inl)
  = pragSrcBrackets (inlinePragmaSource inl) "{-# INLINE"  (pprInline inl
                                   <+> pprPrefixOcc (unLoc var))
ppr_sig (SpecInstSig _ src ty)
  = pragSrcBrackets src "{-# pragma" (text "instance" <+> ppr ty)
ppr_sig (MinimalSig _ src bf)
  = pragSrcBrackets src "{-# MINIMAL" (pprMinimalSig bf)
ppr_sig (PatSynSig _ names sig_ty)
  = text "pattern" <+> pprVarSig (map unLoc names) (ppr sig_ty)
ppr_sig (SCCFunSig _ src fn mlabel)
  = pragSrcBrackets src "{-# SCC" (ppr_fn <+> maybe empty ppr mlabel )
      where
        ppr_fn = case ghcPass @p of
          GhcPs -> ppr fn
          GhcRn -> ppr fn
          GhcTc -> ppr fn
ppr_sig (CompleteMatchSig _ src cs mty)
  = pragSrcBrackets src "{-# COMPLETE"
      ((hsep (punctuate comma (map ppr_n (unLoc cs))))
        <+> opt_sig)
  where
    opt_sig = maybe empty ((\t -> dcolon <+> ppr t) . unLoc) mty
    ppr_n n = case ghcPass @p of
        GhcPs -> ppr n
        GhcRn -> ppr n
        GhcTc -> ppr n

instance OutputableBndrId p
       => Outputable (FixitySig (GhcPass p)) where
  ppr (FixitySig _ names fixity) = sep [ppr fixity, pprops]
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
    = ppr (extractSpecPragName $ inl_src inl) <+> pprSpec var (text "<type>") inl

pprMinimalSig :: (OutputableBndr name)
              => LBooleanFormula (GenLocated l name) -> SDoc
pprMinimalSig (L _ bf) = ppr (fmap unLoc bf)

{-
************************************************************************
*                                                                      *
\subsection{Anno instances}
*                                                                      *
************************************************************************
-}

type instance Anno (HsBindLR (GhcPass idL) (GhcPass idR)) = SrcSpanAnnA
type instance Anno (IPBind (GhcPass p)) = SrcSpanAnnA
type instance Anno (Sig (GhcPass p)) = SrcSpanAnnA

-- For CompleteMatchSig
type instance Anno [LocatedN RdrName] = SrcSpan
type instance Anno [LocatedN Name]    = SrcSpan
type instance Anno [LocatedN Id]      = SrcSpan

type instance Anno (FixitySig (GhcPass p)) = SrcSpanAnnA

type instance Anno StringLiteral = SrcAnn NoEpAnns
type instance Anno (LocatedN RdrName) = SrcSpan
type instance Anno (LocatedN Name) = SrcSpan
type instance Anno (LocatedN Id) = SrcSpan
