module GHC.Tc.Types.BasicTypes (
  -- * TcBinder
    TcBinderStack
  , TcId
  , TcBinder(..)

  -- * Signatures
  , TcSigFun, TcSigInfo(..), TcIdSig(..)
  , TcCompleteSig(..), TcPartialSig(..), TcPatSynSig(..)
  , TcIdSigInst(..)
  , isPartialSig, hasCompleteSig
  , tcSigInfoName, tcIdSigLoc, completeSigPolyId_maybe

  -- * TcTyThing
  , TcTyThing(..)
  , IdBindingInfo(..)
  , IsGroupClosed(..)
  , RhsNames
  , ClosedTypeId
  , tcTyThingCategory
  , tcTyThingTyCon_maybe
  , pprTcTyThingCategory
  ) where

import GHC.Prelude

import GHC.Tc.Types.Origin( UserTypeCtxt )
import GHC.Tc.Utils.TcType

import GHC.Types.Id
import GHC.Types.Basic
import GHC.Types.Var
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.Name.Env
import GHC.Types.Name.Set

import GHC.Hs.Extension ( GhcRn )

import Language.Haskell.Syntax.Type ( LHsSigWcType )

import GHC.Tc.Errors.Types.PromotionErr (PromotionErr, peCategory)

import GHC.Core.TyCon  ( TyCon, tyConKind )
import GHC.Utils.Outputable
import GHC.Utils.Misc


---------------------------
-- The TcBinderStack
---------------------------

type TcBinderStack = [TcBinder]
type TcId = Id
   -- This is a stack of locally-bound ids and tyvars,
   --   innermost on top
   -- Used only in error reporting (relevantBindings in TcError),
   --   and in tidying
   -- We can't use the tcl_env type environment, because it doesn't
   --   keep track of the nesting order

data TcBinder
  = TcIdBndr
       TcId
       TopLevelFlag    -- Tells whether the binding is syntactically top-level
                       -- (The monomorphic Ids for a recursive group count
                       --  as not-top-level for this purpose.)

  | TcIdBndr_ExpType  -- Variant that allows the type to be specified as
                      -- an ExpType
       Name
       ExpType
       TopLevelFlag

  | TcTvBndr          -- e.g.   case x of P (y::a) -> blah
       Name           -- We bind the lexical name "a" to the type of y,
       TyVar          -- which might be an utterly different (perhaps
                      -- existential) tyvar

instance Outputable TcBinder where
   ppr (TcIdBndr id top_lvl)           = ppr id <> brackets (ppr top_lvl)
   ppr (TcIdBndr_ExpType id _ top_lvl) = ppr id <> brackets (ppr top_lvl)
   ppr (TcTvBndr name tv)              = ppr name <+> ppr tv

instance HasOccName TcBinder where
    occName (TcIdBndr id _)             = occName (idName id)
    occName (TcIdBndr_ExpType name _ _) = occName name
    occName (TcTvBndr name _)           = occName name

{- *********************************************************************
*                                                                      *
                Type signatures
*                                                                      *
********************************************************************* -}

-- These data types need to be here only because
-- GHC.Tc.Solver uses them, and GHC.Tc.Solver is fairly
-- low down in the module hierarchy

type TcSigFun  = Name -> Maybe TcSigInfo

-- TcSigInfo is simply the range of TcSigFun
data TcSigInfo = TcIdSig     TcIdSig
               | TcPatSynSig TcPatSynSig    -- For a pattern synonym

-- See Note [Complete and partial type signatures]
data TcIdSig  -- For an Id
  = TcCompleteSig TcCompleteSig
  | TcPartialSig  TcPartialSig

data TcCompleteSig  -- A complete signature with no wildcards,
                    -- so the complete polymorphic type is known.
  = CSig { sig_bndr :: TcId          -- The polymorphic Id with that type

         , sig_ctxt :: UserTypeCtxt  -- In the case of type-class default methods,
                                     -- the Name in the FunSigCtxt is not the same
                                     -- as the TcId; the former is 'op', while the
                                     -- latter is '$dmop' or some such

         , sig_loc  :: SrcSpan       -- Location of the type signature
         }

data TcPartialSig  -- A partial type signature (i.e. includes one or more
                   -- wildcards). In this case it doesn't make sense to give
                   -- the polymorphic Id, because we are going to /infer/ its
                   -- type, so we can't make the polymorphic Id ab-initio
  = PSig { psig_name  :: Name   -- Name of the function; used when report wildcards
         , psig_hs_ty :: LHsSigWcType GhcRn  -- The original partial signature in
                                             -- HsSyn form
         , psig_ctxt  :: UserTypeCtxt
         , psig_loc   :: SrcSpan            -- Location of the type signature
         }

data TcPatSynSig
  = PatSig {
        patsig_name           :: Name,
        patsig_implicit_bndrs :: [InvisTVBinder], -- Implicitly-bound kind vars (Inferred) and
                                                  -- implicitly-bound type vars (Specified)
          -- See Note [The pattern-synonym signature splitting rule] in GHC.Tc.TyCl.PatSyn
        patsig_univ_bndrs     :: [InvisTVBinder], -- Bound by explicit user forall
        patsig_req            :: TcThetaType,
        patsig_ex_bndrs       :: [InvisTVBinder], -- Bound by explicit user forall
        patsig_prov           :: TcThetaType,
        patsig_body_ty        :: TcSigmaType
    }

{- Note [Complete and partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature is partial when it contains one or more wildcards
(= type holes).  The wildcard can either be:
* A (type) wildcard occurring in sig_theta or sig_tau. These are
  stored in sig_wcs.
      f :: Bool -> _
      g :: Eq _a => _a -> _a -> Bool
* Or an extra-constraints wildcard, stored in sig_cts:
      h :: (Num a, _) => a -> a

A type signature is a complete type signature when there are no
wildcards in the type signature, i.e. iff sig_wcs is empty and
sig_extra_cts is Nothing.
-}

data TcIdSigInst
  = TISI { sig_inst_sig :: TcIdSig

         , sig_inst_skols :: [(Name, InvisTVBinder)]
               -- Instantiated type and kind variables, TyVarTvs
               -- The Name is the Name that the renamer chose;
               --   but the TcTyVar may come from instantiating
               --   the type and hence have a different unique.
               -- No need to keep track of whether they are truly lexically
               --   scoped because the renamer has named them uniquely
               -- See Note [Binding scoped type variables] in GHC.Tc.Gen.Sig
               --
               -- NB: The order of sig_inst_skols is irrelevant
               --     for a CompleteSig, but for a PartialSig see
               --     Note [Quantified variables in partial type signatures]

         , sig_inst_theta  :: TcThetaType
               -- Instantiated theta.  In the case of a
               -- PartialSig, sig_theta does not include
               -- the extra-constraints wildcard

         , sig_inst_tau :: TcSigmaType   -- Instantiated tau
               -- See Note [sig_inst_tau may be polymorphic]

         -- Relevant for partial signature only
         , sig_inst_wcs   :: [(Name, TcTyVar)]
               -- Like sig_inst_skols, but for /named/ wildcards (_a etc).
               -- The named wildcards scope over the binding, and hence
               -- their Names may appear in type signatures in the binding

         , sig_inst_wcx   :: Maybe TcType
               -- Extra-constraints wildcard to fill in, if any
               -- If this exists, it is surely of the form (meta_tv |> co)
               -- (where the co might be reflexive). This is filled in
               -- only from the return value of GHC.Tc.Gen.HsType.tcAnonWildCardOcc
         }

{- Note [sig_inst_tau may be polymorphic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that "sig_inst_tau" might actually be a polymorphic type,
if the original function had a signature like
   forall a. Eq a => forall b. Ord b => ....
But that's ok: tcFunBindMatches (called by tcRhs) can deal with that
It happens, too!  See Note [Polymorphic methods] in GHC.Tc.TyCl.Class.

Note [Quantified variables in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall a b. _ -> a -> _ -> b
   f (x,y) p q = q

Then we expect f's final type to be
  f :: forall {x,y}. forall a b. (x,y) -> a -> b -> b

Note that x,y are Inferred, and can't be use for visible type
application (VTA).  But a,b are Specified, and remain Specified
in the final type, so we can use VTA for them.  (Exception: if
it turns out that a's kind mentions b we need to reorder them
with scopedSort.)

The sig_inst_skols of the TISI from a partial signature records
that original order, and is used to get the variables of f's
final type in the correct order.


Note [Wildcards in partial signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wildcards in psig_wcs may stand for a type mentioning
the universally-quantified tyvars of psig_ty

E.g.  f :: forall a. _ -> a
      f x = x
We get sig_inst_skols = [a]
       sig_inst_tau   = _22 -> a
       sig_inst_wcs   = [_22]
and _22 in the end is unified with the type 'a'

Moreover the kind of a wildcard in sig_inst_wcs may mention
the universally-quantified tyvars sig_inst_skols
e.g.   f :: t a -> t _
Here we get
   sig_inst_skols = [k:*, (t::k ->*), (a::k)]
   sig_inst_tau   = t a -> t _22
   sig_inst_wcs   = [ _22::k ]
-}

instance Outputable TcSigInfo where
  ppr (TcIdSig sig)     = ppr sig
  ppr (TcPatSynSig sig) = ppr sig

instance Outputable TcIdSig where
  ppr (TcCompleteSig sig) = ppr sig
  ppr (TcPartialSig sig)  = ppr sig

instance Outputable TcCompleteSig where
  ppr (CSig { sig_bndr = bndr })
      = ppr bndr <+> dcolon <+> ppr (idType bndr)

instance Outputable TcPartialSig where
  ppr (PSig { psig_name = name, psig_hs_ty = hs_ty })
      = text "[partial signature]" <+> ppr name <+> dcolon <+> ppr hs_ty

instance Outputable TcPatSynSig where
  ppr (PatSig { patsig_name = name}) = ppr name

instance Outputable TcIdSigInst where
  ppr (TISI { sig_inst_sig = sig, sig_inst_skols = skols
            , sig_inst_theta = theta, sig_inst_tau = tau })
      = hang (ppr sig) 2 (vcat [ ppr skols, ppr theta <+> darrow <+> ppr tau ])

isPartialSig :: TcIdSigInst -> Bool
isPartialSig (TISI { sig_inst_sig = TcPartialSig {} }) = True
isPartialSig _                                         = False

-- | No signature or a partial signature
hasCompleteSig :: TcSigFun -> Name -> Bool
hasCompleteSig sig_fn name
  = case sig_fn name of
      Just (TcIdSig (TcCompleteSig {})) -> True
      _                                 -> False

tcSigInfoName :: TcSigInfo -> Name
tcSigInfoName (TcIdSig (TcCompleteSig sig)) = idName (sig_bndr sig)
tcSigInfoName (TcIdSig (TcPartialSig  sig)) = psig_name sig
tcSigInfoName (TcPatSynSig sig)             = patsig_name sig

tcIdSigLoc :: TcIdSig -> SrcSpan
tcIdSigLoc (TcCompleteSig sig) = sig_loc sig
tcIdSigLoc (TcPartialSig  sig) = psig_loc sig

completeSigPolyId_maybe :: TcSigInfo -> Maybe TcId
completeSigPolyId_maybe (TcIdSig (TcCompleteSig sig)) = Just (sig_bndr sig)
completeSigPolyId_maybe _                             = Nothing

{- *********************************************************************
*                                                                      *
             TcTyThing
*                                                                      *
********************************************************************* -}

-- | A typecheckable thing available in a local context.  Could be
-- 'AGlobal' 'TyThing', but also lexically scoped variables, etc.
-- See "GHC.Tc.Utils.Env" for how to retrieve a 'TyThing' given a 'Name'.
data TcTyThing
  = AGlobal TyThing             -- Used only in the return type of a lookup

  | ATcId           -- Ids defined in this module; may not be fully zonked
      { tct_id   :: Id
      , tct_info :: IdBindingInfo   -- See Note [Meaning of IdBindingInfo]
      }

  | ATyVar  Name TcTyVar   -- See Note [Type variables in the type environment]

  | ATcTyCon TyCon   -- Used temporarily, during kind checking, for the
                     -- tycons and classes in this recursive group
                     -- The TyCon is always a TcTyCon.  Its kind
                     -- can be a mono-kind or a poly-kind; in TcTyClsDcls see
                     -- Note [Type checking recursive type and class declarations]

  | APromotionErr PromotionErr

-- | Matches on either a global 'TyCon' or a 'TcTyCon'.
tcTyThingTyCon_maybe :: TcTyThing -> Maybe TyCon
tcTyThingTyCon_maybe (AGlobal (ATyCon tc)) = Just tc
tcTyThingTyCon_maybe (ATcTyCon tc_tc)      = Just tc_tc
tcTyThingTyCon_maybe _                     = Nothing

instance Outputable TcTyThing where     -- Debugging only
   ppr (AGlobal g)      = ppr g
   ppr elt@(ATcId {})   = text "Identifier" <>
                          brackets (ppr (tct_id elt) <> dcolon
                                 <> ppr (varType (tct_id elt)) <> comma
                                 <+> ppr (tct_info elt))
   ppr (ATyVar n tv)    = text "Type variable" <+> quotes (ppr n) <+> equals <+> ppr tv
                            <+> dcolon <+> ppr (varType tv)
   ppr (ATcTyCon tc)    = text "ATcTyCon" <+> ppr tc <+> dcolon <+> ppr (tyConKind tc)
   ppr (APromotionErr err) = text "APromotionErr" <+> ppr err

-- | IdBindingInfo describes how an Id is bound.
--
-- It is used for the following purposes:
-- a) for static forms in 'GHC.Tc.Gen.Expr.checkClosedInStaticForm' and
-- b) to figure out when a nested binding can be generalised,
--    in 'GHC.Tc.Gen.Bind.decideGeneralisationPlan'.
--
data IdBindingInfo -- See Note [Meaning of IdBindingInfo]
    = NotLetBound
    | ClosedLet
    | NonClosedLet
         RhsNames        -- Used for (static e) checks only
         ClosedTypeId    -- Used for generalisation checks
                         -- and for (static e) checks

-- | IsGroupClosed describes a group of mutually-recursive bindings
data IsGroupClosed
  = IsGroupClosed
      (NameEnv RhsNames)  -- Free var info for the RHS of each binding in the group
                          -- Used only for (static e) checks

      ClosedTypeId        -- True <=> all the free vars of the group are
                          --          imported or ClosedLet or
                          --          NonClosedLet with ClosedTypeId=True.
                          --          In particular, no tyvars, no NotLetBound

type RhsNames = NameSet   -- Names of variables, mentioned on the RHS of
                          -- a definition, that are not Global or ClosedLet

type ClosedTypeId = Bool
  -- See Note [Meaning of IdBindingInfo]

{- Note [Meaning of IdBindingInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NotLetBound means that
  the Id is not let-bound (e.g. it is bound in a
  lambda-abstraction or in a case pattern)

ClosedLet means that
   - The Id is let-bound,
   - Any free term variables are also Global or ClosedLet
   - Its type has no free variables (NB: a top-level binding subject
     to the MR might have free vars in its type)
   These ClosedLets can definitely be floated to top level; and we
   may need to do so for static forms.

   Property:   ClosedLet
             is equivalent to
               NonClosedLet emptyNameSet True

(NonClosedLet (fvs::RhsNames) (cl::ClosedTypeId)) means that
   - The Id is let-bound

   - The fvs::RhsNames contains the free names of the RHS,
     excluding Global and ClosedLet ones.

   - For the ClosedTypeId field see Note [Bindings with closed types: ClosedTypeId]

For (static e) to be valid, we need for every 'x' free in 'e',
that x's binding is floatable to the top level.  Specifically:
   * x's RhsNames must be empty
   * x's type has no free variables
See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
This test is made in GHC.Tc.Gen.Expr.checkClosedInStaticForm.
Actually knowing x's RhsNames (rather than just its emptiness
or otherwise) is just so we can produce better error messages

Note [Bindings with closed types: ClosedTypeId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f x = let g ys = map not ys
        in ...

Can we generalise 'g' under the OutsideIn algorithm?  Yes,
because all g's free variables are top-level; that is they themselves
have no free type variables, and it is the type variables in the
environment that makes things tricky for OutsideIn generalisation.

Here's the invariant:
   If an Id has ClosedTypeId=True (in its IdBindingInfo), then
   the Id's type is /definitely/ closed (has no free type variables).
   Specifically,
       a) The Id's actual type is closed (has no free tyvars)
       b) Either the Id has a (closed) user-supplied type signature
          or all its free variables are Global/ClosedLet
             or NonClosedLet with ClosedTypeId=True.
          In particular, none are NotLetBound.

Why is (b) needed?   Consider
    \x. (x :: Int, let y = x+1 in ...)
Initially x::alpha.  If we happen to typecheck the 'let' before the
(x::Int), y's type will have a free tyvar; but if the other way round
it won't.  So we treat any let-bound variable with a free
non-let-bound variable as not ClosedTypeId, regardless of what the
free vars of its type actually are.

But if it has a signature, all is well:
   \x. ...(let { y::Int; y = x+1 } in
           let { v = y+2 } in ...)...
Here the signature on 'v' makes 'y' a ClosedTypeId, so we can
generalise 'v'.

Note that:

  * A top-level binding may not have ClosedTypeId=True, if it suffers
    from the MR

  * A nested binding may be closed (eg 'g' in the example we started
    with). Indeed, that's the point; whether a function is defined at
    top level or nested is orthogonal to the question of whether or
    not it is closed.

  * A binding may be non-closed because it mentions a lexically scoped
    *type variable*  Eg
        f :: forall a. blah
        f x = let g y = ...(y::a)...

Under OutsideIn we are free to generalise an Id all of whose free
variables have ClosedTypeId=True (or imported).  This is an extension
compared to the JFP paper on OutsideIn, which used "top-level" as a
proxy for "closed".  (It's not a good proxy anyway -- the MR can make
a top-level binding with a free type variable.)

Note [Type variables in the type environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type environment has a binding for each lexically-scoped
type variable that is in scope.  For example

  f :: forall a. a -> a
  f x = (x :: a)

  g1 :: [a] -> a
  g1 (ys :: [b]) = head ys :: b

  g2 :: [Int] -> Int
  g2 (ys :: [c]) = head ys :: c

* The forall'd variable 'a' in the signature scopes over f's RHS.

* The pattern-bound type variable 'b' in 'g1' scopes over g1's
  RHS; note that it is bound to a skolem 'a' which is not itself
  lexically in scope.

* The pattern-bound type variable 'c' in 'g2' is bound to
  Int; that is, pattern-bound type variables can stand for
  arbitrary types. (see
    GHC proposal #128 "Allow ScopedTypeVariables to refer to types"
    https://github.com/ghc-proposals/ghc-proposals/pull/128,
  and the paper
    "Type variables in patterns", Haskell Symposium 2018.


This is implemented by the constructor
   ATyVar Name TcTyVar
in the type environment.

* The Name is the name of the original, lexically scoped type
  variable

* The TcTyVar is sometimes a skolem (like in 'f'), and sometimes
  a unification variable (like in 'g1', 'g2').  We never zonk the
  type environment so in the latter case it always stays as a
  unification variable, although that variable may be later
  unified with a type (such as Int in 'g2').
-}

instance Outputable IdBindingInfo where
  ppr NotLetBound = text "NotLetBound"
  ppr ClosedLet = text "TopLevelLet"
  ppr (NonClosedLet fvs closed_type) =
    text "TopLevelLet" <+> ppr fvs <+> ppr closed_type

--------------
pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory = text . capitalise . tcTyThingCategory

tcTyThingCategory :: TcTyThing -> String
tcTyThingCategory (AGlobal thing)    = tyThingCategory thing
tcTyThingCategory (ATyVar {})        = "type variable"
tcTyThingCategory (ATcId {})         = "local identifier"
tcTyThingCategory (ATcTyCon {})      = "local tycon"
tcTyThingCategory (APromotionErr pe) = peCategory pe
