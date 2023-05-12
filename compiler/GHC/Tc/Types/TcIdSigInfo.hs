module GHC.Tc.Types.TcIdSigInfo where

import GHC.Prelude


import GHC.Hs

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Origin


import GHC.Types.Id         ( idType )
import GHC.Types.Name
import GHC.Types.Var
import GHC.Types.SrcLoc



import GHC.Utils.Outputable






{- *********************************************************************
*                                                                      *
                Type signatures
*                                                                      *
********************************************************************* -}

-- These data types need to be here only because
-- GHC.Tc.Solver uses them, and GHC.Tc.Solver is fairly
-- low down in the module hierarchy

type TcSigFun  = Name -> Maybe TcSigInfo

data TcSigInfo = TcIdSig     TcIdSigInfo
               | TcPatSynSig TcPatSynInfo

data TcIdSigInfo   -- See Note [Complete and partial type signatures]
  = CompleteSig    -- A complete signature with no wildcards,
                   -- so the complete polymorphic type is known.
      { sig_bndr :: Id          -- The polymorphic Id with that type

      , sig_ctxt :: UserTypeCtxt  -- In the case of type-class default methods,
                                  -- the Name in the FunSigCtxt is not the same
                                  -- as the TcId; the former is 'op', while the
                                  -- latter is '$dmop' or some such

      , sig_loc  :: SrcSpan       -- Location of the type signature
      }

  | PartialSig     -- A partial type signature (i.e. includes one or more
                   -- wildcards). In this case it doesn't make sense to give
                   -- the polymorphic Id, because we are going to /infer/ its
                   -- type, so we can't make the polymorphic Id ab-initio
      { psig_name  :: Name   -- Name of the function; used when report wildcards
      , psig_hs_ty :: LHsSigWcType GhcRn  -- The original partial signature in
                                          -- HsSyn form
      , sig_ctxt   :: UserTypeCtxt
      , sig_loc    :: SrcSpan            -- Location of the type signature
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
  = TISI { sig_inst_sig :: TcIdSigInfo

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
But that's ok: tcMatchesFun (called by tcRhs) can deal with that
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

data TcPatSynInfo
  = TPSI {
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

instance Outputable TcSigInfo where
  ppr (TcIdSig     idsi) = ppr idsi
  ppr (TcPatSynSig tpsi) = text "TcPatSynInfo" <+> ppr tpsi

instance Outputable TcIdSigInfo where
    ppr (CompleteSig { sig_bndr = bndr })
        = ppr bndr <+> dcolon <+> ppr (idType bndr)
    ppr (PartialSig { psig_name = name, psig_hs_ty = hs_ty })
        = text "psig" <+> ppr name <+> dcolon <+> ppr hs_ty

instance Outputable TcIdSigInst where
    ppr (TISI { sig_inst_sig = sig, sig_inst_skols = skols
              , sig_inst_theta = theta, sig_inst_tau = tau })
        = hang (ppr sig) 2 (vcat [ ppr skols, ppr theta <+> darrow <+> ppr tau ])

instance Outputable TcPatSynInfo where
    ppr (TPSI{ patsig_name = name}) = ppr name

isPartialSig :: TcIdSigInst -> Bool
isPartialSig (TISI { sig_inst_sig = PartialSig {} }) = True
isPartialSig _                                       = False

-- | No signature or a partial signature
hasCompleteSig :: TcSigFun -> Name -> Bool
hasCompleteSig sig_fn name
  = case sig_fn name of
      Just (TcIdSig (CompleteSig {})) -> True
      _                               -> False

