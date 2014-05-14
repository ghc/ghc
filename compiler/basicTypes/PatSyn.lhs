%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[PatSyn]{@PatSyn@: Pattern synonyms}

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

module PatSyn (
        -- * Main data types
        PatSyn, mkPatSyn,

        -- ** Type deconstruction
        patSynId, patSynType, patSynArity, patSynIsInfix,
        patSynArgs, patSynArgTys, patSynTyDetails,
        patSynWrapper, patSynMatcher,
        patSynExTyVars, patSynSig, patSynInstArgTys
    ) where

#include "HsVersions.h"

import Type
import Name
import Outputable
import Unique
import Util
import BasicTypes
import FastString
import Var
import Id
import TcType
import HsBinds( HsPatSynDetails(..) )

import qualified Data.Data as Data
import qualified Data.Typeable
import Data.Function
\end{code}


Pattern synonym representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following pattern synonym declaration

        pattern P x = MkT [x] (Just 42)

where
        data T a where
              MkT :: (Show a, Ord b) => [b] -> a -> T a

so pattern P has type

        b -> T (Maybe t)

with the following typeclass constraints:

        provides: (Show (Maybe t), Ord b)
        requires: (Eq t, Num t)

In this case, the fields of MkPatSyn will be set as follows:

  psArgs       = [x :: b]
  psArity      = 1
  psInfix      = False

  psUnivTyVars = [t]
  psExTyVars   = [b]
  psTheta      = ((Show (Maybe t), Ord b), (Eq t, Num t))
  psOrigResTy  = T (Maybe t)


%************************************************************************
%*                                                                      *
\subsection{Pattern synonyms}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A pattern synonym
data PatSyn
  = MkPatSyn {
        psId          :: Id,
        psUnique      :: Unique,                 -- Cached from Name
        psMatcher     :: Id,
        psWrapper     :: Maybe Id,

        psArgs        :: [Var],
        psArity       :: Arity,                  -- == length psArgs
        psInfix       :: Bool,                   -- True <=> declared infix

        psUnivTyVars  :: [TyVar],                -- Universially-quantified type variables
        psExTyVars    :: [TyVar],                -- Existentially-quantified type vars
        psTheta       :: (ThetaType, ThetaType), -- Provided and required dictionaries
        psOrigResTy   :: Type
  }
  deriving Data.Typeable.Typeable
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Instances}
%*                                                                      *
%************************************************************************

\begin{code}
instance Eq PatSyn where
    (==) = (==) `on` getUnique
    (/=) = (/=) `on` getUnique

instance Ord PatSyn where
    (<=) = (<=) `on` getUnique
    (<) = (<) `on` getUnique
    (>=) = (>=) `on` getUnique
    (>) = (>) `on` getUnique
    compare = compare `on` getUnique

instance Uniquable PatSyn where
    getUnique = psUnique

instance NamedThing PatSyn where
    getName = getName . psId

instance Outputable PatSyn where
    ppr = ppr . getName

instance OutputableBndr PatSyn where
    pprInfixOcc = pprInfixName . getName
    pprPrefixOcc = pprPrefixName . getName

instance Data.Data PatSyn where
    -- don't traverse?
    toConstr _   = abstractConstr "PatSyn"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "PatSyn"
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Construction}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Build a new pattern synonym
mkPatSyn :: Name
         -> Bool       -- ^ Is the pattern synonym declared infix?
         -> [Var]      -- ^ Original arguments
         -> [TyVar]    -- ^ Universially-quantified type variables
         -> [TyVar]    -- ^ Existentially-quantified type variables
         -> ThetaType  -- ^ Wanted dicts
         -> ThetaType  -- ^ Given dicts
         -> Type       -- ^ Original result type
         -> Id         -- ^ Name of matcher
         -> Maybe Id   -- ^ Name of wrapper
         -> PatSyn
mkPatSyn name declared_infix orig_args
         univ_tvs ex_tvs
         prov_theta req_theta
         orig_res_ty
         matcher wrapper
    = MkPatSyn {psId = id, psUnique = getUnique name,
                psUnivTyVars = univ_tvs, psExTyVars = ex_tvs,
                psTheta = (prov_theta, req_theta),
                psInfix = declared_infix,
                psArgs = orig_args,
                psArity = length orig_args,
                psOrigResTy = orig_res_ty,
                psMatcher = matcher,
                psWrapper = wrapper }
  where
    pat_ty = mkSigmaTy univ_tvs req_theta $
             mkSigmaTy ex_tvs prov_theta $
             mkFunTys (map varType orig_args) orig_res_ty
    id = mkLocalId name pat_ty
\end{code}

\begin{code}
-- | The 'Name' of the 'PatSyn', giving it a unique, rooted identification
patSynId :: PatSyn -> Id
patSynId = psId

patSynType :: PatSyn -> Type
patSynType = psOrigResTy

-- | Should the 'PatSyn' be presented infix?
patSynIsInfix :: PatSyn -> Bool
patSynIsInfix = psInfix

-- | Arity of the pattern synonym
patSynArity :: PatSyn -> Arity
patSynArity = psArity

patSynArgs :: PatSyn -> [Var]
patSynArgs = psArgs

patSynArgTys :: PatSyn -> [Type]
patSynArgTys = map varType . patSynArgs

patSynTyDetails :: PatSyn -> HsPatSynDetails Type
patSynTyDetails ps = case (patSynIsInfix ps, patSynArgTys ps) of
    (True, [left, right]) -> InfixPatSyn left right
    (_, tys) -> PrefixPatSyn tys

patSynExTyVars :: PatSyn -> [TyVar]
patSynExTyVars = psExTyVars

patSynSig :: PatSyn -> ([TyVar], [TyVar], (ThetaType, ThetaType))
patSynSig ps = (psUnivTyVars ps, psExTyVars ps, psTheta ps)

patSynWrapper :: PatSyn -> Maybe Id
patSynWrapper = psWrapper

patSynMatcher :: PatSyn -> Id
patSynMatcher = psMatcher

patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynInstArgTys ps inst_tys
  = ASSERT2( length tyvars == length inst_tys
          , ptext (sLit "patSynInstArgTys") <+> ppr ps $$ ppr tyvars $$ ppr inst_tys )
    map (substTyWith tyvars inst_tys) arg_tys
  where
    (univ_tvs, ex_tvs, _) = patSynSig ps
    arg_tys = map varType (psArgs ps)
    tyvars = univ_tvs ++ ex_tvs
\end{code}
