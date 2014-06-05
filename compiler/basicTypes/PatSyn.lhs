%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[PatSyn]{@PatSyn@: Pattern synonyms}

\begin{code}

module PatSyn (
        -- * Main data types
        PatSyn, mkPatSyn,

        -- ** Type deconstruction
        patSynId, patSynType, patSynArity, patSynIsInfix,
        patSynArgs, patSynTyDetails,
        patSynWrapper, patSynMatcher,
        patSynExTyVars, patSynSig, 
        patSynInstArgTys, patSynInstResTy
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


Note [Pattern synonym representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  psArgs       = [b]
  psArity      = 1
  psInfix      = False

  psUnivTyVars = [t]
  psExTyVars   = [b]
  psProvTheta  = (Show (Maybe t), Ord b)
  psReqTheta   = (Eq t, Num t)
  psOrigResTy  = T (Maybe t)

Note [Matchers and wrappers for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For each pattern synonym, we generate a single matcher function which
implements the actual matching. For the above example, the matcher
will have type:

        $mP :: forall r t. (Eq t, Num t)
            => T (Maybe t)
            -> (forall b. (Show (Maybe t), Ord b) => b -> r)
            -> r
            -> r

with the following implementation:

        $mP @r @t $dEq $dNum scrut cont fail = case scrut of
            MkT @b $dShow $dOrd [x] (Just 42) -> cont @b $dShow $dOrd x
            _                                 -> fail

For *bidirectional* pattern synonyms, we also generate a single wrapper
function which implements the pattern synonym in an expression
context. For our running example, it will be:

        $WP :: forall t b. (Show (Maybe t), Ord b, Eq t, Num t)
            => b -> T (Maybe t)
        $WP x = MkT [x] (Just 42)

NB: the existential/universal and required/provided split does not
apply to the wrapper since you are only putting stuff in, not getting
stuff out.

Injectivity of bidirectional pattern synonyms is checked in
tcPatToExpr which walks the pattern and returns its corresponding
expression when available.

%************************************************************************
%*                                                                      *
\subsection{Pattern synonyms}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A pattern synonym
-- See Note [Pattern synonym representation]
data PatSyn
  = MkPatSyn {
        psId          :: Id,
        psUnique      :: Unique,      -- Cached from Name

        psArgs        :: [Type],
        psArity       :: Arity,       -- == length psArgs
        psInfix       :: Bool,        -- True <=> declared infix

        psUnivTyVars  :: [TyVar],     -- Universially-quantified type variables
        psExTyVars    :: [TyVar],     -- Existentially-quantified type vars
        psProvTheta   :: ThetaType,   -- Provided dictionaries
        psReqTheta    :: ThetaType,   -- Required dictionaries
        psOrigResTy   :: Type,        -- Mentions only psUnivTyVars

        -- See Note [Matchers and wrappers for pattern synonyms]
        psMatcher     :: Id,
             -- Matcher function, of type
             --   forall r univ_tvs. req_theta
             --                   => res_ty
             --                   -> (forall ex_tvs. prov_theta -> arg_tys -> r)
             --                   -> r -> r

        psWrapper     :: Maybe Id
             -- Nothing  => uni-directional pattern synonym
             -- Just wid => bi-direcitonal
             -- Wrapper function, of type
             --  forall univ_tvs, ex_tvs. (prov_theta, req_theta)
             --                       =>  arg_tys -> res_ty
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
         -> [Type]     -- ^ Original arguments
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
                psProvTheta = prov_theta, psReqTheta = req_theta,
                psInfix = declared_infix,
                psArgs = orig_args,
                psArity = length orig_args,
                psOrigResTy = orig_res_ty,
                psMatcher = matcher,
                psWrapper = wrapper }
  where
    pat_ty = mkSigmaTy univ_tvs req_theta $
             mkSigmaTy ex_tvs prov_theta $
             mkFunTys orig_args orig_res_ty
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

patSynArgs :: PatSyn -> [Type]
patSynArgs = psArgs

patSynTyDetails :: PatSyn -> HsPatSynDetails Type
patSynTyDetails ps = case (patSynIsInfix ps, patSynArgs ps) of
    (True, [left, right]) -> InfixPatSyn left right
    (_, tys) -> PrefixPatSyn tys

patSynExTyVars :: PatSyn -> [TyVar]
patSynExTyVars = psExTyVars

patSynSig :: PatSyn -> ([TyVar], [TyVar], ThetaType, ThetaType)
patSynSig (MkPatSyn { psUnivTyVars = univ_tvs, psExTyVars = ex_tvs
                    , psProvTheta = prov, psReqTheta = req })
  = (univ_tvs, ex_tvs, prov, req)

patSynWrapper :: PatSyn -> Maybe Id
patSynWrapper = psWrapper

patSynMatcher :: PatSyn -> Id
patSynMatcher = psMatcher

patSynInstArgTys :: PatSyn -> [Type] -> [Type]
-- Return the types of the argument patterns
-- e.g.  data D a = forall b. MkD a b (b->a)
--       pattern P f x y = MkD (x,True) y f
--          D :: forall a. forall b. a -> b -> (b->a) -> D a
--          P :: forall c. forall b. (b->(c,Bool)) -> c -> b -> P c
--   patSynInstArgTys P [Int,bb] = [bb->(Int,Bool), Int, bb]
-- NB: the inst_tys should be both universal and existential
patSynInstArgTys ps inst_tys
  = ASSERT2( length tyvars == length inst_tys
          , ptext (sLit "patSynInstArgTys") <+> ppr ps $$ ppr tyvars $$ ppr inst_tys )
    map (substTyWith tyvars inst_tys) (psArgs ps)
  where
    (univ_tvs, ex_tvs, _, _) = patSynSig ps
    tyvars = univ_tvs ++ ex_tvs

patSynInstResTy :: PatSyn -> [Type] -> Type
-- Return the type of whole pattern
-- E.g.  pattern P x y = Just (x,x,y)
--         P :: a -> b -> Just (a,a,b)
--         (patSynInstResTy P [Int,Bool] = Maybe (Int,Int,Bool)
-- NB: unlikepatSynInstArgTys, the inst_tys should be just the *universal* tyvars
patSynInstResTy ps inst_tys
  = ASSERT2( length univ_tvs == length inst_tys
           , ptext (sLit "patSynInstResTy") <+> ppr ps $$ ppr univ_tvs $$ ppr inst_tys )
    substTyWith univ_tvs inst_tys (psOrigResTy ps)
  where
    (univ_tvs, _, _, _) = patSynSig ps
\end{code}
