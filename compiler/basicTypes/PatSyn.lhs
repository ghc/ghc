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
        patSynName, patSynArity, patSynIsInfix,
        patSynArgs, patSynTyDetails, patSynType,
        patSynMatcher,
        patSynWrapper, patSynWorker,
        patSynExTyVars, patSynSig,
        patSynInstArgTys, patSynInstResTy,
        tidyPatSynIds
    ) where

#include "HsVersions.h"

import Type
import TcType( mkSigmaTy )
import Name
import Outputable
import Unique
import Util
import BasicTypes
import FastString
import Var
import HsBinds( HsPatSynDetails(..) )

import qualified Data.Data as Data
import qualified Data.Typeable
import Data.Function
import Control.Arrow (second)
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

        $mP :: forall (r :: ?) t. (Eq t, Num t)
            => T (Maybe t)
            -> (forall b. (Show (Maybe t), Ord b) => b -> r)
            -> (Void# -> r)
            -> r

with the following implementation:

        $mP @r @t $dEq $dNum scrut cont fail = case scrut of
            MkT @b $dShow $dOrd [x] (Just 42) -> cont @b $dShow $dOrd x
            _                                 -> fail Void#

The extra Void# argument for the failure continuation is needed so that
it is lazy even when the result type is unboxed. For the same reason,
if the pattern has no arguments, an extra Void# argument is added
to the success continuation as well.

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

Note [Wrapper/worker for pattern synonyms with unboxed type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For bidirectional pattern synonyms that have no arguments and have
an unboxed type, we add an extra level of indirection, since $WP would
otherwise be a top-level declaration with an unboxed type. In this case,
a separate worker function is generated that has an extra Void# argument,
and the wrapper redirects to it via a compulsory unfolding (that just
applies it on Void#). Example:

        pattern P = 0#

        $WP :: Int#
        $WP unfolded to ($wP Void#)

        $wP :: Void# -> Int#
        $wP _ = 0#

To make things more uniform, we always store two `Id`s in `PatSyn` for
the wrapper and the worker, with the following behaviour:

  if `psWrapper` == Just (`wrapper`, `worker`), then

  * `wrapper` should always be used when compiling the pattern synonym
    in an expression context (and its type is as prescribed)
  * `worker` is always an `Id` with a binding that needs to be exported
    as part of the definition of the pattern synonym

If a separate worker is not needed (because the pattern synonym has arguments
or has a non-unboxed type), the two `Id`s are the same.

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
        psName        :: Name,
        psUnique      :: Unique,      -- Cached from Name

        psArgs        :: [Type],
        psArity       :: Arity,       -- == length psArgs
        psInfix       :: Bool,        -- True <=> declared infix

        psUnivTyVars  :: [TyVar],     -- Universially-quantified type variables
        psReqTheta    :: ThetaType,   -- Required dictionaries
        psExTyVars    :: [TyVar],     -- Existentially-quantified type vars
        psProvTheta   :: ThetaType,   -- Provided dictionaries
        psOrigResTy   :: Type,        -- Mentions only psUnivTyVars

        -- See Note [Matchers and wrappers for pattern synonyms]
        psMatcher     :: Id,
            -- Matcher function. If psArgs is empty, then it has type
             --   forall (r :: ?) univ_tvs. req_theta
             --                       => res_ty
             --                       -> (forall ex_tvs. prov_theta -> Void# -> r)
             --                       -> (Void# -> r)
             --                       -> r
             --
             -- Otherwise:
             --   forall (r :: ?) univ_tvs. req_theta
             --                       => res_ty
             --                       -> (forall ex_tvs. prov_theta -> arg_tys -> r)
             --                       -> (Void# -> r)
             --                       -> r

        psWrapper     :: Maybe (Id, Id)
             -- Nothing  => uni-directional pattern synonym
             -- Just (wrapper, worker) => bi-direcitonal
             -- Wrapper function, of type
             --  forall univ_tvs, ex_tvs. (prov_theta, req_theta)
             --                       =>  arg_tys -> res_ty
             --
             -- See Note [Wrapper/worker for pattern synonyms with unboxed type]
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
    getName = patSynName

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
         -> Bool                 -- ^ Is the pattern synonym declared infix?
         -> ([TyVar], ThetaType) -- ^ Universially-quantified type variables
                                --   and required dicts
         -> ([TyVar], ThetaType) -- ^ Existentially-quantified type variables
                                --   and provided dicts
         -> [Type]               -- ^ Original arguments
         -> Type                 -- ^ Original result type
         -> Id                   -- ^ Name of matcher
         -> Maybe (Id, Id)       -- ^ Name of wrapper/worker
         -> PatSyn
mkPatSyn name declared_infix
         (univ_tvs, req_theta)
         (ex_tvs, prov_theta)
         orig_args
         orig_res_ty
         matcher wrapper
    = MkPatSyn {psName = name, psUnique = getUnique name,
                psUnivTyVars = univ_tvs, psExTyVars = ex_tvs,
                psProvTheta = prov_theta, psReqTheta = req_theta,
                psInfix = declared_infix,
                psArgs = orig_args,
                psArity = length orig_args,
                psOrigResTy = orig_res_ty,
                psMatcher = matcher,
                psWrapper = wrapper }
\end{code}

\begin{code}
-- | The 'Name' of the 'PatSyn', giving it a unique, rooted identification
patSynName :: PatSyn -> Name
patSynName = psName

patSynType :: PatSyn -> Type
-- The full pattern type, used only in error messages
patSynType (MkPatSyn { psUnivTyVars = univ_tvs, psReqTheta = req_theta
                     , psExTyVars   = ex_tvs,   psProvTheta = prov_theta
                     , psArgs = orig_args, psOrigResTy = orig_res_ty })
  = mkSigmaTy univ_tvs req_theta $
    mkSigmaTy ex_tvs prov_theta $
    mkFunTys orig_args orig_res_ty

-- | Should the 'PatSyn' be presented infix?
patSynIsInfix :: PatSyn -> Bool
patSynIsInfix = psInfix

-- | Arity of the pattern synonym
patSynArity :: PatSyn -> Arity
patSynArity = psArity

patSynArgs :: PatSyn -> [Type]
patSynArgs = psArgs

patSynTyDetails :: PatSyn -> HsPatSynDetails Type
patSynTyDetails (MkPatSyn { psInfix = is_infix, psArgs = arg_tys })
  | is_infix, [left,right] <- arg_tys
  = InfixPatSyn left right
  | otherwise
  = PrefixPatSyn arg_tys

patSynExTyVars :: PatSyn -> [TyVar]
patSynExTyVars = psExTyVars

patSynSig :: PatSyn -> ([TyVar], [TyVar], ThetaType, ThetaType, [Type], Type)
patSynSig (MkPatSyn { psUnivTyVars = univ_tvs, psExTyVars = ex_tvs
                    , psProvTheta = prov, psReqTheta = req
                    , psArgs = arg_tys, psOrigResTy = res_ty })
  = (univ_tvs, ex_tvs, prov, req, arg_tys, res_ty)

patSynWrapper :: PatSyn -> Maybe Id
patSynWrapper = fmap fst . psWrapper

patSynWorker :: PatSyn -> Maybe Id
patSynWorker = fmap snd . psWrapper

patSynMatcher :: PatSyn -> Id
patSynMatcher = psMatcher

tidyPatSynIds :: (Id -> Id) -> PatSyn -> PatSyn
tidyPatSynIds tidy_fn ps@(MkPatSyn { psMatcher = match_id, psWrapper = mb_wrap_id })
  = ps { psMatcher = tidy_fn match_id, psWrapper = fmap (second tidy_fn) mb_wrap_id }

patSynInstArgTys :: PatSyn -> [Type] -> [Type]
-- Return the types of the argument patterns
-- e.g.  data D a = forall b. MkD a b (b->a)
--       pattern P f x y = MkD (x,True) y f
--          D :: forall a. forall b. a -> b -> (b->a) -> D a
--          P :: forall c. forall b. (b->(c,Bool)) -> c -> b -> P c
--   patSynInstArgTys P [Int,bb] = [bb->(Int,Bool), Int, bb]
-- NB: the inst_tys should be both universal and existential
patSynInstArgTys (MkPatSyn { psName = name, psUnivTyVars = univ_tvs
                           , psExTyVars = ex_tvs, psArgs = arg_tys })
                 inst_tys
  = ASSERT2( length tyvars == length inst_tys
          , ptext (sLit "patSynInstArgTys") <+> ppr name $$ ppr tyvars $$ ppr inst_tys )
    map (substTyWith tyvars inst_tys) arg_tys
  where
    tyvars = univ_tvs ++ ex_tvs

patSynInstResTy :: PatSyn -> [Type] -> Type
-- Return the type of whole pattern
-- E.g.  pattern P x y = Just (x,x,y)
--         P :: a -> b -> Just (a,a,b)
--         (patSynInstResTy P [Int,Bool] = Maybe (Int,Int,Bool)
-- NB: unlikepatSynInstArgTys, the inst_tys should be just the *universal* tyvars
patSynInstResTy (MkPatSyn { psName = name, psUnivTyVars = univ_tvs
                          , psOrigResTy = res_ty })
                inst_tys
  = ASSERT2( length univ_tvs == length inst_tys
           , ptext (sLit "patSynInstResTy") <+> ppr name $$ ppr univ_tvs $$ ppr inst_tys )
    substTyWith univ_tvs inst_tys res_ty
\end{code}
