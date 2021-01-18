{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[PatSyn]{@PatSyn@: Pattern synonyms}
-}

{-# LANGUAGE CPP #-}

module GHC.Core.PatSyn (
        -- * Main data types
        PatSyn, PatSynMatcher, PatSynBuilder, mkPatSyn,

        -- ** Type deconstruction
        patSynName, patSynArity, patSynIsInfix, patSynResultType,
        patSynArgs,
        patSynMatcher, patSynBuilder,
        patSynUnivTyVarBinders, patSynExTyVars, patSynExTyVarBinders,
        patSynSig, patSynSigBndr,
        patSynInstArgTys, patSynInstResTy, patSynFieldLabels,
        patSynFieldType,

        pprPatSynType
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.TyCo.Ppr
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Basic
import GHC.Types.Var
import GHC.Types.FieldLabel

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import qualified Data.Data as Data
import Data.Function
import Data.List (find)

{-
************************************************************************
*                                                                      *
\subsection{Pattern synonyms}
*                                                                      *
************************************************************************
-}

-- | Pattern Synonym
--
-- See Note [Pattern synonym representation]
-- See Note [Pattern synonym signature contexts]
data PatSyn
  = MkPatSyn {
        psName        :: Name,
        psUnique      :: Unique,       -- Cached from Name

        psArgs        :: [Type],
        psArity       :: Arity,        -- == length psArgs
        psInfix       :: Bool,         -- True <=> declared infix
        psFieldLabels :: [FieldLabel], -- List of fields for a
                                       -- record pattern synonym
                                       -- INVARIANT: either empty if no
                                       -- record pat syn or same length as
                                       -- psArgs

        -- Universally-quantified type variables
        psUnivTyVars  :: [InvisTVBinder],

        -- Required dictionaries (may mention psUnivTyVars)
        psReqTheta    :: ThetaType,

        -- Existentially-quantified type vars
        psExTyVars    :: [InvisTVBinder],

        -- Provided dictionaries (may mention psUnivTyVars or psExTyVars)
        psProvTheta   :: ThetaType,

        -- Result type
        psResultTy   :: Type,  -- Mentions only psUnivTyVars
                               -- See Note [Pattern synonym result type]

        -- See Note [Matchers and builders for pattern synonyms]
        -- See Note [Keep Ids out of PatSyn]
        psMatcher     :: PatSynMatcher,
        psBuilder     :: PatSynBuilder
  }

type PatSynMatcher = (Name, Type, Bool)
     -- Matcher function.
     -- If Bool is True then prov_theta and arg_tys are empty
     -- and type is
     --   forall (p :: RuntimeRep) (r :: TYPE p) univ_tvs.
     --                          req_theta
     --                       => res_ty
     --                       -> (forall ex_tvs. Void# -> r)
     --                       -> (Void# -> r)
     --                       -> r
     --
     -- Otherwise type is
     --   forall (p :: RuntimeRep) (r :: TYPE r) univ_tvs.
     --                          req_theta
     --                       => res_ty
     --                       -> (forall ex_tvs. prov_theta => arg_tys -> r)
     --                       -> (Void# -> r)
     --                       -> r

type PatSynBuilder = Maybe (Name, Type, Bool)
     -- Nothing  => uni-directional pattern synonym
     -- Just (builder, is_unlifted) => bi-directional
     -- Builder function, of type
     --  forall univ_tvs, ex_tvs. (req_theta, prov_theta)
     --                       =>  arg_tys -> res_ty
     -- See Note [Builder for pattern synonyms with unboxed type]

{- Note [Pattern synonym signature contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a pattern synonym signature we write
   pattern P :: req => prov => t1 -> ... tn -> res_ty

Note that the "required" context comes first, then the "provided"
context.  Moreover, the "required" context must not mention
existentially-bound type variables; that is, ones not mentioned in
res_ty.  See lots of discussion in #10928.

If there is no "provided" context, you can omit it; but you
can't omit the "required" part (unless you omit both).

Example 1:
      pattern P1 :: (Num a, Eq a) => b -> Maybe (a,b)
      pattern P1 x = Just (3,x)

  We require (Num a, Eq a) to match the 3; there is no provided
  context.

Example 2:
      data T2 where
        MkT2 :: (Num a, Eq a) => a -> a -> T2

      pattern P2 :: () => (Num a, Eq a) => a -> T2
      pattern P2 x = MkT2 3 x

  When we match against P2 we get a Num dictionary provided.
  We can use that to check the match against 3.

Example 3:
      pattern P3 :: Eq a => a -> b -> T3 b

   This signature is illegal because the (Eq a) is a required
   constraint, but it mentions the existentially-bound variable 'a'.
   You can see it's existential because it doesn't appear in the
   result type (T3 b).

Note [Pattern synonym result type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T a b = MkT b a

   pattern P :: a -> T [a] Bool
   pattern P x = MkT True [x]

P's psResultTy is (T a Bool), and it really only matches values of
type (T [a] Bool).  For example, this is ill-typed

   f :: T p q -> String
   f (P x) = "urk"

This is different to the situation with GADTs:

   data S a where
     MkS :: Int -> S Bool

Now MkS (and pattern synonyms coming from MkS) can match a
value of type (S a), not just (S Bool); we get type refinement.

That in turn means that if you have a pattern

   P x :: T [ty] Bool

it's not entirely straightforward to work out the instantiation of
P's universal tyvars. You have to /match/
  the type of the pattern, (T [ty] Bool)
against
  the psResultTy for the pattern synonym, T [a] Bool
to get the instantiation a := ty.

This is very unlike DataCons, where univ tyvars match 1-1 the
arguments of the TyCon.

Side note: I (SG) get the impression that instantiated return types should
generate a *required* constraint for pattern synonyms, rather than a *provided*
constraint like it's the case for GADTs. For example, I'd expect these
declarations to have identical semantics:

    pattern Just42 :: Maybe Int
    pattern Just42 = Just 42

    pattern Just'42 :: (a ~ Int) => Maybe a
    pattern Just'42 = Just 42

The latter generates the proper required constraint, the former does not.
Also rather different to GADTs is the fact that Just42 doesn't have any
universally quantified type variables, whereas Just'42 or MkS above has.

Note [Keep Ids out of PatSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We carefully arrange that PatSyn does not contain the Ids for the matcher
and builder.  We want PatSyn, like TyCon and DataCon, to be completely
immutable. But, the matcher and builder are relatively sophisticated
functions, and we want to get their final IdInfo in the same way as
any other Id, so we'd have to update the Ids in the PatSyn too.

Rather than try to tidy PatSyns (which is easy to forget and is a bit
tricky, see #19074), it seems cleaner to make them entirely immutable,
like TyCons and Classes.  To that end PatSynBuilder and PatSynMatcher
contain Names not Ids. Which, it turns out, is absolutely fine.

c.f. DefMethInfo in Class, which contains the Name, but not the Id,
of the default method.

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

        requires: (Eq t, Num t)
        provides: (Show (Maybe t), Ord b)

In this case, the fields of MkPatSyn will be set as follows:

  psArgs       = [b]
  psArity      = 1
  psInfix      = False

  psUnivTyVars = [t]
  psExTyVars   = [b]
  psProvTheta  = (Show (Maybe t), Ord b)
  psReqTheta   = (Eq t, Num t)
  psResultTy  = T (Maybe t)

Note [Matchers and builders for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For each pattern synonym P, we generate

  * a "matcher" function, used to desugar uses of P in patterns,
    which implements pattern matching

  * A "builder" function (for bidirectional pattern synonyms only),
    used to desugar uses of P in expressions, which constructs P-values.

For the above example, the matcher function has type:

        $mP :: forall (r :: ?) t. (Eq t, Num t)
            => T (Maybe t)
            -> (forall b. (Show (Maybe t), Ord b) => b -> r)
            -> (Void# -> r)
            -> r

with the following implementation:

        $mP @r @t $dEq $dNum scrut cont fail
          = case scrut of
              MkT @b $dShow $dOrd [x] (Just 42) -> cont @b $dShow $dOrd x
              _                                 -> fail Void#

Notice that the return type 'r' has an open kind, so that it can
be instantiated by an unboxed type; for example where we see
     f (P x) = 3#

The extra Void# argument for the failure continuation is needed so that
it is lazy even when the result type is unboxed.

For the same reason, if the pattern has no arguments, an extra Void#
argument is added to the success continuation as well.

For *bidirectional* pattern synonyms, we also generate a "builder"
function which implements the pattern synonym in an expression
context. For our running example, it will be:

        $bP :: forall t b. (Eq t, Num t, Show (Maybe t), Ord b)
            => b -> T (Maybe t)
        $bP x = MkT [x] (Just 42)

NB: the existential/universal and required/provided split does not
apply to the builder since you are only putting stuff in, not getting
stuff out.

Injectivity of bidirectional pattern synonyms is checked in
tcPatToExpr which walks the pattern and returns its corresponding
expression when available.

Note [Builder for pattern synonyms with unboxed type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For bidirectional pattern synonyms that have no arguments and have an
unboxed type, we add an extra Void# argument to the builder, else it
would be a top-level declaration with an unboxed type.

        pattern P = 0#

        $bP :: Void# -> Int#
        $bP _ = 0#

This means that when typechecking an occurrence of P in an expression,
we must remember that the builder has this void argument. This is
done by GHC.Tc.TyCl.PatSyn.patSynBuilderOcc.

Note [Pattern synonyms and the data type Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type of a pattern synonym is of the form (See Note
[Pattern synonym signatures] in GHC.Tc.Gen.Sig):

    forall univ_tvs. req => forall ex_tvs. prov => ...

We cannot in general represent this by a value of type Type:

 - if ex_tvs is empty, then req and prov cannot be distinguished from
   each other
 - if req is empty, then univ_tvs and ex_tvs cannot be distinguished
   from each other, and moreover, prov is seen as the "required" context
   (as it is the only context)


************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Eq PatSyn where
    (==) = (==) `on` getUnique
    (/=) = (/=) `on` getUnique

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

{-
************************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
************************************************************************
-}

-- | Build a new pattern synonym
mkPatSyn :: Name
         -> Bool                 -- ^ Is the pattern synonym declared infix?
         -> ([InvisTVBinder], ThetaType) -- ^ Universially-quantified type
                                         -- variables and required dicts
         -> ([InvisTVBinder], ThetaType) -- ^ Existentially-quantified type
                                         -- variables and provided dicts
         -> [Type]               -- ^ Original arguments
         -> Type                 -- ^ Original result type
         -> PatSynMatcher        -- ^ Matcher
         -> PatSynBuilder        -- ^ Builder
         -> [FieldLabel]         -- ^ Names of fields for
                                 --   a record pattern synonym
         -> PatSyn
 -- NB: The univ and ex vars are both in TyBinder form and TyVar form for
 -- convenience. All the TyBinders should be Named!
mkPatSyn name declared_infix
         (univ_tvs, req_theta)
         (ex_tvs, prov_theta)
         orig_args
         orig_res_ty
         matcher builder field_labels
    = MkPatSyn {psName = name, psUnique = getUnique name,
                psUnivTyVars = univ_tvs,
                psExTyVars = ex_tvs,
                psProvTheta = prov_theta, psReqTheta = req_theta,
                psInfix = declared_infix,
                psArgs = orig_args,
                psArity = length orig_args,
                psResultTy = orig_res_ty,
                psMatcher = matcher,
                psBuilder = builder,
                psFieldLabels = field_labels
                }

-- | The 'Name' of the 'PatSyn', giving it a unique, rooted identification
patSynName :: PatSyn -> Name
patSynName = psName

-- | Should the 'PatSyn' be presented infix?
patSynIsInfix :: PatSyn -> Bool
patSynIsInfix = psInfix

-- | Arity of the pattern synonym
patSynArity :: PatSyn -> Arity
patSynArity = psArity

patSynArgs :: PatSyn -> [Type]
patSynArgs = psArgs

patSynFieldLabels :: PatSyn -> [FieldLabel]
patSynFieldLabels = psFieldLabels

-- | Extract the type for any given labelled field of the 'DataCon'
patSynFieldType :: PatSyn -> FieldLabelString -> Type
patSynFieldType ps label
  = case find ((== label) . flLabel . fst) (psFieldLabels ps `zip` psArgs ps) of
      Just (_, ty) -> ty
      Nothing -> pprPanic "dataConFieldType" (ppr ps <+> ppr label)

patSynUnivTyVarBinders :: PatSyn -> [InvisTVBinder]
patSynUnivTyVarBinders = psUnivTyVars

patSynExTyVars :: PatSyn -> [TyVar]
patSynExTyVars ps = binderVars (psExTyVars ps)

patSynExTyVarBinders :: PatSyn -> [InvisTVBinder]
patSynExTyVarBinders = psExTyVars

patSynSigBndr :: PatSyn -> ([InvisTVBinder], ThetaType, [InvisTVBinder], ThetaType, [Scaled Type], Type)
patSynSigBndr (MkPatSyn { psUnivTyVars = univ_tvs, psExTyVars = ex_tvs
                        , psProvTheta = prov, psReqTheta = req
                        , psArgs = arg_tys, psResultTy = res_ty })
  = (univ_tvs, req, ex_tvs, prov, map unrestricted arg_tys, res_ty)

patSynSig :: PatSyn -> ([TyVar], ThetaType, [TyVar], ThetaType, [Scaled Type], Type)
patSynSig ps = let (u_tvs, req, e_tvs, prov, arg_tys, res_ty) = patSynSigBndr ps
               in (binderVars u_tvs, req, binderVars e_tvs, prov, arg_tys, res_ty)

patSynMatcher :: PatSyn -> PatSynMatcher
patSynMatcher = psMatcher

patSynBuilder :: PatSyn -> PatSynBuilder
patSynBuilder = psBuilder

patSynResultType :: PatSyn -> Type
patSynResultType = psResultTy

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
  = ASSERT2( tyvars `equalLength` inst_tys
          , text "patSynInstArgTys" <+> ppr name $$ ppr tyvars $$ ppr inst_tys )
    map (substTyWith tyvars inst_tys) arg_tys
  where
    tyvars = binderVars (univ_tvs ++ ex_tvs)

patSynInstResTy :: PatSyn -> [Type] -> Type
-- Return the type of whole pattern
-- E.g.  pattern P x y = Just (x,x,y)
--         P :: a -> b -> Just (a,a,b)
--         (patSynInstResTy P [Int,Bool] = Maybe (Int,Int,Bool)
-- NB: unlike patSynInstArgTys, the inst_tys should be just the *universal* tyvars
patSynInstResTy (MkPatSyn { psName = name, psUnivTyVars = univ_tvs
                          , psResultTy = res_ty })
                inst_tys
  = ASSERT2( univ_tvs `equalLength` inst_tys
           , text "patSynInstResTy" <+> ppr name $$ ppr univ_tvs $$ ppr inst_tys )
    substTyWith (binderVars univ_tvs) inst_tys res_ty

-- | Print the type of a pattern synonym. The foralls are printed explicitly
pprPatSynType :: PatSyn -> SDoc
pprPatSynType (MkPatSyn { psUnivTyVars = univ_tvs,  psReqTheta  = req_theta
                        , psExTyVars   = ex_tvs,    psProvTheta = prov_theta
                        , psArgs       = orig_args, psResultTy = orig_res_ty })
  = sep [ pprForAll $ tyVarSpecToBinders univ_tvs
        , pprThetaArrowTy req_theta
        , ppWhen insert_empty_ctxt $ parens empty <+> darrow
        , pprType sigma_ty ]
  where
    sigma_ty = mkInvisForAllTys ex_tvs $
               mkInvisFunTysMany prov_theta $
               mkVisFunTysMany orig_args orig_res_ty
    insert_empty_ctxt = null req_theta && not (null prov_theta && null ex_tvs)
