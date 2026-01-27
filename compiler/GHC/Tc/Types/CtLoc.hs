
module GHC.Tc.Types.CtLoc (

  -- * CtLoc
  CtLoc(..), ctLocSpan, ctLocEnv, ctLocLevel, ctLocOrigin,
  ctLocTypeOrKind_maybe, toInvisibleLoc,
  ctLocDepth, bumpCtLocDepth, resetCtLocDepth,
  isGivenLoc, mkGivenLoc, mkKindEqLoc,
  setCtLocOrigin, updateCtLocOrigin, setCtLocEnv, setCtLocSpan,
  pprCtLoc, adjustCtLoc, adjustCtLocTyConBinder,

  -- * CtLocEnv
  CtLocEnv(..),
  getCtLocEnvLoc, setCtLocEnvLoc, setCtLocRealLoc,
  getCtLocEnvLvl, setCtLocEnvLvl,
  ctLocEnvInGeneratedCode,

  -- * SubGoalDepth
  SubGoalDepth, initialSubGoalDepth, maxSubGoalDepth,
  bumpSubGoalDepth, subGoalDepthExceeded,

  -- * Explanations
  CtExplanations(..), ctLocExplanations, updCtLocExplanations,
  outOfScopeNT, stuckDataFamApp, nullCtExplanations,

  -- * RoleExplanation
  RoleExplanation(..),
  tyConAppRoleExplanation, appTyRoleExplanation

  ) where

import GHC.Prelude

import GHC.Tc.Types.BasicTypes
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Origin

import GHC.Tc.Utils.TcType

import GHC.Types.Basic( IntWithInf, mkIntWithInf, TypeOrKind(..) )
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Unique.Map ( UniqMap, emptyUniqMap, plusUniqMap_C, unitUniqMap, isNullUniqMap )
import GHC.Types.Unique.Set ( UniqSet, emptyUniqSet, unionUniqSets, unitUniqSet, isEmptyUniqSet )

import GHC.Core.DataCon ( DataCon )
import GHC.Core.TyCon
  ( Role(..), TyCon, TyConBinder
  , isVisibleTyConBinder, isNamedTyConBinder
  )
import GHC.Utils.Outputable

import qualified Data.Semigroup as S
import qualified GHC.Data.List.NonEmpty as NE

{- *********************************************************************
*                                                                      *
            SubGoalDepth
*                                                                      *
********************************************************************* -}

{- Note [SubGoalDepth]
~~~~~~~~~~~~~~~~~~~~~~
The 'SubGoalDepth' takes care of stopping the constraint solver from looping.

The counter starts at zero and increases. It includes dictionary constraints,
equality simplification, and type family reduction. (Why combine these? Because
it's actually quite easy to mistake one for another, in sufficiently involved
scenarios, like ConstraintKinds.)

The flag -freduction-depth=n fixes the maximum level.

* The counter includes the depth of type class instance declarations.  Example:
     [W] d{7} : Eq [Int]
  That is d's dictionary-constraint depth is 7.  If we use the instance
     $dfEqList :: Eq a => Eq [a]
  to simplify it, we get
     d{7} = $dfEqList d'{8}
  where d'{8} : Eq Int, and d' has depth 8.

  For civilised (decidable) instance declarations, each increase of
  depth removes a type constructor from the type, so the depth never
  gets big; i.e. is bounded by the structural depth of the type.

* The counter also increments when resolving
equalities involving type functions. Example:
  Assume we have a wanted at depth 7:
    [W] d{7} : F () ~ a
  If there is a type function equation "F () = Int", this would be rewritten to
    [W] d{8} : Int ~ a
  and remembered as having depth 8.

  Again, without UndecidableInstances, this counter is bounded, but without it
  can resolve things ad infinitum. Hence there is a maximum level.

* Lastly, every time an equality is rewritten, the counter increases. Again,
  rewriting an equality constraint normally makes progress, but it's possible
  the "progress" is just the reduction of an infinitely-reducing type family.
  Hence we need to track the rewrites.

When compiling a program requires a greater depth, then GHC recommends turning
off this check entirely by setting -freduction-depth=0. This is because the
exact number that works is highly variable, and is likely to change even between
minor releases. Because this check is solely to prevent infinite compilation
times, it seems safe to disable it when a user has ascertained that their program
doesn't loop at the type level.

-}

-- | See Note [SubGoalDepth]
newtype SubGoalDepth = SubGoalDepth Int
  deriving (Eq, Ord, Outputable)

initialSubGoalDepth :: SubGoalDepth
initialSubGoalDepth = SubGoalDepth 0

bumpSubGoalDepth :: SubGoalDepth -> SubGoalDepth
bumpSubGoalDepth (SubGoalDepth n) = SubGoalDepth (n + 1)

maxSubGoalDepth :: SubGoalDepth -> SubGoalDepth -> SubGoalDepth
maxSubGoalDepth (SubGoalDepth n) (SubGoalDepth m) = SubGoalDepth (n `max` m)

subGoalDepthExceeded :: IntWithInf -> SubGoalDepth -> Bool
subGoalDepthExceeded reductionDepth (SubGoalDepth d)
  = mkIntWithInf d > reductionDepth


{- *********************************************************************
*                                                                      *
            CtLoc
*                                                                      *
************************************************************************

The 'CtLoc' gives information about where a constraint came from.
This is important for decent error message reporting because
dictionaries don't appear in the original source code.

-}


-- | 'CtLoc' gives information about where a constraint came from.
--
-- This is important for decent error message reporting, because
-- dictionaries don't appear in the original source code.
--
-- See Note [CtLoc].
data CtLoc = CtLoc { ctl_origin   :: CtOrigin
                   , ctl_env      :: CtLocEnv -- Everything we need to know about
                                              -- the context this Ct arose in.
                   , ctl_t_or_k   :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_expln    :: CtExplanations
                      -- ^ Explanations we can display to the user if
                      -- the constraint goes unsolved; see Note [CtExplanations]
                   , ctl_depth    :: !SubGoalDepth
                   }

{- Note [CtLoc]
~~~~~~~~~~~~~~~
When we report an unsolved constraint to the user, we report several pieces of
information to the user for additional context:

  - `CtOrigin` describes the place in the source code where the constraint
    originated; the same `CtOrigin` is inherited by its sub-goals.
    For example, starting with `Maybe t1 ~# Maybe t2` we may decompose to
    `t1 ~# t2`, but the latter inherits the same `CtOrigin`.
  - `CtLocEnv` provides further context, e.g. a source code line number, as
    well as e.g. in-scope bindings so that they can be suggested for an
    out-of-scope error.
  - `CtExplanations` contain additional information that explain why we ended
    up with this sub-goal. They pertain to work the solver has done to process
    the original constraint. See See Note [CtExplanations].

Note [CtExplanations]
~~~~~~~~~~~~~~~~~~~~~
The CtExplanations type records information about steps the solver has taken
when processing a constraint, in particular when the solver had to make a choice.

For example, starting with a representational equality `T a ~R# T b`, the solver
may proceed in two different ways:

  (Unwrap)
    If T is a newtype whose constructor is in scope, we may unwrap it, resulting
    in the constraint `a ~R# b`.
  (Decompose)
    If T is a not a newtype or its constructor isn't in scope, we decompose
    according to the role of `T`. If `T` has nominal role, this means we end
    up with `a ~# b`.

It's helpful to report some additional explanations if we end up with `a ~# b`
starting from `T a ~R# T b`: the user might wonder why the role went from
representational to nominal:

  (NTScope)
    If `T` is a newtype whose constructor is out-of-scope, inform the user:

      NB: The data constructor ‘MkT’ of newtype ‘T’ is not in scope.

  (Role)
    If moreover `T` has nominal role, also report that to the user.

      NB: ‘T’ has nominal role in its first argument.

This is especially important as `T` may otherwise not appear in the error
message at all (as indeed is the case if we end up with `a ~# b`), so it can be
jolly confusing: #15850, #20289, #23731, #26137.

CRUCIAL POINT: we store the out-of-scope newtype constructor precisely at the
point in the typechecker in which we perform the in-scope test. That is, in
the ReprEq case of GHC.Tc.Solver.Equality.can_eq_nc, where we call
tcTopNormaliseNewTypeTF_maybe.
This ensures that the explanations are in sync with the source of truth.

For the time being, CtExplanations records only information relating to
representational equalities, as described above. In the future, one could
imagine adding explanations relating to instance resolution, e.g.

  - Store the result of instance lookup, e.g. "no instances" or
    "overlapping instances", to avoid needing to perform a second instance
    lookup in GHC.Tc.Errors.mkDictErr.
  - When GHC holds off from using a quantified constraint because there is no
    single "best match" (#22216).
-}

-- | Explanations that can be presented to the user about why the solver
-- might have failed to solve a constraint. See Note [CtExplanations].
--
-- For example: couldn't solve a representational equality because a newtype
-- constructor was out of scope.
data CtExplanations
  -- NB: there might be several explanations associated with a single constraint.
  -- Example: T23731b, where the error "Couldn't match type ‘Int’ with ‘Sum Int’"
  -- comes with the explanations
  --
  --    The type constructor ‘Foo’ has nominal role in its first argument.
  --    The data constructor ‘MkFoo’ of newtype ‘Foo’ is not in scope.
  = CtExplanations
    { ctexpl_roleExplanations :: [RoleExplanation]
      -- ^ Explanations to the user that involve roles,
      -- e.g. remarking that a 'TyCon' has nominal role in one of its arguments.
    , ctexpl_outOfScopeNTs :: UniqSet DataCon
      -- ^ Newtypes that could not be unwrapped because their constructors
      -- were not in scope.
      --
      -- NB: store the 'DataCon' rather than the 'TyCon', as this avoids
      -- any issues with internal representation 'TyCon's for data families,
      -- as in 'data family D a; newtype instance D Int = MkDInt Bool'.

    , ctexpl_stuckDataFamApps :: UniqMap TyCon (NE.NonEmpty [Type])
      -- ^ Data family applications that could not be reduced.
    }

outOfScopeNT :: DataCon -> CtExplanations
outOfScopeNT nt =
  mempty { ctexpl_outOfScopeNTs = unitUniqSet nt }

stuckDataFamApp :: TyCon -> [Type] -> CtExplanations
stuckDataFamApp tc tys =
  mempty { ctexpl_stuckDataFamApps = unitUniqMap tc (NE.singleton tys) }

nullCtExplanations :: CtExplanations -> Bool
nullCtExplanations (CtExplanations a b c) =
  null a && isEmptyUniqSet b && isNullUniqMap c

instance Outputable CtExplanations where
  ppr (CtExplanations ns cs dfs) =
    hang (text "CtExplanations")
      2 $ vcat
            [ text "nominal reasons:" <+> ppr ns
            , text "out of scope newtype constructors:" <+> ppr cs
            , text "stuck data-fam apps:" <+> ppr dfs
            ]
instance Semigroup CtExplanations where
  CtExplanations n1 c1 dfs1 <> CtExplanations n2 c2 dfs2 =
    CtExplanations
      (n1 ++ n2)
      (c1 `unionUniqSets` c2)
      (plusUniqMap_C (S.<>) dfs1 dfs2)

instance Monoid CtExplanations where
  mempty = CtExplanations [] emptyUniqSet emptyUniqMap

-- | Information about why a representational equality gave rise to
-- a nominal equality.
data RoleExplanation
  -- | The 'TyCon' has the given role this argument.
  = TyConArg
      TyCon
      Int -- ^ argument position (starting from 1); may be greater than the
          -- arity of the 'TyCon' in the case of an over-saturated application
      Role
  -- | The role of the argument in an 'AppTy' is nominal.
  --
  -- Store the function type in the 'AppTy'.
  | NominalAppTy Type
instance Outputable RoleExplanation where
  ppr = \case
    TyConArg tc i r ->
      text "NominalTyConArg" <+> ppr tc <+> ppr i <+> ppr r
    NominalAppTy f ->
      text "NominalAppTy" <+> ppr f

addRoleExplanation :: RoleExplanation -> CtExplanations -> CtExplanations
addRoleExplanation rea expln =
  expln { ctexpl_roleExplanations = rea : ctexpl_roleExplanations expln }

mkKindEqLoc :: TcType -> TcType   -- original *types* being compared
            -> CtLoc -> CtLoc
mkKindEqLoc s1 s2 ctloc
  | CtLoc { ctl_t_or_k = t_or_k, ctl_origin = origin } <- ctloc
  = ctloc { ctl_origin = KindEqOrigin s1 s2 origin t_or_k
          , ctl_t_or_k = Just KindLevel }

adjustCtLocTyConBinder :: Maybe TyConBinder -> Maybe RoleExplanation -> CtLoc -> CtLoc
-- Adjust the CtLoc when decomposing a type constructor
adjustCtLocTyConBinder mb_tc_bndr mb_repr loc
  = adjustCtLoc mb_invis_kind is_kind mb_repr loc
  where
    mb_invis_kind
      | Just tc_bndr <- mb_tc_bndr
      , not $ isVisibleTyConBinder tc_bndr
      = Just InvisibleKind
      | otherwise
      = Nothing
    is_kind = maybe False isNamedTyConBinder mb_tc_bndr

-- | Add a role explanation when decomposing 'T a ~r1 T b' for some role 'r1'
-- into 'a ~r2 b' with stronger role 'r2' (e.g. turning a representational
-- equality into a nominal equality).
tyConAppRoleExplanation :: Role -> TyCon -> (Int, Role) -> Maybe RoleExplanation
tyConAppRoleExplanation role tc (arg_no, arg_role) =
  if arg_role < role -- finer notion of equality
  then Just $ TyConArg tc arg_no arg_role
  else Nothing

appTyRoleExplanation :: Type -> CtOrigin -> Maybe RoleExplanation
appTyRoleExplanation s orig
  | _ : _ <- defaultReprEqOrigins orig
  = Just $ NominalAppTy s
  | otherwise
  = Nothing

adjustCtLoc :: Maybe InvisibleBit
            -> Bool    -- True <=> A kind argument
            -> Maybe RoleExplanation
            -> CtLoc -> CtLoc
-- Adjust the CtLoc when decomposing a type constructor, application, etc
adjustCtLoc mb_invis is_kind mb_repr loc
  = loc3
  where
    loc1 | is_kind   = toKindLoc loc
         | otherwise = loc
    loc2 =
      case mb_invis of
        Nothing    -> loc1
        Just invis -> toInvisibleLoc invis loc1
    loc3 | Just repr_loc <- mb_repr
         = updCtLocExplanations (addRoleExplanation repr_loc) loc2
         | otherwise
         = loc2

-- | Take a CtLoc and moves it to the kind level
toKindLoc :: CtLoc -> CtLoc
toKindLoc loc = loc { ctl_t_or_k = Just KindLevel }

toInvisibleLoc :: InvisibleBit -> CtLoc -> CtLoc
toInvisibleLoc invis loc = updateCtLocOrigin loc (toInvisibleOrigin invis)

mkGivenLoc :: TcLevel -> SkolemInfoAnon -> CtLocEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin   = GivenOrigin skol_info
          , ctl_env      = setCtLocEnvLvl env tclvl
          , ctl_t_or_k   = Nothing    -- these only matter
          , ctl_expln    = mempty     -- for error messages
          , ctl_depth    = initialSubGoalDepth }

ctLocEnv :: CtLoc -> CtLocEnv
ctLocEnv = ctl_env

ctLocLevel :: CtLoc -> TcLevel
ctLocLevel loc = getCtLocEnvLvl (ctLocEnv loc)

ctLocDepth :: CtLoc -> SubGoalDepth
ctLocDepth = ctl_depth

ctLocOrigin :: CtLoc -> CtOrigin
ctLocOrigin = ctl_origin

ctLocExplanations :: CtLoc -> CtExplanations
ctLocExplanations = ctl_expln

updCtLocExplanations :: (CtExplanations -> CtExplanations) -> CtLoc -> CtLoc
updCtLocExplanations f loc = loc { ctl_expln = f $ ctl_expln loc }

ctLocSpan :: CtLoc -> RealSrcSpan
ctLocSpan (CtLoc { ctl_env = lcl}) = getCtLocEnvLoc lcl

ctLocTypeOrKind_maybe :: CtLoc -> Maybe TypeOrKind
ctLocTypeOrKind_maybe = ctl_t_or_k

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (setCtLocRealLoc lcl loc)

bumpCtLocDepth :: CtLoc -> CtLoc
bumpCtLocDepth loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth d }

resetCtLocDepth :: CtLoc -> CtLoc
resetCtLocDepth loc = loc { ctl_depth = initialSubGoalDepth }

setCtLocOrigin :: CtLoc -> CtOrigin -> CtLoc
setCtLocOrigin ctl orig = ctl { ctl_origin = orig }

updateCtLocOrigin :: CtLoc -> (CtOrigin -> CtOrigin) -> CtLoc
updateCtLocOrigin ctl@(CtLoc { ctl_origin = orig }) upd
  = ctl { ctl_origin = upd orig }

setCtLocEnv :: CtLoc -> CtLocEnv -> CtLoc
setCtLocEnv ctl env = ctl { ctl_env = env }

isGivenLoc :: CtLoc -> Bool
isGivenLoc loc = isGivenOrigin (ctLocOrigin loc)

pprCtLoc :: CtLoc -> SDoc
-- "arising from ... at ..."
-- Not an instance of Outputable because of the "arising from" prefix
pprCtLoc (CtLoc { ctl_origin = o, ctl_env = lcl})
  = sep [ pprCtOrigin o
        , text "at" <+> ppr (getCtLocEnvLoc lcl)]


{- *********************************************************************
*                                                                      *
            CtLocEnv
*                                                                      *
********************************************************************* -}

-- | Local typechecker environment for a constraint.
--
-- Used to restore the environment of a constraint
-- when reporting errors, see `setCtLocM`.
--
-- See also 'TcLclCtxt'.
data CtLocEnv = CtLocEnv { ctl_ctxt :: ![ErrCtxt]
                         , ctl_loc :: !RealSrcSpan
                         , ctl_bndrs :: !TcBinderStack
                         , ctl_tclvl :: !TcLevel
                         , ctl_in_gen_code :: !Bool
                         , ctl_rdr :: !LocalRdrEnv }

getCtLocEnvLoc :: CtLocEnv -> RealSrcSpan
getCtLocEnvLoc = ctl_loc

getCtLocEnvLvl :: CtLocEnv -> TcLevel
getCtLocEnvLvl = ctl_tclvl

setCtLocEnvLvl :: CtLocEnv -> TcLevel -> CtLocEnv
setCtLocEnvLvl env lvl = env { ctl_tclvl = lvl }

setCtLocRealLoc :: CtLocEnv -> RealSrcSpan -> CtLocEnv
setCtLocRealLoc env ss = env { ctl_loc = ss }

setCtLocEnvLoc :: CtLocEnv -> SrcSpan -> CtLocEnv
-- See Note [Error contexts in generated code]
-- for the ctl_in_gen_code manipulation
setCtLocEnvLoc env (RealSrcSpan loc _)
  = env { ctl_loc = loc, ctl_in_gen_code = False }

setCtLocEnvLoc env loc@(UnhelpfulSpan _)
  | isGeneratedSrcSpan loc
  = env { ctl_in_gen_code = True }
  | otherwise
  = env

ctLocEnvInGeneratedCode :: CtLocEnv -> Bool
ctLocEnvInGeneratedCode = ctl_in_gen_code
