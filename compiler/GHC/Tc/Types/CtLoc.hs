module GHC.Tc.Types.CtLoc (

  -- * CtLoc
  CtLoc(..), ctLocSpan, ctLocEnv, ctLocLevel, ctLocOrigin,
  ctLocTypeOrKind_maybe, toInvisibleLoc,
  ctLocDepth, bumpCtLocDepth, isGivenLoc, mkGivenLoc, mkKindEqLoc,
  setCtLocOrigin, updateCtLocOrigin, setCtLocEnv, setCtLocSpan,
  pprCtLoc, adjustCtLoc, adjustCtLocTyConBinder,

  -- * CtLocEnv
  CtLocEnv(..),
  getCtLocEnvLoc, setCtLocEnvLoc, setCtLocRealLoc,
  getCtLocEnvLvl, setCtLocEnvLvl,
  ctLocEnvInGeneratedCode,

  -- * SubGoalDepth
  SubGoalDepth, initialSubGoalDepth, maxSubGoalDepth,
  bumpSubGoalDepth, subGoalDepthExceeded

  ) where

import GHC.Prelude

import GHC.Tc.Types.BasicTypes
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Origin

import GHC.Tc.Utils.TcType

import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Basic( IntWithInf, mkIntWithInf, TypeOrKind(..) )

import GHC.Core.TyCon( TyConBinder, isVisibleTyConBinder, isNamedTyConBinder )

import GHC.Utils.Outputable


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

data CtLoc = CtLoc { ctl_origin   :: CtOrigin
                   , ctl_env      :: CtLocEnv -- Everything we need to know about
                                              -- the context this Ct arose in.
                   , ctl_t_or_k   :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_depth    :: !SubGoalDepth }

mkKindEqLoc :: TcType -> TcType   -- original *types* being compared
            -> CtLoc -> CtLoc
mkKindEqLoc s1 s2 ctloc
  | CtLoc { ctl_t_or_k = t_or_k, ctl_origin = origin } <- ctloc
  = ctloc { ctl_origin = KindEqOrigin s1 s2 origin t_or_k
          , ctl_t_or_k = Just KindLevel }

adjustCtLocTyConBinder :: TyConBinder -> CtLoc -> CtLoc
-- Adjust the CtLoc when decomposing a type constructor
adjustCtLocTyConBinder tc_bndr loc
  = adjustCtLoc is_vis is_kind loc
  where
    is_vis  = isVisibleTyConBinder tc_bndr
    is_kind = isNamedTyConBinder tc_bndr

adjustCtLoc :: Bool    -- True <=> A visible argument
            -> Bool    -- True <=> A kind argument
            -> CtLoc -> CtLoc
-- Adjust the CtLoc when decomposing a type constructor, application, etc
adjustCtLoc is_vis is_kind loc
  = loc2
  where
    loc1 | is_kind   = toKindLoc loc
         | otherwise = loc
    loc2 | is_vis    = loc1
         | otherwise = toInvisibleLoc loc1

-- | Take a CtLoc and moves it to the kind level
toKindLoc :: CtLoc -> CtLoc
toKindLoc loc = loc { ctl_t_or_k = Just KindLevel }

toInvisibleLoc :: CtLoc -> CtLoc
toInvisibleLoc loc = updateCtLocOrigin loc toInvisibleOrigin

mkGivenLoc :: TcLevel -> SkolemInfoAnon -> CtLocEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin   = GivenOrigin skol_info
          , ctl_env      = setCtLocEnvLvl env tclvl
          , ctl_t_or_k   = Nothing    -- this only matters for error msgs
          , ctl_depth    = initialSubGoalDepth }

ctLocEnv :: CtLoc -> CtLocEnv
ctLocEnv = ctl_env

ctLocLevel :: CtLoc -> TcLevel
ctLocLevel loc = getCtLocEnvLvl (ctLocEnv loc)

ctLocDepth :: CtLoc -> SubGoalDepth
ctLocDepth = ctl_depth

ctLocOrigin :: CtLoc -> CtOrigin
ctLocOrigin = ctl_origin

ctLocSpan :: CtLoc -> RealSrcSpan
ctLocSpan (CtLoc { ctl_env = lcl}) = getCtLocEnvLoc lcl

ctLocTypeOrKind_maybe :: CtLoc -> Maybe TypeOrKind
ctLocTypeOrKind_maybe = ctl_t_or_k

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (setCtLocRealLoc lcl loc)

bumpCtLocDepth :: CtLoc -> CtLoc
bumpCtLocDepth loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth d }

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
