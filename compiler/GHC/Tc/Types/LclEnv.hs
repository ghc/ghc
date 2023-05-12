module GHC.Tc.Types.LclEnv where

import GHC.Prelude ( Bool, IO )



import GHC.Tc.Utils.TcType ( TcLevel, ExpType )
import GHC.Tc.Types.Constraint ( WantedConstraints )
import GHC.Tc.Errors.Types ( TcRnMessage )

import GHC.Core.Type ( TyVar )
import GHC.Core.UsageEnv ( UsageEnv )

import GHC.Types.Id         ( idName )
import GHC.Types.Name.Reader ( LocalRdrEnv )
import GHC.Types.Name ( Name, HasOccName(..) )
import GHC.Types.Name.Env ( NameEnv )
import GHC.Types.Var ( Id )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Types.SrcLoc ( RealSrcSpan )
import GHC.Types.Basic ( TopLevelFlag )

import GHC.Data.IOEnv ( IORef )


import GHC.Utils.Error ( Messages, SDoc )
import GHC.Utils.Outputable
    ( Outputable(..), IsLine((<+>), (<>)), brackets )



import GHC.Tc.Types.TcTyThing ( TcTyThing )
import GHC.Tc.Types.TH ( ThStage, ThLevel )

{-
************************************************************************
*                                                                      *
                The local typechecker environment
*                                                                      *
************************************************************************

Note [The Global-Env/Local-Env story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type checking, we keep in the tcg_type_env
        * All types and classes
        * All Ids derived from types and classes (constructors, selectors)

At the end of type checking, we zonk the local bindings,
and as we do so we add to the tcg_type_env
        * Locally defined top-level Ids

Why?  Because they are now Ids not TcIds.  This final GlobalEnv is
        a) fed back (via the knot) to typechecking the
           unfoldings of interface signatures
        b) used in the ModDetails of this module
-}

data TcLclEnv           -- Changes as we move inside an expression
                        -- Discarded after typecheck/rename; not passed on to desugarer
  = TcLclEnv {
        tcl_loc        :: RealSrcSpan,     -- Source span
        tcl_ctxt       :: [ErrCtxt],       -- Error context, innermost on top
        tcl_in_gen_code :: Bool,           -- See Note [Rebindable syntax and HsExpansion]
        tcl_tclvl      :: TcLevel,

        tcl_th_ctxt    :: ThStage,         -- Template Haskell context
        tcl_th_bndrs   :: ThBindEnv,       -- and binder info
            -- The ThBindEnv records the TH binding level of in-scope Names
            -- defined in this module (not imported)
            -- We can't put this info in the TypeEnv because it's needed
            -- (and extended) in the renamer, for untyped splices

        tcl_arrow_ctxt :: ArrowCtxt,       -- Arrow-notation context

        tcl_rdr :: LocalRdrEnv,         -- Local name envt
                -- Maintained during renaming, of course, but also during
                -- type checking, solely so that when renaming a Template-Haskell
                -- splice we have the right environment for the renamer.
                --
                --   Does *not* include global name envt; may shadow it
                --   Includes both ordinary variables and type variables;
                --   they are kept distinct because tyvar have a different
                --   occurrence constructor (Name.TvOcc)
                -- We still need the unsullied global name env so that
                --   we can look up record field names

        tcl_env  :: TcTypeEnv,    -- The local type environment:
                                  -- Ids and TyVars defined in this module

        tcl_usage :: TcRef UsageEnv, -- Required multiplicity of bindings is accumulated here.


        tcl_bndrs :: TcBinderStack,   -- Used for reporting relevant bindings,
                                      -- and for tidying types

        tcl_lie  :: TcRef WantedConstraints,    -- Place to accumulate type constraints
        tcl_errs :: TcRef (Messages TcRnMessage)     -- Place to accumulate diagnostics
    }

setLclEnvTcLevel :: TcLclEnv -> TcLevel -> TcLclEnv
setLclEnvTcLevel env lvl = env { tcl_tclvl = lvl }

getLclEnvTcLevel :: TcLclEnv -> TcLevel
getLclEnvTcLevel = tcl_tclvl

setLclEnvLoc :: TcLclEnv -> RealSrcSpan -> TcLclEnv
setLclEnvLoc env loc = env { tcl_loc = loc }

getLclEnvLoc :: TcLclEnv -> RealSrcSpan
getLclEnvLoc = tcl_loc

lclEnvInGeneratedCode :: TcLclEnv -> Bool
lclEnvInGeneratedCode = tcl_in_gen_code

type ErrCtxt = (Bool, TidyEnv -> IO (TidyEnv, SDoc))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display

{- TODO move these into where CtLoc is defined.


-- These are here to avoid module loops: one might expect them
-- in GHC.Tc.Types.Constraint, but they refer to ErrCtxt which refers to TcM.
-- Easier to just keep these definitions here, alongside TcM.
pushErrCtxt :: CtOrigin -> ErrCtxt -> CtLoc -> CtLoc
pushErrCtxt o err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_origin = o, ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }

pushErrCtxtSameOrigin :: ErrCtxt -> CtLoc -> CtLoc
-- Just add information w/o updating the origin!
pushErrCtxtSameOrigin err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }
  -}


type TcTypeEnv = NameEnv TcTyThing

type ThBindEnv = NameEnv (TopLevelFlag, ThLevel)
   -- Domain = all Ids bound in this module (ie not imported)
   -- The TopLevelFlag tells if the binding is syntactically top level.
   -- We need to know this, because the cross-stage persistence story allows
   -- cross-stage at arbitrary types if the Id is bound at top level.
   --
   -- Nota bene: a ThLevel of 'outerLevel' is *not* the same as being
   -- bound at top level!  See Note [Template Haskell levels] in GHC.Tc.Gen.Splice

---------------------------
-- Arrow-notation context
---------------------------

{- Note [Escaping the arrow scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In arrow notation, a variable bound by a proc (or enclosed let/kappa)
is not in scope to the left of an arrow tail (-<) or the head of (|..|).
For example

        proc x -> (e1 -< e2)

Here, x is not in scope in e1, but it is in scope in e2.  This can get
a bit complicated:

        let x = 3 in
        proc y -> (proc z -> e1) -< e2

Here, x and z are in scope in e1, but y is not.

We implement this by
recording the environment when passing a proc (using newArrowScope),
and returning to that (using escapeArrowScope) on the left of -< and the
head of (|..|).

All this can be dealt with by the *renamer*. But the type checker needs
to be involved too.  Example (arrowfail001)
  class Foo a where foo :: a -> ()
  data Bar = forall a. Foo a => Bar a
  get :: Bar -> ()
  get = proc x -> case x of Bar a -> foo -< a
Here the call of 'foo' gives rise to a (Foo a) constraint that should not
be captured by the pattern match on 'Bar'.  Rather it should join the
constraints from further out.  So we must capture the constraint bag
from further out in the ArrowCtxt that we push inwards.
-}

data ArrowCtxt   -- Note [Escaping the arrow scope]
  = NoArrowCtxt
  | ArrowCtxt LocalRdrEnv (IORef WantedConstraints)


---------------------------
-- The TcBinderStack
---------------------------

type TcBinderStack = [TcBinder]
   -- This is a stack of locally-bound ids and tyvars,
   --   innermost on top
   -- Used only in error reporting (relevantBindings in TcError),
   --   and in tidying
   -- We can't use the tcl_env type environment, because it doesn't
   --   keep track of the nesting order

type TcId = Id
type TcRef = IORef

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


{-
************************************************************************
*                                                                      *
            CtLoc
*                                                                      *
************************************************************************

The 'CtLoc' gives information about where a constraint came from.
This is important for decent error message reporting because
dictionaries don't appear in the original source code.

-}

{-
data CtLoc = CtLoc { ctl_origin   :: CtOrigin
                   , ctl_env      :: TcLclEnv
                   , ctl_t_or_k   :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_depth    :: !SubGoalDepth }
                   -}

  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: TcBinderStack
  --    level:            tcl_tclvl :: TcLevel