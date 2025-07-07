module GHC.Tc.Types.LclEnv (
    TcLclEnv(..)
  , TcLclCtxt(..)
  , modifyLclCtxt

  , getLclEnvArrowCtxt
  , getLclEnvThBndrs
  , getLclEnvTypeEnv
  , getLclEnvBinderStack
  , getLclEnvErrCtxt
  , getLclEnvLoc
  , getLclEnvRdrEnv
  , getLclEnvTcLevel
  , getLclEnvThLevel
  , setLclEnvTcLevel
  , setLclEnvLoc
  , setLclEnvRdrEnv
  , setLclEnvBinderStack
  , setLclEnvErrCtxt
  , setLclEnvThLevel
  , setLclEnvTypeEnv
  , modifyLclEnvTcLevel

  , getLclEnvSrcCodeOrigin
  , setLclEnvSrcCodeOrigin
  , setLclCtxtSrcCodeOrigin
  , lclEnvInGeneratedCode

  , addLclEnvErrCtxt

  , ErrCtxtStack (..)
  , ArrowCtxt(..)
  , ThBindEnv
  , TcTypeEnv
) where

import GHC.Prelude

import GHC.Hs ( SrcCodeOrigin )
import GHC.Tc.Utils.TcType ( TcLevel )
import GHC.Tc.Errors.Types ( TcRnMessage )

import GHC.Core.UsageEnv ( UsageEnv )

import GHC.Types.Name.Reader ( LocalRdrEnv )
import GHC.Types.Name.Env ( NameEnv )
import GHC.Types.SrcLoc ( RealSrcSpan )
import GHC.Types.Basic ( TopLevelFlag )

import GHC.Types.Error ( Messages )

import GHC.Tc.Types.BasicTypes
import GHC.Tc.Types.TH
import GHC.Tc.Types.TcRef
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Constraint ( WantedConstraints )

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
        -- The part that we sometimes restore using `restoreLclEnv`.
        tcl_lcl_ctxt    :: !TcLclCtxt,

        -- These are exactly the parts of TcLclEnv which are not set by `restoreLclEnv`.

        tcl_usage :: TcRef UsageEnv, -- Required multiplicity of bindings is accumulated here.
        tcl_lie  :: TcRef WantedConstraints,    -- Place to accumulate type constraints
        tcl_errs :: TcRef (Messages TcRnMessage)     -- Place to accumulate diagnostics
    }


data ErrCtxtStack
  = UserCodeCtxt {err_ctxt :: [ErrCtxt]}
  | GeneratedCodeCtxt { src_code_origin :: SrcCodeOrigin
                      , err_ctxt ::  [ErrCtxt] }

isGeneratedCodeCtxt :: ErrCtxtStack -> Bool
isGeneratedCodeCtxt UserCodeCtxt{} = False
isGeneratedCodeCtxt _ = True

get_src_code_origin :: ErrCtxtStack -> Maybe SrcCodeOrigin
get_src_code_origin (UserCodeCtxt{}) = Nothing
get_src_code_origin es = Just $ src_code_origin es

modify_err_ctxt_stack :: ([ErrCtxt] -> [ErrCtxt]) -> ErrCtxtStack -> ErrCtxtStack
modify_err_ctxt_stack f (UserCodeCtxt e) =  UserCodeCtxt (f e)
modify_err_ctxt_stack _ c = c -- any updates on the err context in generated context should be ignored


data TcLclCtxt
  = TcLclCtxt {
        tcl_loc        :: RealSrcSpan,     -- Source span
        tcl_ctxt       :: ErrCtxtStack,
        tcl_tclvl      :: TcLevel,
        tcl_bndrs      :: TcBinderStack,   -- Used for reporting relevant bindings,
                                           -- and for tidying type

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


        tcl_th_ctxt    :: ThLevel,         -- Template Haskell context
        tcl_th_bndrs   :: ThBindEnv,       -- and binder info
            -- The ThBindEnv records the TH binding level of in-scope Names
            -- defined in this module (not imported)
            -- We can't put this info in the TypeEnv because it's needed
            -- (and extended) in the renamer, for untyped splices

        tcl_arrow_ctxt :: ArrowCtxt,       -- Arrow-notation context

        tcl_env  :: TcTypeEnv    -- The local type environment:
                                 -- Ids and TyVars defined in this module
    }

getLclEnvThLevel :: TcLclEnv -> ThLevel
getLclEnvThLevel = tcl_th_ctxt . tcl_lcl_ctxt

setLclEnvThLevel :: ThLevel -> TcLclEnv -> TcLclEnv
setLclEnvThLevel l = modifyLclCtxt (\env -> env { tcl_th_ctxt = l })

getLclEnvThBndrs :: TcLclEnv -> ThBindEnv
getLclEnvThBndrs = tcl_th_bndrs . tcl_lcl_ctxt

getLclEnvArrowCtxt :: TcLclEnv -> ArrowCtxt
getLclEnvArrowCtxt = tcl_arrow_ctxt . tcl_lcl_ctxt

getLclEnvTypeEnv :: TcLclEnv -> TcTypeEnv
getLclEnvTypeEnv = tcl_env . tcl_lcl_ctxt

setLclEnvTypeEnv :: TcTypeEnv -> TcLclEnv -> TcLclEnv
setLclEnvTypeEnv ty_env = modifyLclCtxt (\env -> env { tcl_env = ty_env})

setLclEnvTcLevel :: TcLevel -> TcLclEnv -> TcLclEnv
setLclEnvTcLevel lvl = modifyLclCtxt (\env -> env {tcl_tclvl = lvl })

modifyLclEnvTcLevel :: (TcLevel -> TcLevel) -> TcLclEnv -> TcLclEnv
modifyLclEnvTcLevel f = modifyLclCtxt (\env -> env { tcl_tclvl = f (tcl_tclvl env)})

getLclEnvTcLevel :: TcLclEnv -> TcLevel
getLclEnvTcLevel = tcl_tclvl . tcl_lcl_ctxt

setLclEnvLoc :: RealSrcSpan -> TcLclEnv -> TcLclEnv
setLclEnvLoc loc = modifyLclCtxt (\lenv -> lenv { tcl_loc = loc })

getLclEnvLoc :: TcLclEnv -> RealSrcSpan
getLclEnvLoc = tcl_loc . tcl_lcl_ctxt

getLclEnvErrCtxt :: TcLclEnv -> [ErrCtxt]
getLclEnvErrCtxt = err_ctxt . tcl_ctxt . tcl_lcl_ctxt

setLclEnvErrCtxt :: [ErrCtxt] -> TcLclEnv -> TcLclEnv
setLclEnvErrCtxt ctxt = modifyLclCtxt (\env -> env { tcl_ctxt = modify_err_ctxt_stack (\ _ -> ctxt) (tcl_ctxt env) })

addLclEnvErrCtxt :: ErrCtxt -> TcLclEnv -> TcLclEnv
addLclEnvErrCtxt ec = modifyLclCtxt (\env -> env { tcl_ctxt = modify_err_ctxt_stack (\ctxt -> ec : ctxt) (tcl_ctxt env) })

getLclEnvSrcCodeOrigin :: TcLclEnv -> Maybe SrcCodeOrigin
getLclEnvSrcCodeOrigin = get_src_code_origin . tcl_ctxt . tcl_lcl_ctxt

setLclEnvSrcCodeOrigin :: SrcCodeOrigin -> TcLclEnv -> TcLclEnv
setLclEnvSrcCodeOrigin o = modifyLclCtxt (setLclCtxtSrcCodeOrigin o)

setLclCtxtSrcCodeOrigin :: SrcCodeOrigin -> TcLclCtxt -> TcLclCtxt
setLclCtxtSrcCodeOrigin o ctxt = ctxt { tcl_ctxt = GeneratedCodeCtxt o (err_ctxt $ tcl_ctxt ctxt) }

lclCtxtInGeneratedCode :: TcLclCtxt -> Bool
lclCtxtInGeneratedCode = isGeneratedCodeCtxt . tcl_ctxt

lclEnvInGeneratedCode :: TcLclEnv -> Bool
lclEnvInGeneratedCode = lclCtxtInGeneratedCode . tcl_lcl_ctxt

getLclEnvBinderStack :: TcLclEnv -> TcBinderStack
getLclEnvBinderStack = tcl_bndrs . tcl_lcl_ctxt

setLclEnvBinderStack :: TcBinderStack -> TcLclEnv -> TcLclEnv
setLclEnvBinderStack stack = modifyLclCtxt (\env -> env { tcl_bndrs = stack })

getLclEnvRdrEnv :: TcLclEnv -> LocalRdrEnv
getLclEnvRdrEnv = tcl_rdr . tcl_lcl_ctxt

setLclEnvRdrEnv :: LocalRdrEnv -> TcLclEnv -> TcLclEnv
setLclEnvRdrEnv rdr_env = modifyLclCtxt (\env -> env { tcl_rdr = rdr_env })

modifyLclCtxt :: (TcLclCtxt -> TcLclCtxt) -> TcLclEnv -> TcLclEnv
modifyLclCtxt upd env =
  let !res = upd (tcl_lcl_ctxt env)
  in env { tcl_lcl_ctxt = res }



type TcTypeEnv = NameEnv TcTyThing

type ThBindEnv = NameEnv (TopLevelFlag, ThLevelIndex)
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
  | ArrowCtxt LocalRdrEnv (TcRef WantedConstraints)
