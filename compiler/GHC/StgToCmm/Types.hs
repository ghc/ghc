

module GHC.StgToCmm.Types
  ( CgInfos (..)
  , LambdaFormInfo (..)
  , ModuleLFInfos
  , StandardFormInfo (..)
  , DoSCCProfiling
  , DoExtDynRefs
  ) where

import GHC.Prelude

import GHC.Core.DataCon

import GHC.Stg.InferTags.TagSig

import GHC.Runtime.Heap.Layout

import GHC.Types.Basic
import GHC.Types.ForeignStubs
import GHC.Types.Name.Env
import GHC.Types.Name.Set

import GHC.Utils.Outputable


{-
Note [Conveying CAF-info and LFInfo between modules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some information about an Id is generated in the code generator, and is not
available earlier.  Namely:

* CAF info.   Code motion in Cmm or earlier phases may move references around so
  we compute information about which bits of code refer to which CAF late in the
  Cmm pipeline.

* LambdaFormInfo. This records the details of a closure representation,
  including
    - the final arity (for functions)
    - whether it is a data constructor, and if so its tag

Collectively we call this CgInfo (see GHC.StgToCmm.Types).

It's very useful for importing modules to have this information. We can always
make a conservative assumption, but that is bad: e.g.

* For CAF info, if we know nothing we have to assume it is a CAF which bloats
  the SRTs of the importing module.

  Conservative assumption here is made when creating new Ids.

* For data constructors, we really like having well-tagged pointers. See #14677,
  #16559, #15155, and wiki: commentary/rts/haskell-execution/pointer-tagging

  Conservative assumption here is made when we import an Id without a
  LambdaFormInfo in the interface, in GHC.StgToCmm.Closure.mkLFImported.

So we arrange to always serialise this information into the interface file.  The
moving parts are:

* We record the CgInfo in the IdInfo of the Id.

* GHC.Driver.Pipeline: the call to updateModDetailsIdInfos augments the
  ModDetails constructed at the end of the Core pipeline, with CgInfo
  gleaned from the back end.  The hard work is done in GHC.Iface.UpdateIdInfos.

* For ModIface we generate the final ModIface with CgInfo in
  GHC.Iface.Make.mkFullIface.

* We don't absolutely guarantee to serialise the CgInfo: we won't if you have
  -fomit-interface-pragmas or -fno-code; and we won't read it in if you have
  -fignore-interface-pragmas.  (We could revisit this decision.)
-}

-- | Codegen-generated Id infos, to be passed to downstream via interfaces.
--
-- This stuff is for optimization purposes only, they're not compulsory.
--
-- * When CafInfo of an imported Id is not known it's safe to treat it as CAFFY.
-- * When LambdaFormInfo of an imported Id is not known it's safe to treat it as
--   `LFUnknown True` (which just says "it could be anything" and we do slow
--   entry).
--
-- See also Note [Conveying CAF-info and LFInfo between modules] above.
--
data CgInfos = CgInfos
  { cgNonCafs :: !NonCaffySet
      -- ^ Exported Non-CAFFY closures in the current module. Everything else is
      -- either not exported of CAFFY.
  , cgLFInfos :: !ModuleLFInfos
      -- ^ LambdaFormInfos of exported closures in the current module.
  , cgIPEStub :: !CStub
      -- ^ The C stub which is used for IPE information
  , cgTagSigs :: !(NameEnv TagSig)
  }

--------------------------------------------------------------------------------
--                LambdaFormInfo
--------------------------------------------------------------------------------

-- | Maps names in the current module to their LambdaFormInfos
type ModuleLFInfos = NameEnv LambdaFormInfo

-- | Information about an identifier, from the code generator's point of view.
-- Every identifier is bound to a LambdaFormInfo in the environment, which gives
-- the code generator enough info to be able to tail call or return that
-- identifier.
data LambdaFormInfo
  = LFReEntrant         -- Reentrant closure (a function)
        !TopLevelFlag   -- True if top level
        !RepArity       -- Arity. Invariant: always > 0
        !Bool           -- True <=> no fvs
        !ArgDescr       -- Argument descriptor (should really be in ClosureInfo)

  | LFThunk             -- Thunk (zero arity)
        !TopLevelFlag
        !Bool           -- True <=> no free vars
        !Bool           -- True <=> updatable (i.e., *not* single-entry)
        !StandardFormInfo
        !Bool           -- True <=> *might* be a function type

  | LFCon               -- A saturated constructor application
        !DataCon        -- The constructor

  | LFUnknown           -- Used for function arguments and imported things.
                        -- We know nothing about this closure.
                        -- Treat like updatable "LFThunk"...
                        -- Imported things which we *do* know something about use
                        -- one of the other LF constructors (eg LFReEntrant for
                        -- known functions)
        !Bool           -- True <=> *might* be a function type
                        --      The False case is good when we want to enter it,
                        --        because then we know the entry code will do
                        --        For a function, the entry code is the fast entry point

  | LFUnlifted          -- A value of unboxed type;
                        -- always a value, needs evaluation

  | LFLetNoEscape       -- See LetNoEscape module for precise description

instance Outputable LambdaFormInfo where
    ppr (LFReEntrant top rep fvs argdesc) =
      text "LFReEntrant" <> brackets
        (ppr top <+> ppr rep <+> pprFvs fvs <+> ppr argdesc)
    ppr (LFThunk top hasfv updateable sfi m_function) =
      text "LFThunk" <> brackets
        (ppr top <+> pprFvs hasfv <+> pprUpdateable updateable <+>
         ppr sfi <+> pprFuncFlag m_function)
    ppr (LFCon con) =
      text "LFCon" <> brackets (ppr con)
    ppr (LFUnknown m_func) =
      text "LFUnknown" <> brackets (pprFuncFlag m_func)
    ppr LFUnlifted =
      text "LFUnlifted"
    ppr LFLetNoEscape =
      text "LFLetNoEscape"

pprFvs :: Bool -> SDoc
pprFvs True = text "no-fvs"
pprFvs False = text "fvs"

pprFuncFlag :: Bool -> SDoc
pprFuncFlag True = text "mFunc"
pprFuncFlag False = text "value"

pprUpdateable :: Bool -> SDoc
pprUpdateable True = text "updateable"
pprUpdateable False = text "oneshot"

--------------------------------------------------------------------------------
-- | StandardFormInfo tells whether this thunk has one of a small number of
-- standard forms

data StandardFormInfo
  = NonStandardThunk
        -- The usual case: not of the standard forms

  | SelectorThunk
        -- A SelectorThunk is of form
        --      case x of
        --           con a1,..,an -> ak
        -- and the constructor is from a single-constr type.
       !WordOff         -- 0-origin offset of ak within the "goods" of
                        -- constructor (Recall that the a1,...,an may be laid
                        -- out in the heap in a non-obvious order.)

  | ApThunk
        -- An ApThunk is of form
        --        x1 ... xn
        -- The code for the thunk just pushes x2..xn on the stack and enters x1.
        -- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
        -- in the RTS to save space.
        !RepArity       -- Arity, n
  deriving (Eq)

instance Outputable StandardFormInfo where
  ppr NonStandardThunk = text "RegThunk"
  ppr (SelectorThunk w) = text "SelThunk:" <> ppr w
  ppr (ApThunk n) = text "ApThunk:" <> ppr n

--------------------------------------------------------------------------------
--                Gaining sight in a sea of blindness
--------------------------------------------------------------------------------
type DoSCCProfiling = Bool
type DoExtDynRefs   = Bool
