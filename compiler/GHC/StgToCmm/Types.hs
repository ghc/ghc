

module GHC.StgToCmm.Types
  ( CmmCgInfos (..)
  , LambdaFormInfo (..)
  , ModuleLFInfos
  , StandardFormInfo (..)
  , DoSCCProfiling
  , DoExtDynRefs
  ) where

import GHC.Prelude

import GHC.Core.DataCon

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
  LambdaFormInfo in the interface, in GHC.StgToCmm.Closure.importedIdLFInfo.

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

Note [Imported unlifted nullary datacon wrappers must have correct LFInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in `Note [Conveying CAF-info and LFInfo between modules]`,
imported unlifted nullary datacons must have their LambdaFormInfo set to
reflect the fact that they are evaluated. This is necessary as otherwise
references to them may be passed untagged to code that expects tagged
references because of the unlifted nature of the argument.

For example, in

   type T :: UnliftedType
   data T = T1
          | T2

   f :: T -> Int
   f x = case x of T1 -> 1; T2 -> 2

`f` expects `x` to be evaluated and properly tagged due to its unliftedness.
We can guarantee all occurrences of `T1` and `T2` are considered evaluated and
are properly tagged by giving them the `LFCon` LambdaFormInfo which indicates
they are fully saturated constructor applications.
(The LambdaFormInfo is used to tag the pointer with the tag of the
constructor, in `litIdInfo`)

What may be less obvious is that this must be done for not only datacon
workers but also *wrappers*. The reason is found in this program
from #23146:

    module B where

    type NP :: [UnliftedType] -> UnliftedType
    data NP xs where
      UNil :: NP '[]


    module A where
    import B

    fieldsSam :: NP xs -> NP xs -> Bool
    fieldsSam UNil UNil = True

    x = fieldsSam UNil UNil

Due to its GADT nature, `B.UNil` produces a trivial wrapper

    $WUNil :: NP '[]
    $WUNil = UNil @'[] @~(<co:1>)

which is referenced in the RHS of `A.x`. If we fail to give `$WUNil` the
correct `LFCon 0` `LambdaFormInfo` then we will end up passing an untagged
pointer to `fieldsSam`. This is problematic as `fieldsSam` may take advantage
of the unlifted nature of its arguments by omitting handling of the zero
tag when scrutinising them.

The fix is straightforward: ensure we always construct a /correct/ LFInfo for
datacon workers and wrappers, and populate the `lfInfo` with it. See
Note [LFInfo of DataCon workers and wrappers]. This fixed #23146.

See also Note [The LFInfo of Imported Ids]
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
data CmmCgInfos = CmmCgInfos
  { cgNonCafs :: !NonCaffySet
      -- ^ Exported Non-CAFFY closures in the current module. Everything else is
      -- either not exported of CAFFY.
  , cgLFInfos :: !ModuleLFInfos
      -- ^ LambdaFormInfos of exported closures in the current module.
  , cgIPEStub :: !CStub
      -- ^ The C stub which is used for IPE information
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

  | LFCon               -- A saturated data constructor application
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
