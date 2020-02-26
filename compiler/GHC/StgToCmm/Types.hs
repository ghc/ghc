{-# LANGUAGE CPP #-}

module GHC.StgToCmm.Types
  ( CgInfos (..)
  , LambdaFormInfo (..)
  , ModuleLFInfos
  , Liveness
  , ArgDescr (..)
  , StandardFormInfo (..)
  , WordOff
  ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Types.Basic
import GHC.Core.DataCon
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import Outputable

-- | Codegen-generated Id infos, to be passed to downstream via interfaces.
--
-- This stuff is for optimization purposes only, they're not compulsory.
--
-- * When CafInfo of an imported Id is not known it's safe to treat it as CAFFY.
-- * When LambdaFormInfo of an imported Id is not known it's safe to treat it as
--   `LFUnknown True` (which just says "it could be anything" and we do slow
--   entry).
--
data CgInfos = CgInfos
  { cgNonCafs :: !NameSet
      -- ^ Exported Non-CAFFY closures in the current module. Everything else is
      -- either not exported of CAFFY.
  , cgLFInfos :: !ModuleLFInfos
      -- ^ LambdaFormInfos of exported closures in the current module.
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
        !OneShotInfo
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
    ppr (LFReEntrant top oneshot rep fvs argdesc) =
        text "LFReEntrant" <> brackets (ppr top <+> ppr oneshot <+>
                                        ppr rep <+> pprFvs fvs <+> ppr argdesc)
    ppr (LFThunk top hasfv updateable sfi m_function) =
        text "LFThunk" <> brackets (ppr top <+> pprFvs hasfv <+> pprUpdateable updateable <+>
                                    ppr sfi <+> pprFuncFlag m_function)
    ppr (LFCon con) = text "LFCon" <> brackets (ppr con)
    ppr (LFUnknown m_func) = text "LFUnknown" <> brackets (pprFuncFlag m_func)
    ppr LFUnlifted = text "LFUnlifted"
    ppr LFLetNoEscape = text "LFLetNoEscape"

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

-- | We represent liveness bitmaps as a Bitmap (whose internal representation
-- really is a bitmap).  These are pinned onto case return vectors to indicate
-- the state of the stack for the garbage collector.
--
-- In the compiled program, liveness bitmaps that fit inside a single word
-- (StgWord) are stored as a single word, while larger bitmaps are stored as a
-- pointer to an array of words.

type Liveness = [Bool]   -- One Bool per word; True  <=> non-ptr or dead
                         --                    False <=> ptr

--------------------------------------------------------------------------------
-- | An ArgDescr describes the argument pattern of a function

data ArgDescr
  = ArgSpec             -- Fits one of the standard patterns
        !Int            -- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen              -- General case
        Liveness        -- Details about the arguments

  | ArgUnknown          -- For imported binds.
                        -- Invariant: Never Unknown for binds of the module
                        -- we are compiling.
  deriving (Eq)

instance Outputable ArgDescr where
  ppr (ArgSpec n) = text "ArgSpec" <+> ppr n
  ppr (ArgGen ls) = text "ArgGen" <+> ppr ls
  ppr ArgUnknown = text "ArgUnknown"

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

-- | Word offset, or word count
type WordOff = Int

instance Outputable StandardFormInfo where
  ppr NonStandardThunk = text "RegThunk"
  ppr (SelectorThunk w) = text "SelThunk:" <> ppr w
  ppr (ApThunk n) = text "ApThunk:" <> ppr n
