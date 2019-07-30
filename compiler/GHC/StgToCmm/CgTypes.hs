{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

-- | Code generation related types used in other parts of the compiler.
--
-- In particular we export here any information which might be written to or
-- read from interface files.

module GHC.StgToCmm.CgTypes
  ( LambdaFormInfo(..), StandardFormInfo(..), CgIfaceInfo, CgIfaceInfoList
  , exportLF
  , whnfLF
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes

import Outputable
import SMRep
import DataCon
import NameEnv
import Name

{- Note [Backend information in interface files]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is desireable to store certain information in interface files
which only becomes obvious during code generation.

One of these cases is the information required to tag or enter
a reference via the fast path. This info is contained in LambdaFormInfo.

This allows the code generate to decide on an accurate call pattern
potentially avoiding slow calls. It also allows us to tag references
to constructors correctly avoiding the need to enter their closure.

The information is gathered while running the backend inside the stream.
Once the stream has run we write out the information into interface files
as usual.

-}

-----------------------------------------------------------------------------
--                General types
-----------------------------------------------------------------------------

-- | Information about imported things coming from interface files.
--   A map for faster lookup.
type CgIfaceInfo = NameEnv LambdaFormInfo
-- | The information in list form as it will be writting to the interface
--   file.
type CgIfaceInfoList = [(Name,LambdaFormInfo)]

-- | Recursivity Flag
data FreeVarFlag = HasFreeVars
                 | NoFreeVars
                 deriving (Eq,Enum,Bounded)

instance Outputable FreeVarFlag where
  ppr HasFreeVars = text "hasFvs"
  ppr NoFreeVars  = text "noFvs"

data LfUpdateable = Updateable
                  | NotUpdateable
                  deriving stock (Eq,Enum,Bounded)
                  -- deriving via Binary
                  --       via (BoundedEnumBinary LfUpdateable)

data LfValueFlag = Value            -- ^ Definitely a value (eg. Nothing)
                 | MaybeFunction    -- ^ Might be a function.
                 deriving stock (Eq,Enum,Bounded)

-----------------------------------------------------------------------------
--                LambdaFormInfo
-----------------------------------------------------------------------------

-- Information about an identifier, from the code generator's point of
-- view.  Every identifier is bound to a LambdaFormInfo in the
-- environment, which gives the code generator enough info to be able to
-- tail call or return that identifier. We also share this view via interface
-- files for exported ids.

data LambdaFormInfo
  = LFReEntrant         -- ^ Reentrant closure (a function)
        { lf_top        :: !TopLevelFlag  -- ^ True if top level
        , lf_os_info    :: !OneShotInfo
        , lf_repArity   :: !RepArity      -- ^ Arity. Invariant: always > 0
        , lf_no_fvs     :: !Bool   -- ^ True <=> no fvs
        , lf_arg_desc   :: ArgDescr      -- ^ Argument descriptor
                                         -- (should really be in ClosureInfo)
        }

  | LFThunk             -- Thunk (zero arity)
        TopLevelFlag
        !Bool           -- True <=> no free vars
        !Bool           -- True <=> updatable (i.e., *not* single-entry)
        !StandardFormInfo
        !Bool           -- True <=> *might* be a function type

  | LFCon               -- A saturated constructor application
        DataCon         -- The constructor

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
  deriving (Eq)

-- | Force fields of LambdaFormInfo into whnf.
whnfLF :: LambdaFormInfo -> LambdaFormInfo
whnfLF lf@LFLetNoEscape = lf
whnfLF lf@LFUnlifted = lf
whnfLF lf@(LFUnknown !_a1) = lf
whnfLF lf@(LFCon !_a1) = lf
whnfLF lf@(LFThunk !_a1 _ _ _ _) = lf
whnfLF lf@LFReEntrant { lf_arg_desc = !_a1 } = lf

-- | Should we export this info?
-- There is no point in exporting LFUnlifted,
-- it is easily recreated based on an ids type.
exportLF :: LambdaFormInfo -> Bool
exportLF LFUnlifted     = False
exportLF _              = True

pprFvs :: Bool -> SDoc
pprFvs True = text "no-fvs"
pprFvs False = text "fvs"

pprFuncFlag :: Bool -> SDoc
pprFuncFlag True = text "mFunc"
pprFuncFlag False = text "value"

pprUpdateable :: Bool -> SDoc
pprUpdateable True = text "updateable"
pprUpdateable False = text "oneshot"

instance Outputable LambdaFormInfo where
    ppr (LFReEntrant top oneshot rep fvs argdesc) =
        text "LFReEntrant" <> brackets (ppr top <+> ppr oneshot <+>
                                        ppr rep <+> pprFvs fvs <+> ppr argdesc)
    ppr (LFThunk top hasfv updateable sfi m_function) =
        text "LFThunk" <> brackets (ppr top <+> pprFvs hasfv <+> pprUpdateable updateable <+>
                                    ppr sfi <+> pprFuncFlag m_function)
    ppr (LFCon con) = text "LFCon" <> brackets (ppr con)
    ppr (LFUnknown m_func) =
        text "LFUnknown" <> brackets (pprFuncFlag m_func)
    ppr (LFUnlifted) = text "LFUnlifted"
    ppr (LFLetNoEscape) = text "LF-LNE"

-------------------------
-- StandardFormInfo tells whether this thunk has one of
-- a small number of standard forms

data StandardFormInfo
  = NonStandardThunk
        -- The usual case: not of the standard forms

  | SelectorThunk
        -- A SelectorThunk is of form
        --      case x of
        --           con a1,..,an -> ak
        -- and the constructor is from a single-constr type.
       !WordOff          -- 0-origin offset of ak within the "goods" of
                        -- constructor (Recall that the a1,...,an may be laid
                        -- out in the heap in a non-obvious order.)

  | ApThunk
        -- An ApThunk is of form
        --        x1 ... xn
        -- The code for the thunk just pushes x2..xn on the stack and enters x1.
        -- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
        -- in the RTS to save space.
        !RepArity                -- Arity, n
   deriving (Eq)

instance Outputable StandardFormInfo where
  ppr NonStandardThunk = text "RegThunk"
  ppr (SelectorThunk w) = text "SelThunk:" <> ppr w
  ppr (ApThunk n) = text "ApThunk:" <> ppr n
