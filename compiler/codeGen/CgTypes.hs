{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Code generation related types used in other parts of the compiler.

-- In particular we define here any information which might be written to or
-- read from interface files.
-- Inside the codegenerator this module should in general not be imported.
-- Instead the types are reexported by StgCmmClosure


module CgTypes
  ( LambdaFormInfo(..), StandardFormInfo(..), CgInfoImported
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes

import Binary
import Outputable
import SMRep
import DataCon
import NameEnv

-----------------------------------------------------------------------------
--                General types
-----------------------------------------------------------------------------

-- | Information about imported things coming from interface files.
type CgInfoImported = NameEnv LambdaFormInfo

-----------------------------------------------------------------------------
--                LambdaFormInfo
-----------------------------------------------------------------------------

-- Information about an identifier, from the code generator's point of
-- view.  Every identifier is bound to a LambdaFormInfo in the
-- environment, which gives the code generator enough info to be able to
-- tail call or return that identifier.

data LambdaFormInfo
  = LFReEntrant         -- Reentrant closure (a function)
        TopLevelFlag    -- True if top level
        OneShotInfo
        !RepArity       -- Arity. Invariant: always > 0
        !Bool           -- True <=> no fvs
        ArgDescr        -- Argument descriptor (should really be in ClosureInfo)

  | LFThunk             -- Thunk (zero arity)
        TopLevelFlag
        !Bool           -- True <=> no free vars
        !Bool           -- True <=> updatable (i.e., *not* single-entry)
        StandardFormInfo
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

instance Outputable LambdaFormInfo where
    ppr (LFReEntrant top oneshot rep fvs argdesc) =
        text "LFReEntrant" <> brackets (ppr top <+> ppr oneshot <+>
                                        ppr rep <+> ppr fvs <+> ppr argdesc)
    ppr (LFThunk top hasfv updateable sfi m_function) =
        text "LFThunk" <> brackets (ppr top <+> ppr hasfv <+> ppr updateable <+>
                                    ppr sfi <+> ppr m_function)
    ppr (LFCon con) = text "LFCon" <> brackets (ppr con)
    ppr (LFUnknown m_func) =
        text "LFUnknown" <>
            if m_func
                then brackets (text "mf")
                else empty
    ppr (LFUnlifted) = text "LFUnlifted"
    ppr (LFLetNoEscape) = text "LF-LNE"

-- instance Binary LambdaFormInfo where
--     put_ bh (LFReEntrant top oneshot rep fvs argdesc) =
--         putByte bh 0 >>
--         put_ bh top >>
--         put_ bh oneshot >>
--         put_ bh rep >>
--         put_ bh fvs >>
--         put_ bh argdesc
--     put_ bh (LFThunk top hasfv updateable sfi m_function) =
--         putByte bh 1 >>
--         put_ bh top >>
--         put_ bh hasfv >>
--         put_ bh updateable >>
--         put_ bh sfi >>
--         put_ bh m_function
--     put_ bh (LFCon con) = putByte bh 2 -- >> put_ bh (dataConName con)
--     put_ _h (LFUnknown _m_func) =
--       panic "We should never export Unknown info"
--     put_ bh (LFUnlifted) = putByte bh 4
--     put_ bh (LFLetNoEscape) = putByte bh 5
--     get bh = do
--         con <- getByte bh
--         case con of
--             0 -> pure LFReEntrant <*> get bh <*> get bh <*> get bh <*> get bh <*> get bh
--             1 -> pure LFThunk <*> get bh <*> get bh <*> get bh <*> get bh <*> get bh
--             2 -> pure $ LFCon nilDataCon
--             3 -> pure LFUnknown <*> get bh
--             4 -> pure LFUnlifted
--             5 -> pure LFLetNoEscape
--             _ -> panic "Invalid byte"


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
       WordOff          -- 0-origin offset of ak within the "goods" of
                        -- constructor (Recall that the a1,...,an may be laid
                        -- out in the heap in a non-obvious order.)

  | ApThunk
        -- An ApThunk is of form
        --        x1 ... xn
        -- The code for the thunk just pushes x2..xn on the stack and enters x1.
        -- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
        -- in the RTS to save space.
        RepArity                -- Arity, n

instance Binary StandardFormInfo where
  put_ bh NonStandardThunk  = putByte bh 0
  put_ bh (SelectorThunk w) = putByte bh 1 >> put_ bh w
  put_ bh (ApThunk n)       = putByte bh 2 >> put_ bh n
  get  bh = do
    con <- getByte bh
    case con of
        0 -> pure NonStandardThunk
        1 -> pure SelectorThunk <*> get bh
        2 -> pure ApThunk       <*> get bh
        _ -> panic "Invalid Byte"

instance Outputable StandardFormInfo where
  ppr NonStandardThunk = text "RegThunk"
  ppr (SelectorThunk w) = text "SelThunk:" <> ppr w
  ppr (ApThunk n) = text "ApThunk:" <> ppr n

-- -----------------------------------------------------------------------------
-- Representation inside interface files.

