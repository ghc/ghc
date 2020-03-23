{-# LANGUAGE CPP, DataKinds, GADTs, KindSignatures, LambdaCase #-}

module GHC.StgToCmm.Types
  ( WordOff

    -- * LambdaFormInfo types
  , LFIVariant (..), ImportedLFI, LocalLFI, LambdaFormInfo (..)
  , LFReEntrant (..), LFThunk (..)
  , ModuleLFInfos

    -- * LambdaFormInfo queries
  , lfr_top_lvl, lfr_one_shot, lfr_rep_arity, lfr_no_fvs, lfr_arg_descr
  , lft_top_lvl, lft_no_fvs, lft_updatable, lft_sfi, lft_mb_fun

    -- * Other stuff
  , toImportedLFI
  , Liveness
  , ArgDescr (..)
  , StandardFormInfo (..)
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes
import NameEnv
import Outputable
import Name

-- | Word offset, or word count
type WordOff = Int

--------------------------------------------------------------------------------
--                LambdaFormInfo
--------------------------------------------------------------------------------

-- | Type alias for LambdaFormInfos of imported things
type ImportedLFI = LambdaFormInfo 'LFI_Imported

-- | Type alias for LambdaForInfos of local things
type LocalLFI = LambdaFormInfo 'LFI_Local

-- | Maps names in the current module to their exported LambdaFormInfos
type ModuleLFInfos = NameEnv ImportedLFI

-- | LambdaFormInfo variants
data LFIVariant
  = LFI_Imported
  | LFI_Local

-- | LambdaFormInfo for a re-entrant closure (a function)
data LFReEntrant lfi_variant where
  LFR_Imported
    :: !RepArity
    -> LFReEntrant 'LFI_Imported

  LFR_Local
    :: !TopLevelFlag
    -> !OneShotInfo
    -> !RepArity
    -> !Bool
       -- ^ True <=> no fvs
    -> !ArgDescr
    -> LFReEntrant 'LFI_Local

-- | Lambda form info for a thunk (zero arity)
data LFThunk lfi_variant where
  LFT_Imported
    :: !Bool
       -- ^ True <=> updatable (i.e., *not* single-entry)
    -> !StandardFormInfo
    -> !Bool
       -- ^ True <=> *might* be a function type
    -> LFThunk 'LFI_Imported

  LFT_Local
    :: !TopLevelFlag
    -> !Bool
        -- ^ True <=> no fvs
    -> !Bool
       -- ^ True <=> updatable (i.e., *not* single-entry)
    -> !StandardFormInfo
    -> !Bool
       -- ^ True <=> *might* be a function type
    -> LFThunk 'LFI_Local

--------------------------------------------------------------------------------
--                LambdaFormInfo queries
--------------------------------------------------------------------------------

lfr_top_lvl :: LFReEntrant a -> TopLevelFlag
lfr_top_lvl = \case
  LFR_Imported _ -> TopLevel
  LFR_Local top_lvl _ _ _ _ -> top_lvl

lfr_one_shot :: LFReEntrant 'LFI_Local -> OneShotInfo
lfr_one_shot (LFR_Local _ one_shot _ _ _) = one_shot

lfr_rep_arity :: LFReEntrant a -> RepArity
lfr_rep_arity = \case
    LFR_Imported arity -> arity
    LFR_Local _ _ arity _ _ -> arity

lfr_no_fvs :: LFReEntrant a -> Bool
lfr_no_fvs = \case
    LFR_Imported _ -> True
    LFR_Local _ _ _ no_fvs _ -> no_fvs

lfr_arg_descr :: LFReEntrant 'LFI_Local -> ArgDescr
lfr_arg_descr (LFR_Local _ _ _ _ arg_descr) = arg_descr

lft_top_lvl :: LFThunk a -> TopLevelFlag
lft_top_lvl = \case
    LFT_Imported _ _ _ -> TopLevel
    LFT_Local top_lvl _ _ _ _ -> top_lvl

lft_no_fvs :: LFThunk a -> Bool
lft_no_fvs = \case
    LFT_Imported _ _ _ -> True
    LFT_Local _ no_fvs _ _ _ -> no_fvs

lft_updatable :: LFThunk a -> Bool
lft_updatable = \case
    LFT_Imported updatable _ _ -> updatable
    LFT_Local _ _ updatable _ _ -> updatable

lft_sfi :: LFThunk a -> StandardFormInfo
lft_sfi = \case
    LFT_Imported _ sfi _ -> sfi
    LFT_Local _ _ _ sfi _ -> sfi

lft_mb_fun :: LFThunk a -> Bool
lft_mb_fun = \case
    LFT_Imported _ _ mb_fun -> mb_fun
    LFT_Local _ _ _ _ mb_fun -> mb_fun

--------------------------------------------------------------------------------
--                Local LFI to imported LFI
--------------------------------------------------------------------------------

toImportedLFI :: LambdaFormInfo a -> ImportedLFI
toImportedLFI = \case
    LFReEntrant lfr -> LFReEntrant (toImportedLFR lfr)
    LFThunk lft -> LFThunk (toImportedLFT lft)
    LFCon name tag -> LFCon name tag
    LFUnknown mb_fun -> LFUnknown mb_fun
    LFUnlifted -> LFUnlifted
    LFLetNoEscape -> LFLetNoEscape -- TODO: This case should be unreachable

toImportedLFR :: LFReEntrant a -> LFReEntrant 'LFI_Imported
toImportedLFR = \case
    LFR_Imported arity -> LFR_Imported arity
    LFR_Local _ _ arity _ _ -> LFR_Imported arity

toImportedLFT :: LFThunk a -> LFThunk 'LFI_Imported
toImportedLFT = \case
    LFT_Imported updatable sfi mb_fun -> LFT_Imported updatable sfi mb_fun
    LFT_Local _ _ updatable sfi mb_fun -> LFT_Imported updatable sfi mb_fun

--------------------------------------------------------------------------------

-- | Information about an identifier, from the code generator's point of view.
--
-- Local identifiers are bound to a LambdaFormInfo in the environment, which
-- gives the code generator enough info to be able to tail call or return that
-- identifier.
--
-- Imported identifiers have the information in idLFInfo field.
data LambdaFormInfo (lfi_variant :: LFIVariant)
  = LFReEntrant         -- Reentrant closure (a function)
        !(LFReEntrant lfi_variant)

  | LFThunk             -- Thunk (zero arity)
        !(LFThunk lfi_variant)

  | LFCon               -- A saturated constructor application
        !Name           -- Name of the constructor
        !ConTag         -- The constructor's (1-based) tag

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

  -- TODO: This should only be available for local LFIs
  | LFLetNoEscape       -- See LetNoEscape module for precise description

instance Outputable (LFReEntrant a) where
    ppr (LFR_Imported arity ) =
        text "LFReEntrant" <> brackets (ppr arity)

    ppr (LFR_Local top one_shot arity no_fvs arg_desc) =
        text "LFReEntrant" <> brackets
          (ppr top <+> ppr one_shot <+>
           ppr arity <+> pprFvs no_fvs <+> ppr arg_desc)

instance Outputable (LFThunk a) where
    ppr (LFT_Imported updatable sfi mb_fun) =
        text "LFThunk" <> brackets (hcat
          [ text "upd=" <> ppr updatable
          , text "sfi=" <> ppr sfi
          , text "mb_fun=" <> ppr mb_fun
          ])

    ppr (LFT_Local top no_fvs updatable sfi mb_fun) =
        text "LFThunk" <> brackets (hcat
          [ text "top_lvl=" <> ppr top
          , text "no_fvs=" <> ppr no_fvs
          , text "updatable=" <> ppr updatable
          , text "sfi=" <> ppr sfi
          , text "mb_fun=" <> ppr mb_fun
          ])

instance Outputable (LambdaFormInfo a) where
    ppr (LFReEntrant lfr) = ppr lfr
    ppr (LFThunk lft) = ppr lft
    ppr (LFCon name _tag) = text "LFCon" <> brackets (ppr name)
    ppr (LFUnknown mb_fun) = text "LFUnknown" <> brackets (pprFuncFlag mb_fun)
    ppr LFUnlifted = text "LFUnlifted"
    ppr LFLetNoEscape = text "LFLetNoEscape"

pprFvs :: Bool -> SDoc
pprFvs True = text "no-fvs"
pprFvs False = text "fvs"

pprFuncFlag :: Bool -> SDoc
pprFuncFlag True = text "mFunc"
pprFuncFlag False = text "value"

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
  deriving (Eq)

instance Outputable ArgDescr where
  ppr (ArgSpec n) = text "ArgSpec" <+> ppr n
  ppr (ArgGen ls) = text "ArgGen" <+> ppr ls

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
  deriving (Eq)

instance Outputable StandardFormInfo where
  ppr NonStandardThunk = text "RegThunk"
  ppr (SelectorThunk w) = text "SelThunk:" <> ppr w
  ppr (ApThunk n) = text "ApThunk:" <> ppr n
