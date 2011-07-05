----------------------------------------------------------------------------
--
-- Pretty-printing of common Cmm types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

--
-- This is where we walk over Cmm emitting an external representation,
-- suitable for parsing, in a syntax strongly reminiscent of C--. This
-- is the "External Core" for the Cmm layer.
--
-- As such, this should be a well-defined syntax: we want it to look nice.
-- Thus, we try wherever possible to use syntax defined in [1],
-- "The C-- Reference Manual", http://www.cminusminus.org/. We differ
-- slightly, in some cases. For one, we use I8 .. I64 for types, rather
-- than C--'s bits8 .. bits64.
--
-- We try to ensure that all information available in the abstract
-- syntax is reproduced, or reproducible, in the concrete syntax.
-- Data that is not in printed out can be reconstructed according to
-- conventions used in the pretty printer. There are at least two such
-- cases:
--      1) if a value has wordRep type, the type is not appended in the
--      output.
--      2) MachOps that operate over wordRep type are printed in a
--      C-style, rather than as their internal MachRep name.
--
-- These conventions produce much more readable Cmm output.
--
-- A useful example pass over Cmm is in nativeGen/MachCodeGen.hs
--

module PprCmmDecl
    ( writeCmms, pprCmms, pprCmm, pprSection, pprStatic
    )
where

import CmmDecl
import CLabel
import PprCmmExpr


import Outputable
import FastString

import Data.List
import System.IO

-- Temp Jan08
import SMRep
import ClosureInfo
#include "../includes/rts/storage/FunTypes.h"


pprCmms :: (Outputable info, Outputable g) => [GenCmm CmmStatics info g] -> SDoc
pprCmms cmms = pprCode CStyle (vcat (intersperse separator $ map ppr cmms))
        where
          separator = space $$ ptext (sLit "-------------------") $$ space

writeCmms :: (Outputable info, Outputable g) => Handle -> [GenCmm CmmStatics info g] -> IO ()
writeCmms handle cmms = printForC handle (pprCmms cmms)

-----------------------------------------------------------------------------

instance (Outputable d, Outputable info, Outputable g)
    => Outputable (GenCmm d info g) where
    ppr c = pprCmm c

instance (Outputable d, Outputable info, Outputable i)
	=> Outputable (GenCmmTop d info i) where
    ppr t = pprTop t

instance Outputable CmmStatics where
    ppr e = pprStatics e

instance Outputable CmmStatic where
    ppr e = pprStatic e

instance Outputable CmmInfoTable where
    ppr e = pprInfoTable e


-----------------------------------------------------------------------------

pprCmm :: (Outputable d, Outputable info, Outputable g) => GenCmm d info g -> SDoc
pprCmm (Cmm tops) = vcat $ intersperse blankLine $ map pprTop tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop 	:: (Outputable d, Outputable info, Outputable i)
	=> GenCmmTop d info i -> SDoc

pprTop (CmmProc info lbl graph)

  = vcat [ pprCLabel lbl <> lparen <> rparen
         , nest 8 $ lbrace <+> ppr info $$ rbrace
         , nest 4 $ ppr graph
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--
pprTop (CmmData section ds) = 
    (hang (pprSection section <+> lbrace) 4 (ppr ds))
    $$ rbrace

-- --------------------------------------------------------------------------
-- Info tables.

pprInfoTable :: CmmInfoTable -> SDoc
pprInfoTable CmmNonInfoTable = empty
pprInfoTable (CmmInfoTable stat_clos (ProfilingInfo closure_type closure_desc) tag info) =
    vcat [ptext (sLit "has static closure: ") <> ppr stat_clos <+>
          ptext (sLit "type: ") <> pprLit closure_type,
          ptext (sLit "desc: ") <> pprLit closure_desc,
          ptext (sLit "tag: ") <> integer (toInteger tag),
          pprTypeInfo info]

pprTypeInfo :: ClosureTypeInfo -> SDoc
pprTypeInfo (ConstrInfo layout constr descr) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "constructor: ") <> integer (toInteger constr),
          pprLit descr]
pprTypeInfo (FunInfo layout srt arity _args slow_entry) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "srt: ") <> ppr srt,
-- Temp Jan08
          ptext (sLit ("fun_type: ")) <> integer (toInteger (argDescrType _args)),

          ptext (sLit "arity: ") <> integer (toInteger arity),
          --ptext (sLit "args: ") <> ppr args, -- TODO: needs to be printed
          ptext (sLit "slow: ") <> pprLit slow_entry
         ]
pprTypeInfo (ThunkInfo layout srt) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger (fst layout)),
          ptext (sLit "nptrs: ") <> integer (toInteger (snd layout)),
          ptext (sLit "srt: ") <> ppr srt]
pprTypeInfo (ThunkSelectorInfo offset srt) =
    vcat [ptext (sLit "ptrs: ") <> integer (toInteger offset),
          ptext (sLit "srt: ") <> ppr srt]
pprTypeInfo (ContInfo stack srt) =
    vcat [ptext (sLit "stack: ") <> ppr stack,
          ptext (sLit "srt: ") <> ppr srt]

-- Temp Jan08
argDescrType :: ArgDescr -> StgHalfWord
-- The "argument type" RTS field type
argDescrType (ArgSpec n) = n
argDescrType (ArgGen liveness)
  | isBigLiveness liveness = ARG_GEN_BIG
  | otherwise		   = ARG_GEN

-- Temp Jan08
isBigLiveness :: Liveness -> Bool
isBigLiveness (BigLiveness _)   = True
isBigLiveness (SmallLiveness _) = False

instance Outputable ForeignHint where
  ppr NoHint     = empty
  ppr SignedHint = quotes(text "signed")
--  ppr AddrHint   = quotes(text "address")
-- Temp Jan08
  ppr AddrHint   = (text "PtrHint")

-- --------------------------------------------------------------------------
-- Static data.
--      Strings are printed as C strings, and we print them as I8[],
--      following C--
--
pprStatics :: CmmStatics -> SDoc
pprStatics (Statics lbl ds) = vcat ((pprCLabel lbl <> colon) : map ppr ds)

pprStatic :: CmmStatic -> SDoc
pprStatic s = case s of
    CmmStaticLit lit   -> nest 4 $ ptext (sLit "const") <+> pprLit lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Section -> SDoc
pprSection s = case s of
    Text              -> section <+> doubleQuotes (ptext (sLit "text"))
    Data              -> section <+> doubleQuotes (ptext (sLit "data"))
    ReadOnlyData      -> section <+> doubleQuotes (ptext (sLit "readonly"))
    ReadOnlyData16    -> section <+> doubleQuotes (ptext (sLit "readonly16"))
    RelocatableReadOnlyData
                      -> section <+> doubleQuotes (ptext (sLit "relreadonly"))
    UninitialisedData -> section <+> doubleQuotes (ptext (sLit "uninitialised"))
    OtherSection s'   -> section <+> doubleQuotes (text s')
 where
    section = ptext (sLit "section")
