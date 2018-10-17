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
-- "The C-- Reference Manual", http://www.cs.tufts.edu/~nr/c--/index.html. We
-- differ slightly, in some cases. For one, we use I8 .. I64 for types, rather
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
module PprCmmDecl
    ( writeCmms, pprCmms, pprCmmGroup, pprSection, pprStatic
    )
where

import GhcPrelude

import PprCmmExpr
import Cmm

import DynFlags
import Outputable
import FastString

import Data.List
import System.IO

-- Temp Jan08
import SMRep


pprCmms :: (Outputable info, Outputable g)
        => [GenCmmGroup CmmStatics info g] -> SDoc
pprCmms cmms = pprCode CStyle (vcat (intersperse separator $ map ppr cmms))
        where
          separator = space $$ text "-------------------" $$ space

writeCmms :: (Outputable info, Outputable g)
          => DynFlags -> Handle -> [GenCmmGroup CmmStatics info g] -> IO ()
writeCmms dflags handle cmms = printForC dflags handle (pprCmms cmms)

-----------------------------------------------------------------------------

instance (Outputable d, Outputable info, Outputable i)
      => Outputable (GenCmmDecl d info i) where
    ppr t = pprTop t

instance Outputable CmmStatics where
    ppr = pprStatics

instance Outputable CmmStatic where
    ppr = pprStatic

instance Outputable CmmInfoTable where
    ppr = pprInfoTable


-----------------------------------------------------------------------------

pprCmmGroup :: (Outputable d, Outputable info, Outputable g)
            => GenCmmGroup d info g -> SDoc
pprCmmGroup tops
    = vcat $ intersperse blankLine $ map pprTop tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop :: (Outputable d, Outputable info, Outputable i)
       => GenCmmDecl d info i -> SDoc

pprTop (CmmProc info lbl live graph)

  = vcat [ ppr lbl <> lparen <> rparen <+> text "// " <+> ppr live
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
pprInfoTable (CmmInfoTable { cit_lbl = lbl, cit_rep = rep
                           , cit_prof = prof_info
                           , cit_srt = srt })
  = vcat [ text "label: " <> ppr lbl
         , text "rep: " <> ppr rep
         , case prof_info of
             NoProfilingInfo -> empty
             ProfilingInfo ct cd ->
               vcat [ text "type: " <> pprWord8String ct
                    , text "desc: " <> pprWord8String cd ]
         , text "srt: " <> ppr srt ]

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
pprStatics (Statics lbl ds) = vcat ((ppr lbl <> colon) : map ppr ds)

pprStatic :: CmmStatic -> SDoc
pprStatic s = case s of
    CmmStaticLit lit   -> nest 4 $ text "const" <+> pprLit lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Section -> SDoc
pprSection (Section t suffix) =
  section <+> doubleQuotes (pprSectionType t <+> char '.' <+> ppr suffix)
  where
    section = text "section"

pprSectionType :: SectionType -> SDoc
pprSectionType s = doubleQuotes (ptext t)
 where
  t = case s of
    Text              -> sLit "text"
    Data              -> sLit "data"
    ReadOnlyData      -> sLit "readonly"
    ReadOnlyData16    -> sLit "readonly16"
    RelocatableReadOnlyData
                      -> sLit "relreadonly"
    UninitialisedData -> sLit "uninitialised"
    CString           -> sLit "cstring"
    OtherSection s'   -> sLit s' -- Not actually a literal though.
