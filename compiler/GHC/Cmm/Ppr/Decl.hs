{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


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
module GHC.Cmm.Ppr.Decl
    ( pprCmms, pprCmmGroup, pprSection, pprStatic
    )
where

import GHC.Prelude

import GHC.Platform
import GHC.Cmm.Ppr.Expr
import GHC.Cmm

import GHC.Utils.Outputable

import Data.List (intersperse)

import qualified Data.ByteString as BS


pprCmms :: (OutputableP Platform info, OutputableP Platform g)
        => Platform -> [GenCmmGroup RawCmmStatics info g] -> SDoc
pprCmms platform cmms = pprCode CStyle (vcat (intersperse separator $ map (pdoc platform) cmms))
        where
          separator = space $$ text "-------------------" $$ space

-----------------------------------------------------------------------------

instance (OutputableP Platform d, OutputableP Platform info, OutputableP Platform i)
      => OutputableP Platform (GenCmmDecl d info i) where
    pdoc = pprTop

instance OutputableP Platform (GenCmmStatics a) where
    pdoc = pprStatics

instance OutputableP Platform CmmStatic where
    pdoc = pprStatic

instance OutputableP Platform CmmInfoTable where
    pdoc = pprInfoTable


-----------------------------------------------------------------------------

pprCmmGroup :: (OutputableP Platform d, OutputableP Platform info, OutputableP Platform g)
            => Platform -> GenCmmGroup d info g -> SDoc
pprCmmGroup platform tops
    = vcat $ intersperse blankLine $ map (pprTop platform) tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop :: (OutputableP Platform d, OutputableP Platform info, OutputableP Platform i)
       => Platform -> GenCmmDecl d info i -> SDoc

pprTop platform (CmmProc info lbl live graph)

  = vcat [ pdoc platform lbl <> lparen <> rparen <+> lbrace <+> text "// " <+> ppr live
         , nest 8 $ lbrace <+> pdoc platform info $$ rbrace
         , nest 4 $ pdoc platform graph
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--
pprTop platform (CmmData section ds) =
    (hang (pprSection platform section <+> lbrace) 4 (pdoc platform ds))
    $$ rbrace

-- --------------------------------------------------------------------------
-- Info tables.

pprInfoTable :: Platform -> CmmInfoTable -> SDoc
pprInfoTable platform (CmmInfoTable { cit_lbl = lbl, cit_rep = rep
                           , cit_prof = prof_info
                           , cit_srt = srt })
  = vcat [ text "label: " <> pdoc platform lbl
         , text "rep: " <> ppr rep
         , case prof_info of
             NoProfilingInfo -> empty
             ProfilingInfo ct cd ->
               vcat [ text "type: " <> text (show (BS.unpack ct))
                    , text "desc: " <> text (show (BS.unpack cd)) ]
         , text "srt: " <> pdoc platform srt ]

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

pprStatics :: Platform -> GenCmmStatics a -> SDoc
pprStatics platform (CmmStatics lbl itbl ccs payload) =
  pdoc platform lbl <> colon <+> pdoc platform itbl <+> ppr ccs <+> pdoc platform payload
pprStatics platform (CmmStaticsRaw lbl ds) = vcat ((pdoc platform lbl <> colon) : map (pprStatic platform) ds)

pprStatic :: Platform -> CmmStatic -> SDoc
pprStatic platform s = case s of
    CmmStaticLit lit   -> nest 4 $ text "const" <+> pprLit platform lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')
    CmmFileEmbed path  -> nest 4 $ text "incbin " <+> text (show path)

-- --------------------------------------------------------------------------
-- data sections
--
pprSection :: Platform -> Section -> SDoc
pprSection platform (Section t suffix) =
  section <+> doubleQuotes (pprSectionType t <+> char '.' <+> pdoc platform suffix)
  where
    section = text "section"

pprSectionType :: SectionType -> SDoc
pprSectionType s = doubleQuotes $ case s of
  Text                    -> text "text"
  Data                    -> text "data"
  ReadOnlyData            -> text "readonly"
  ReadOnlyData16          -> text "readonly16"
  RelocatableReadOnlyData -> text "relreadonly"
  UninitialisedData       -> text "uninitialised"
  InitArray               -> text "initarray"
  FiniArray               -> text "finiarray"
  CString                 -> text "cstring"
  OtherSection s'         -> text s'
