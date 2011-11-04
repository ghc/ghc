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

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PprCmmDecl
    ( writeCmms, pprCmms, pprCmmGroup, pprSection, pprStatic
    )
where

import CLabel
import PprCmmExpr
import Cmm

import Outputable
import Platform
import FastString

import Data.List
import System.IO

-- Temp Jan08
import SMRep
#include "../includes/rts/storage/FunTypes.h"


pprCmms :: (PlatformOutputable info, PlatformOutputable g)
        => Platform -> [GenCmmGroup CmmStatics info g] -> SDoc
pprCmms platform cmms = pprCode CStyle (vcat (intersperse separator $ map (pprPlatform platform) cmms))
        where
          separator = space $$ ptext (sLit "-------------------") $$ space

writeCmms :: (PlatformOutputable info, PlatformOutputable g)
          => Platform -> Handle -> [GenCmmGroup CmmStatics info g] -> IO ()
writeCmms platform handle cmms = printForC handle (pprCmms platform cmms)

-----------------------------------------------------------------------------

instance (PlatformOutputable d, PlatformOutputable info, PlatformOutputable i)
      => PlatformOutputable (GenCmmDecl d info i) where
    pprPlatform platform t = pprTop platform t

instance PlatformOutputable CmmStatics where
    pprPlatform = pprStatics

instance PlatformOutputable CmmStatic where
    pprPlatform = pprStatic

instance PlatformOutputable CmmInfoTable where
    pprPlatform = pprInfoTable


-----------------------------------------------------------------------------

pprCmmGroup :: (PlatformOutputable d,
                PlatformOutputable info,
                PlatformOutputable g)
            => Platform -> GenCmmGroup d info g -> SDoc
pprCmmGroup platform tops
    = vcat $ intersperse blankLine $ map (pprTop platform) tops

-- --------------------------------------------------------------------------
-- Top level `procedure' blocks.
--
pprTop :: (PlatformOutputable d, PlatformOutputable info, PlatformOutputable i)
       => Platform -> GenCmmDecl d info i -> SDoc

pprTop platform (CmmProc info lbl graph)

  = vcat [ pprCLabel platform lbl <> lparen <> rparen
         , nest 8 $ lbrace <+> pprPlatform platform info $$ rbrace
         , nest 4 $ pprPlatform platform graph
         , rbrace ]

-- --------------------------------------------------------------------------
-- We follow [1], 4.5
--
--      section "data" { ... }
--
pprTop platform (CmmData section ds) =
    (hang (pprSection section <+> lbrace) 4 (pprPlatform platform ds))
    $$ rbrace

-- --------------------------------------------------------------------------
-- Info tables.

pprInfoTable :: Platform -> CmmInfoTable -> SDoc
pprInfoTable _ CmmNonInfoTable
  = empty
pprInfoTable platform
             (CmmInfoTable { cit_lbl = lbl, cit_rep = rep
                           , cit_prof = prof_info
                           , cit_srt = _srt })  
  = vcat [ ptext (sLit "label:") <+> pprPlatform platform lbl
         , ptext (sLit "rep:") <> ppr rep
         , case prof_info of
	     NoProfilingInfo -> empty
             ProfilingInfo ct cd -> vcat [ ptext (sLit "type:") <+> pprWord8String ct
                                         , ptext (sLit "desc: ") <> pprWord8String cd ] ]

instance PlatformOutputable C_SRT where
  pprPlatform _ (NoC_SRT) = ptext (sLit "_no_srt_")
  pprPlatform platform (C_SRT label off bitmap)
      = parens (pprPlatform platform label <> comma <> ppr off
                                           <> comma <> text (show bitmap))

instance Outputable ForeignHint where
  ppr NoHint     = empty
  ppr SignedHint = quotes(text "signed")
--  ppr AddrHint   = quotes(text "address")
-- Temp Jan08
  ppr AddrHint   = (text "PtrHint")
instance PlatformOutputable ForeignHint where
    pprPlatform _ = ppr

-- --------------------------------------------------------------------------
-- Static data.
--      Strings are printed as C strings, and we print them as I8[],
--      following C--
--
pprStatics :: Platform -> CmmStatics -> SDoc
pprStatics platform (Statics lbl ds) = vcat ((pprCLabel platform lbl <> colon) : map (pprPlatform platform) ds)

pprStatic :: Platform -> CmmStatic -> SDoc
pprStatic platform s = case s of
    CmmStaticLit lit   -> nest 4 $ ptext (sLit "const") <+> pprLit platform lit <> semi
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
