-- Cmm representations using Hoopl's Graph CmmNode e x.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module GHC.Cmm (
     -- * Cmm top-level datatypes
     DCmmGroup,
     CmmProgram, CmmGroup, CmmGroupSRTs, RawCmmGroup, GenCmmGroup,
     CmmDecl, DCmmDecl, CmmDeclSRTs, GenCmmDecl(..),
     CmmDataDecl, cmmDataDeclCmmDecl, DCmmGraph,
     CmmGraph, GenCmmGraph, GenGenCmmGraph(..),
     toBlockMap, revPostorder, toBlockList,
     CmmBlock, RawCmmDecl,
     Section(..), SectionType(..),
     GenCmmStatics(..), type CmmStatics, type RawCmmStatics, CmmStatic(..),
     SectionProtection(..), sectionProtection,

     DWrap(..), unDeterm, removeDeterm, removeDetermDecl, removeDetermGraph,

     -- ** Blocks containing lists
     GenBasicBlock(..), blockId,
     ListGraph(..), pprBBlock,

     -- * Info Tables
     GenCmmTopInfo(..)
     , DCmmTopInfo
     , CmmTopInfo
     , CmmStackInfo(..), CmmInfoTable(..), topInfoTable, topInfoTableD,
     ClosureTypeInfo(..),
     ProfilingInfo(..), renderProfInfo, ConstrDescription,

     -- * Statements, expressions and types
     module GHC.Cmm.Node,
     module GHC.Cmm.Expr,

     -- * Pretty-printing
     pprCmmGroup, pprSection, pprStatic
  ) where

import GHC.Prelude
import GHC.Utils.Panic (pprPanic)

import GHC.Platform
import GHC.Core.TyCo.Rep
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Unit.Types (Module)
import GHC.Types.Name (Name, pprFullName, getOccString)
import GHC.Types.Id
import GHC.Types.CostCentre
import GHC.Cmm.CLabel
import GHC.Cmm.BlockId
import GHC.Cmm.Node
import GHC.Runtime.Heap.Layout
import GHC.Cmm.Expr
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Utils.Outputable

import Data.Void (Void)
import Data.List (intersperse)
import Data.ByteString (ByteString)

-----------------------------------------------------------------------------
--  Cmm, GenCmm
-----------------------------------------------------------------------------

-- A CmmProgram is a list of CmmGroups
-- A CmmGroup is a list of top-level declarations

-- When object-splitting is on, each group is compiled into a separate
-- .o file. So typically we put closely related stuff in a CmmGroup.
-- Section-splitting follows suit and makes one .text subsection for each
-- CmmGroup.

type CmmProgram = [CmmGroup]

type GenCmmGroup d h g = [GenCmmDecl d h g]
-- | Cmm group after STG generation
type DCmmGroup    = GenCmmGroup CmmStatics    DCmmTopInfo              DCmmGraph
-- | Cmm group before SRT generation
type CmmGroup     = GenCmmGroup CmmStatics    CmmTopInfo               CmmGraph
-- | Cmm group with SRTs
type CmmGroupSRTs = GenCmmGroup RawCmmStatics CmmTopInfo               CmmGraph
-- | "Raw" cmm group (TODO (osa): not sure what that means)
type RawCmmGroup  = GenCmmGroup RawCmmStatics (LabelMap RawCmmStatics) CmmGraph

-----------------------------------------------------------------------------
--  CmmDecl, GenCmmDecl
-----------------------------------------------------------------------------

-- GenCmmDecl is abstracted over
--   d, the type of static data elements in CmmData
--   h, the static info preceding the code of a CmmProc
--   g, the control-flow graph of a CmmProc
--
-- We expect there to be two main instances of this type:
--   (a) C--, i.e. populated with various C-- constructs
--   (b) Native code, populated with data/instructions

-- | A top-level chunk, abstracted over the type of the contents of
-- the basic blocks (Cmm or instructions are the likely instantiations).
data GenCmmDecl d h g
  = CmmProc     -- A procedure
     h                 -- Extra header such as the info table
     CLabel            -- Entry label
     [GlobalReg]       -- Registers live on entry. Note that the set of live
                       -- registers will be correct in generated C-- code, but
                       -- not in hand-written C-- code. However,
                       -- splitAtProcPoints calculates correct liveness
                       -- information for CmmProcs.
     g                 -- Control-flow graph for the procedure's code

  | CmmData     -- Static data
        Section
        d

  deriving (Functor)

instance (OutputableP Platform d, OutputableP Platform info, OutputableP Platform i)
      => OutputableP Platform (GenCmmDecl d info i) where
    pdoc = pprTop

type DCmmDecl    = GenCmmDecl CmmStatics DCmmTopInfo DCmmGraph
type CmmDecl     = GenCmmDecl CmmStatics    CmmTopInfo CmmGraph
type CmmDeclSRTs = GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph
type CmmDataDecl = GenCmmDataDecl CmmStatics
type GenCmmDataDecl d = GenCmmDecl d Void Void -- When `CmmProc` case can be statically excluded

cmmDataDeclCmmDecl :: GenCmmDataDecl d -> GenCmmDecl d h g
cmmDataDeclCmmDecl = \ case
    CmmProc void _ _ _ -> case void of
    CmmData section d -> CmmData section d
{-# INLINE cmmDataDeclCmmDecl #-}

type RawCmmDecl
   = GenCmmDecl
        RawCmmStatics
        (LabelMap RawCmmStatics)
        CmmGraph

-----------------------------------------------------------------------------
--     Graphs
-----------------------------------------------------------------------------

type CmmGraph = GenCmmGraph CmmNode
type DCmmGraph = GenGenCmmGraph DWrap CmmNode

type GenCmmGraph n = GenGenCmmGraph LabelMap n

data GenGenCmmGraph s n = CmmGraph { g_entry :: BlockId, g_graph :: Graph' s Block n C C }
type CmmBlock = Block CmmNode C C

instance OutputableP Platform CmmGraph where
    pdoc = pprCmmGraph

toBlockMap :: CmmGraph -> LabelMap CmmBlock
toBlockMap (CmmGraph {g_graph=GMany NothingO body NothingO}) = body

pprCmmGraph :: Platform -> CmmGraph -> SDoc
pprCmmGraph platform g
   = text "{" <> text "offset"
  $$ nest 2 (vcat $ map (pdoc platform) blocks)
  $$ text "}"
  where blocks = revPostorder g
    -- revPostorder has the side-effect of discarding unreachable code,
    -- so pretty-printed Cmm will omit any unreachable blocks.  This can
    -- sometimes be confusing.

revPostorder :: CmmGraph -> [CmmBlock]
revPostorder g = {-# SCC "revPostorder" #-}
    revPostorderFrom (toBlockMap g) (g_entry g)

toBlockList :: CmmGraph -> [CmmBlock]
toBlockList g = mapElems $ toBlockMap g

-----------------------------------------------------------------------------
--     Info Tables
-----------------------------------------------------------------------------

-- | CmmTopInfo is attached to each CmmDecl (see defn of CmmGroup), and contains
-- the extra info (beyond the executable code) that belongs to that CmmDecl.
data GenCmmTopInfo f = TopInfo { info_tbls  :: f CmmInfoTable
                               , stack_info :: CmmStackInfo }

newtype DWrap a = DWrap [(BlockId, a)]

unDeterm :: DWrap a -> [(BlockId, a)]
unDeterm (DWrap f) = f

type DCmmTopInfo = GenCmmTopInfo DWrap
type CmmTopInfo  = GenCmmTopInfo LabelMap

instance OutputableP Platform CmmTopInfo where
    pdoc = pprTopInfo

pprTopInfo :: Platform -> CmmTopInfo -> SDoc
pprTopInfo platform (TopInfo {info_tbls=info_tbl, stack_info=stack_info}) =
  vcat [text "info_tbls: " <> pdoc platform info_tbl,
        text "stack_info: " <> ppr stack_info]

topInfoTableD :: GenCmmDecl a DCmmTopInfo (GenGenCmmGraph s n) -> Maybe CmmInfoTable
topInfoTableD (CmmProc infos _ _ g) = case (info_tbls infos) of
                                          DWrap xs -> lookup (g_entry g) xs
topInfoTableD _                     = Nothing

topInfoTable :: GenCmmDecl a CmmTopInfo (GenGenCmmGraph s n) -> Maybe CmmInfoTable
topInfoTable (CmmProc infos _ _ g) = mapLookup (g_entry g) (info_tbls infos)
topInfoTable _                     = Nothing

data CmmStackInfo
   = StackInfo {
       arg_space :: ByteOff,
               -- number of bytes of arguments on the stack on entry to the
               -- the proc.  This is filled in by GHC.StgToCmm.codeGen, and
               -- used by the stack allocator later.
       do_layout :: Bool
               -- Do automatic stack layout for this proc.  This is
               -- True for all code generated by the code generator,
               -- but is occasionally False for hand-written Cmm where
               -- we want to do the stack manipulation manually.
  }

instance Outputable CmmStackInfo where
    ppr = pprStackInfo

pprStackInfo :: CmmStackInfo -> SDoc
pprStackInfo (StackInfo {arg_space=arg_space}) =
  text "arg_space: " <> ppr arg_space

-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: Maybe CLabel,   -- empty, or a closure address
      cit_clo  :: Maybe (Id, CostCentreStack)
        -- Just (id,ccs) <=> build a static closure later
        -- Nothing <=> don't build a static closure
        --
        -- Static closures for FUNs and THUNKs are *not* generated by
        -- the code generator, because we might want to add SRT
        -- entries to them later (for FUNs at least; THUNKs are
        -- treated the same for consistency). See Note [SRTs] in
        -- GHC.Cmm.Info.Build, in particular the [FUN] optimisation.
        --
        -- This is strictly speaking not a part of the info table that
        -- will be finally generated, but it's the only convenient
        -- place to convey this information from the code generator to
        -- where we build the static closures in
        -- GHC.Cmm.Info.Build.doSRTs.
    } deriving (Eq, Ord)

instance OutputableP Platform CmmInfoTable where
    pdoc = pprInfoTable

-----------------------------------------------------------------------------
--              Profiling
-----------------------------------------------------------------------------

data ProfilingInfo
  = NoProfilingInfo
  | ClosureProfilingInfo Module Id -- this_module, closure_id
  | DataProfilingInfo Name Name -- datacon_tycon_name, datacon_name
  | ParsedProfilingInfo String String
  -- ^ Construct profiling info directly with the description and type strings (as in Cmm.Parser)
  -- Note that if these strings leak non-deterministic uniques those will
  -- ultimately leak into the object files regardless of the renaming pass.
  -- In the parser they are stable since they're parsed directly as strings.
  deriving (Eq, Ord)

-- | Render the profiling information as strings from a 'ProfilingInfo':
-- the first string is the type and the second is the description.
renderProfInfo :: ProfilingInfo -> Maybe (String {- type -}, String {- description -})
renderProfInfo NoProfilingInfo = Nothing
renderProfInfo (ClosureProfilingInfo mod_name id) = Just (ty_descr, val_descr)
  where
    ty_descr  = getTyDescription (idType id)
    val_descr = closureDescription mod_name (idName id)
renderProfInfo (DataProfilingInfo datacon_tycon_name datacon_name) = Just (ty_descr, val_descr)
  where
   ty_descr  = getOccString datacon_tycon_name
   val_descr = getOccString datacon_name
renderProfInfo (ParsedProfilingInfo ty_descr val_descr) = Just (ty_descr, val_descr)

-- For "global" data constructors the description is simply occurrence
-- name of the data constructor itself.  Otherwise it is determined by
-- @closureDescription@ from the let binding information.

closureDescription
   :: Module            -- Module
   -> Name              -- Id of closure binding
   -> String
        -- Not called for StgRhsCon which have global info tables built in
        -- CgConTbls.hs with a description generated from the data constructor
closureDescription mod_name name
  = showSDocOneLine defaultSDocContext
    (char '<' <> pprFullName mod_name name <> char '>')

getTyDescription :: Type -> String
getTyDescription ty
  = case (tcSplitSigmaTy ty) of { (_, _, tau_ty) ->
    case tau_ty of
      TyVarTy _              -> "*"
      AppTy fun _            -> getTyDescription fun
      TyConApp tycon _       -> getOccString tycon
      FunTy {}              -> '-' : fun_result tau_ty
      ForAllTy _  ty         -> getTyDescription ty
      LitTy n                -> getTyLitDescription n
      CastTy ty _            -> getTyDescription ty
      CoercionTy co          -> pprPanic "getTyDescription" (ppr co)
    }
  where
    fun_result (FunTy { ft_res = res }) = '>' : fun_result res
    fun_result other                    = getTyDescription other

getTyLitDescription :: TyLit -> String
getTyLitDescription l =
  case l of
    NumTyLit n -> show n
    StrTyLit n -> show n
    CharTyLit n -> show n

-----------------------------------------------------------------------------
--              Static Data
-----------------------------------------------------------------------------

data SectionType
  = Text
  | Data
  | ReadOnlyData
  | RelocatableReadOnlyData
  | UninitialisedData
    -- See Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini
  | InitArray           -- .init_array on ELF, .ctor on Windows
  | FiniArray           -- .fini_array on ELF, .dtor on Windows
  | CString
  | OtherSection String
  deriving (Show)

data SectionProtection
  = ReadWriteSection
  | ReadOnlySection
  | WriteProtectedSection -- See Note [Relocatable Read-Only Data]
  deriving (Eq)

-- | Should a data in this section be considered constant at runtime
sectionProtection :: Section -> SectionProtection
sectionProtection (Section t _) = case t of
    Text                    -> ReadOnlySection
    ReadOnlyData            -> ReadOnlySection
    RelocatableReadOnlyData -> WriteProtectedSection
    InitArray               -> ReadOnlySection
    FiniArray               -> ReadOnlySection
    CString                 -> ReadOnlySection
    Data                    -> ReadWriteSection
    UninitialisedData       -> ReadWriteSection
    (OtherSection _)        -> ReadWriteSection

{-
Note [Relocatable Read-Only Data]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Relocatable data are only read-only after relocation at the start of the
program. They should be writable from the source code until then. Failure to
do so would end up in segfaults at execution when using linkers that do not
enforce writability of those sections, such as the gold linker.
-}

data Section = Section SectionType CLabel

data CmmStatic
  = CmmStaticLit CmmLit
        -- ^ a literal value, size given by cmmLitRep of the literal.
  | CmmUninitialised Int
        -- ^ uninitialised data, N bytes long
  | CmmString ByteString
        -- ^ string of 8-bit values only, not zero terminated.
  | CmmFileEmbed FilePath Int
        -- ^ an embedded binary file and its byte length

instance OutputableP Platform CmmStatic where
    pdoc = pprStatic

instance Outputable CmmStatic where
  ppr (CmmStaticLit lit) = text "CmmStaticLit" <+> ppr lit
  ppr (CmmUninitialised n) = text "CmmUninitialised" <+> ppr n
  ppr (CmmString _) = text "CmmString"
  ppr (CmmFileEmbed fp _) = text "CmmFileEmbed" <+> text fp

-- | Static data before or after SRT generation
data GenCmmStatics (rawOnly :: Bool) where
    CmmStatics
      :: CLabel       -- Label of statics
      -> CmmInfoTable
      -> CostCentreStack
      -> [CmmLit]     -- Payload
      -> [CmmLit]     -- Non-pointers that go to the end of the closure
                      -- This is used by stg_unpack_cstring closures.
                      -- See Note [unpack_cstring closures] in StgStdThunks.cmm.
      -> GenCmmStatics 'False

    -- | Static data, after SRTs are generated
    CmmStaticsRaw
      :: CLabel       -- Label of statics
      -> [CmmStatic]  -- The static data itself
      -> GenCmmStatics a

instance OutputableP Platform (GenCmmStatics a) where
    pdoc = pprStatics

type CmmStatics    = GenCmmStatics 'False
type RawCmmStatics = GenCmmStatics 'True

-- Converting out of deterministic Cmm

removeDeterm :: DCmmGroup -> CmmGroup
removeDeterm = map removeDetermDecl

removeDetermDecl :: DCmmDecl -> CmmDecl
removeDetermDecl (CmmProc h e r g) = CmmProc (removeDetermTop h) e r (removeDetermGraph g)
removeDetermDecl (CmmData a b) = CmmData a b

removeDetermTop :: DCmmTopInfo -> CmmTopInfo
removeDetermTop (TopInfo a b) = TopInfo (mapFromList $ unDeterm a) b

removeDetermGraph :: DCmmGraph -> CmmGraph
removeDetermGraph (CmmGraph x y) =
  let y' = case y of
            GMany a (DWrap b) c -> GMany a (mapFromList b) c
  in CmmGraph x y'


-- -----------------------------------------------------------------------------
-- Basic blocks consisting of lists

-- These are used by the LLVM and NCG backends, when populating Cmm
-- with lists of instructions.

data GenBasicBlock i
   = BasicBlock BlockId [i]
   deriving (Functor)


-- | The branch block id is that of the first block in
-- the branch, which is that branch's entry point
blockId :: GenBasicBlock i -> BlockId
blockId (BasicBlock blk_id _ ) = blk_id

newtype ListGraph i
   = ListGraph [GenBasicBlock i]
   deriving (Functor)

instance Outputable instr => Outputable (ListGraph instr) where
    ppr (ListGraph blocks) = vcat (map ppr blocks)

instance OutputableP env instr => OutputableP env (ListGraph instr) where
    pdoc env g = ppr (fmap (pdoc env) g)


instance Outputable instr => Outputable (GenBasicBlock instr) where
    ppr = pprBBlock

instance OutputableP env instr => OutputableP env (GenBasicBlock instr) where
    pdoc env block = ppr (fmap (pdoc env) block)

pprBBlock :: Outputable stmt => GenBasicBlock stmt -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map ppr stmts))


-- --------------------------------------------------------------------------
-- Pretty-printing Cmm
-- --------------------------------------------------------------------------
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
-- Pretty-printing info tables
-- --------------------------------------------------------------------------

pprInfoTable :: Platform -> CmmInfoTable -> SDoc
pprInfoTable platform (CmmInfoTable { cit_lbl = lbl, cit_rep = rep
                           , cit_prof = prof_info
                           , cit_srt = srt })
  = vcat [ text "label: " <> pdoc platform lbl
         , text "rep: " <> ppr rep
         , case renderProfInfo prof_info of
             Nothing -> empty
             Just (ct, cd) ->
               vcat [ text "type: " <> text (show ct)
                    , text "desc: " <> text (show cd) ]
         , text "srt: " <> pdoc platform srt ]

-- --------------------------------------------------------------------------
-- Static data.
--      Strings are printed as C strings, and we print them as I8[],
--      following C--
--

pprStatics :: Platform -> GenCmmStatics a -> SDoc
pprStatics platform (CmmStatics lbl itbl ccs payload extras) =
  pdoc platform lbl <> colon <+> pdoc platform itbl <+> ppr ccs <+> pdoc platform payload <+> ppr extras
pprStatics platform (CmmStaticsRaw lbl ds) = vcat ((pdoc platform lbl <> colon) : map (pprStatic platform) ds)

pprStatic :: Platform -> CmmStatic -> SDoc
pprStatic platform s = case s of
    CmmStaticLit lit   -> nest 4 $ text "const" <+> pdoc platform lit <> semi
    CmmUninitialised i -> nest 4 $ text "I8" <> brackets (int i)
    CmmString s'       -> nest 4 $ text "I8[]" <+> text (show s')
    CmmFileEmbed path _ -> nest 4 $ text "incbin " <+> text (show path)

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
  RelocatableReadOnlyData -> text "relreadonly"
  UninitialisedData       -> text "uninitialised"
  InitArray               -> text "initarray"
  FiniArray               -> text "finiarray"
  CString                 -> text "cstring"
  OtherSection s'         -> text s'
