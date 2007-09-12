{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This module is pure representation and should be imported only by
-- clients that need to manipulate representation and know what
-- they're doing.  Clients that need to create flow graphs should
-- instead import MkZipCfgCmm.

module ZipCfgCmmRep
  ( CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, Middle(..), Last(..), Convention(..)
  , ValueDirection(..)
  )
where

#include "HsVersions.h"

import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmCallTarget(..), CmmActuals, CmmFormalsWithoutKinds, CmmFormals
           , CmmStmt(CmmJump, CmmSwitch) -- imported in order to call ppr
           )
import PprCmm()

import CLabel
import ClosureInfo
import FastString
import ForeignCall
import MachOp
import qualified ZipDataflow as DF
import ZipCfg 
import MkZipCfg

import Maybes
import Outputable
import Prelude hiding (zip, unzip, last)

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmZ      = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ   = GenCmmTop CmmStatic CmmInfo CmmGraph

data Middle
  = MidNop
  | MidComment FastString

  | MidAssign CmmReg CmmExpr     -- Assign to register

  | MidStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | MidUnsafeCall                -- An "unsafe" foreign call;
     CmmCallTarget               -- just a fat machine instructoin
     CmmFormals                  -- zero or more results
     CmmActuals                  -- zero or more arguments

  | CopyIn    -- Move parameters or results from conventional locations to registers
              -- Note [CopyIn invariant]
        Convention 
        CmmFormals      -- eventually [CmmKind] will be used only for foreign
                        -- calls and will migrate into 'Convention' (helping to
                        -- drain "the swamp")
        C_SRT           -- Static things kept alive by this block
  | CopyOut Convention CmmActuals

data Last
  = LastReturn CmmActuals          -- Return from a function,
                                  -- with these return values.

  | LastJump   CmmExpr CmmActuals
        -- Tail call to another procedure

  | LastBranch BlockId CmmFormalsWithoutKinds
        -- To another block in the same procedure
        -- The parameters are unused at present.

  | LastCall {                   -- A call (native or safe foreign)
        cml_target :: CmmExpr,   -- never a CmmPrim to a CallishMachOp!
        cml_next   :: Maybe BlockId }  -- BlockId of continuation, if call returns

  | LastCondBranch {            -- conditional branch
        cml_pred :: CmmExpr,
        cml_true, cml_false :: BlockId
    }

  | LastSwitch CmmExpr [Maybe BlockId]   -- Table branch
        -- The scrutinee is zero-based; 
        --      zero -> first block
        --      one  -> second block etc
        -- Undefined outside range, and when there's a Nothing

data Convention
  = ConventionStandard CCallConv ValueDirection
  | ConventionPrivate
                -- Used for control transfers within a (pre-CPS) procedure
                -- All jump sites known, never pushed on the stack (hence no SRT)
                -- You can choose whatever calling convention
                -- you please (provided you make sure
                -- all the call sites agree)!
  deriving Eq

data ValueDirection = Arguments | Results
  -- Arguments go with procedure definitions, jumps, and arguments to calls
  -- Results go with returns and with results of calls.
  deriving Eq

{-
Note [CopyIn invariant]
~~~~~~~~~~~~~~~~~~~~~~~
In principle, CopyIn ought to be a First node, but in practice, the
possibility raises all sorts of hairy issues with graph splicing,
rewriting, and so on.  In the end, NR finds it better to make the
placement of CopyIn a dynamic invariant.  This change will complicate
the dataflow fact for the proc-point calculation, but it should make
things easier in many other respects.  
-}

instance HavingSuccessors Last where
    succs = cmmSuccs
    fold_succs = fold_cmm_succs

instance LastNode Last where
    mkBranchNode id = LastBranch id []
    isBranchNode (LastBranch _ []) = True
    isBranchNode _ = False
    branchNodeTarget (LastBranch id []) = id
    branchNodeTarget _ = panic "asked for target of non-branch"

cmmSuccs :: Last -> [BlockId]
cmmSuccs (LastReturn {})        = []
cmmSuccs (LastJump {})          = [] 
cmmSuccs (LastBranch id _)      = [id]
cmmSuccs (LastCall _ (Just id)) = [id]
cmmSuccs (LastCall _ Nothing)   = []
cmmSuccs (LastCondBranch _ t f) = [f, t]  -- meets layout constraint
cmmSuccs (LastSwitch _ edges)   = catMaybes edges

fold_cmm_succs :: (BlockId -> a -> a) -> Last -> a -> a
fold_cmm_succs _f (LastReturn {})          z = z
fold_cmm_succs _f (LastJump {})            z = z
fold_cmm_succs  f (LastBranch id _)        z = f id z
fold_cmm_succs  f (LastCall _ (Just id))   z = f id z
fold_cmm_succs _f (LastCall _ Nothing)     z = z
fold_cmm_succs  f (LastCondBranch _ te fe) z = f te (f fe z)
fold_cmm_succs  f (LastSwitch _ edges)     z = foldl (flip f) z $ catMaybes edges


----------------------------------------------------------------
-- prettyprinting (avoids recursive imports)

instance Outputable Middle where
    ppr s = pprMiddle s

instance Outputable Last where
    ppr s = pprLast s

instance Outputable Convention where
    ppr = pprConvention

instance DF.DebugNodes Middle Last

instance Outputable CmmGraph where
    ppr = pprLgraph

debugPpr :: Bool
debugPpr = debugIsOn

pprMiddle :: Middle -> SDoc    
pprMiddle stmt = (case stmt of

    MidNop -> semi

    CopyIn conv args _ ->
        if null args then ptext SLIT("empty CopyIn")
        else commafy (map pprHinted args) <+> equals <+>
             ptext SLIT("foreign") <+> doubleQuotes(ppr conv) <+> ptext SLIT("...")

    CopyOut conv args ->
        if null args then empty
        else ptext SLIT("CopyOut") <+> doubleQuotes(ppr conv) <+>
             parens (commafy (map pprHinted args))

    --  // text
    MidComment s -> text "//" <+> ftext s

    -- reg = expr;
    MidAssign reg expr -> ppr reg <+> equals <+> ppr expr <> semi

    -- rep[lv] = expr;
    MidStore lv expr -> rep <> brackets(ppr lv) <+> equals <+> ppr expr <> semi
        where
          rep = ppr ( cmmExprRep expr )

    -- call "ccall" foo(x, y)[r1, r2];
    -- ToDo ppr volatile
    MidUnsafeCall (CmmCallee fn cconv) results args ->
        hcat [ if null results
                  then empty
                  else parens (commafy $ map ppr results) <>
                       ptext SLIT(" = "),
               ptext SLIT("call"), space, 
               doubleQuotes(ppr cconv), space,
               target fn, parens  ( commafy $ map ppr args ),
               semi ]
        where
            target t@(CmmLit _) = ppr t
            target fn'          = parens (ppr fn')

    MidUnsafeCall (CmmPrim op) results args ->
        pprMiddle (MidUnsafeCall (CmmCallee (CmmLit lbl) CCallConv) results args)
        where
          lbl = CmmLabel (mkForeignLabel (mkFastString (show op)) Nothing False)
  ) <>
  if debugPpr then empty
  else text " //" <+>
       case stmt of
         MidNop {}     -> text "MidNop"
         CopyIn {}     -> text "CopyIn"
         CopyOut {}    -> text "CopyOut"
         MidComment {} -> text "MidComment"
         MidAssign {}  -> text "MidAssign"
         MidStore {}   -> text "MidStore"
         MidUnsafeCall {} -> text "MidUnsafeCall"


pprHinted :: Outputable a => (a, MachHint) -> SDoc
pprHinted (a, NoHint)     = ppr a
pprHinted (a, PtrHint)    = doubleQuotes (text "address") <+> ppr a
pprHinted (a, SignedHint) = doubleQuotes (text "signed")  <+> ppr a
pprHinted (a, FloatHint)  = doubleQuotes (text "float")   <+> ppr a

pprLast :: Last -> SDoc    
pprLast stmt = (case stmt of
    LastBranch ident args     -> genBranchWithArgs ident args
    LastCondBranch expr t f   -> genFullCondBranch expr t f
    LastJump expr params      -> ppr $ CmmJump expr params
    LastReturn results        -> hcat [ ptext SLIT("return"), space
                                      , parens ( commafy $ map pprHinted results )
                                      , semi ]
    LastSwitch arg ids        -> ppr $ CmmSwitch arg ids
    LastCall tgt k            -> genBareCall tgt k
  ) <>
  if debugPpr then empty
  else text " //" <+>
       case stmt of
         LastBranch {} -> text "LastBranch"
         LastCondBranch {} -> text "LastCondBranch"
         LastJump {} -> text "LastJump"
         LastReturn {} -> text "LastReturn"
         LastSwitch {} -> text "LastSwitch"
         LastCall {} -> text "LastCall"

genBareCall :: CmmExpr -> Maybe BlockId -> SDoc
genBareCall fn k =
        hcat [ ptext SLIT("foreign"), space
             , doubleQuotes(ptext SLIT("<convention from CopyOut>")), space
             , target fn, parens  ( ptext SLIT("<parameters from CopyOut>") ), space
             , case k of Nothing -> ptext SLIT("never returns")
                         Just k -> ptext SLIT("returns to") <+> ppr k
             , semi ]
        where
            target t@(CmmLit _) = ppr t
            target fn'          = parens (ppr fn')

genBranchWithArgs :: (Outputable id, Outputable arg) => id -> [arg] -> SDoc
genBranchWithArgs ident [] = ptext SLIT("goto") <+> ppr ident <> semi
genBranchWithArgs ident args = ptext SLIT("goto") <+> ppr ident <+>
                               parens (commafy (map ppr args)) <> semi

genFullCondBranch :: Outputable id => CmmExpr -> id -> id -> SDoc
genFullCondBranch expr t f =
    hsep [ ptext SLIT("if")
         , parens(ppr expr)
         , ptext SLIT("goto")
         , ppr t <> semi
         , ptext SLIT("else goto")
         , ppr f <> semi
         ]

pprConvention :: Convention -> SDoc
pprConvention (ConventionStandard c _) = ppr c
pprConvention (ConventionPrivate {}  ) = text "<private-convention>"

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs
