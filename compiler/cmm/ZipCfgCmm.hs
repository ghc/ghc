{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module ZipCfgCmm
  ( mkNop, mkAssign, mkStore, mkCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkCbranch, mkSwitch, mkReturn, mkComment, mkCmmIfThenElse
         , mkCmmWhileDo
  , mkCopyIn, mkCopyOut
  , CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, Middle(..), Last(..), Convention(..)
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
import Outputable hiding (empty)
import qualified Outputable as PP
import Prelude hiding (zip, unzip, last)

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmZ      = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ   = GenCmmTop CmmStatic CmmInfo CmmGraph

mkNop        :: CmmAGraph
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph
mkCall       :: CmmCallTarget -> CmmFormals -> CmmActuals -> C_SRT -> CmmAGraph
mkUnsafeCall :: CmmCallTarget -> CmmFormals -> CmmActuals -> CmmAGraph
mkFinalCall  :: CmmCallTarget -> CmmActuals -> CmmAGraph -- never returns
mkJump       :: CmmExpr -> CmmActuals -> CmmAGraph
mkCbranch    :: CmmExpr -> BlockId -> BlockId -> CmmAGraph
mkSwitch     :: CmmExpr -> [Maybe BlockId] -> CmmAGraph
mkReturn     :: CmmActuals -> CmmAGraph
mkComment    :: FastString -> CmmAGraph

-- Not to be forgotten, but exported by MkZipCfg:
--mkBranch      :: BlockId -> CmmAGraph
--mkLabel       :: BlockId -> CmmAGraph
mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> CmmAGraph
mkCmmWhileDo    :: CmmExpr -> CmmAGraph -> CmmAGraph 

--------------------------------------------------------------------------

mkCmmIfThenElse e = mkIfThenElse (mkCbranch e)
mkCmmWhileDo    e = mkWhileDo    (mkCbranch e)

mkCopyIn     :: Convention -> CmmFormals -> C_SRT -> CmmAGraph
mkCopyOut    :: Convention -> CmmFormals -> CmmAGraph

  -- ^ XXX: Simon or Simon thinks maybe the hints are being abused and
  -- we should have CmmFormalsWithoutKinds here, but for now it is CmmFormals
  -- for consistency with the rest of the back end ---NR

mkComment fs = mkMiddle (MidComment fs)

data Middle
  = MidNop
  | MidComment FastString

  | MidAssign CmmReg CmmExpr     -- Assign to register

  | MidStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | MidUnsafeCall                -- An "unsafe" foreign call;
     CmmCallTarget               -- just a fat machine instructoin
     CmmFormals              -- zero or more results
     CmmActuals                  -- zero or more arguments

  | CopyIn    -- Move parameters or results from conventional locations to registers
              -- Note [CopyIn invariant]
        Convention 
        CmmFormals      
        C_SRT           -- Static things kept alive by this block
  | CopyOut Convention CmmFormals 

data Last
  = LastReturn CmmActuals          -- Return from a function,
                                  -- with these return values.

  | LastJump   CmmExpr CmmActuals
        -- Tail call to another procedure

  | LastBranch BlockId CmmFormalsWithoutKinds
        -- To another block in the same procedure
        -- The parameters are unused at present.

  | LastCall {                   -- A call (native or safe foreign)
        cml_target :: CmmCallTarget,
        cml_actual :: CmmActuals,        -- Zero or more arguments
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
  = Argument CCallConv  -- Used for function formal params
  | Result CCallConv    -- Used for function results

  | Local       -- Used for control transfers within a (pre-CPS) procedure
                -- All jump sites known, never pushed on the stack (hence no SRT)
                -- You can choose whatever calling convention
                -- you please (provided you make sure
                -- all the call sites agree)!
  deriving Eq

-- ^ In a complete LGraph for a procedure, the [[Exit]] node should not
-- appear, but it is useful in a subgraph (e.g., replacement for a node).

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


-- ================ IMPLEMENTATION ================--

mkNop                     = mkMiddle $ MidNop
mkAssign l r              = mkMiddle $ MidAssign l r
mkStore  l r              = mkMiddle $ MidStore  l r
mkCopyIn  conv args srt   = mkMiddle $ CopyIn  conv args srt
mkCopyOut conv args       = mkMiddle $ CopyOut conv args 

mkJump e args             = mkLast   $ LastJump e args
mkCbranch pred ifso ifnot = mkLast   $ LastCondBranch pred ifso ifnot
mkReturn actuals          = mkLast   $ LastReturn actuals
mkSwitch e tbl            = mkLast   $ LastSwitch e tbl

mkUnsafeCall tgt results actuals = mkMiddle $ MidUnsafeCall tgt results actuals
mkFinalCall  tgt actuals         = mkLast   $ LastCall      tgt actuals Nothing

mkCall tgt results actuals srt =
  withFreshLabel "call successor" $ \k ->
    mkLast (LastCall tgt actuals (Just k)) <*>
    mkLabel k <*>
    mkCopyIn (Result CmmCallConv) results srt

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
cmmSuccs (LastReturn {})          = []
cmmSuccs (LastJump {})            = [] 
cmmSuccs (LastBranch id _)        = [id]
cmmSuccs (LastCall _ _ (Just id)) = [id]
cmmSuccs (LastCall _ _ Nothing)   = []
cmmSuccs (LastCondBranch _ t f)   = [f, t]  -- meets layout constraint
cmmSuccs (LastSwitch _ edges)     = catMaybes edges

fold_cmm_succs :: (BlockId -> a -> a) -> Last -> a -> a
fold_cmm_succs _f (LastReturn {})          z = z
fold_cmm_succs _f (LastJump {})            z = z
fold_cmm_succs  f (LastBranch id _)        z = f id z
fold_cmm_succs  f (LastCall _ _ (Just id)) z = f id z
fold_cmm_succs _f (LastCall _ _ Nothing)   z = z
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
    ppr = pprCmmGraphAsRep

pprCmmGraphAsRep :: CmmGraph -> SDoc
pprCmmGraphAsRep g = vcat (map ppr_block blocks)
    where blocks = postorder_dfs g
          ppr_block (Block id tail) = hang (ppr id <> colon) 4 (ppr tail)

pprMiddle :: Middle -> SDoc    
pprMiddle stmt = (case stmt of

    MidNop -> semi

    CopyIn conv args _ ->
        if null args then ptext SLIT("empty CopyIn")
        else commafy (map pprHinted args) <+> equals <+>
             ptext SLIT("foreign") <+> doubleQuotes(ppr conv) <+> ptext SLIT("...")

    CopyOut conv args ->
        if null args then PP.empty
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
                  then PP.empty
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
  ) <+> text "//" <+>
  case stmt of
    MidNop {} -> text "MidNop"
    CopyIn {} -> text "CopyIn"
    CopyOut {} -> text "CopyOut"
    MidComment {} -> text "MidComment"
    MidAssign {} -> text "MidAssign"
    MidStore {} -> text "MidStore"
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
    LastCall tgt params k     -> genCall tgt params k
  ) <+> text "//" <+>
  case stmt of
    LastBranch {} -> text "LastBranch"
    LastCondBranch {} -> text "LastCondBranch"
    LastJump {} -> text "LastJump"
    LastReturn {} -> text "LastReturn"
    LastSwitch {} -> text "LastSwitch"
    LastCall {} -> text "LastCall"


genCall :: CmmCallTarget -> CmmActuals -> Maybe BlockId -> SDoc
genCall (CmmCallee fn cconv) args k =
        hcat [ ptext SLIT("foreign"), space
             , doubleQuotes(ppr cconv), space
             , target fn, parens  ( commafy $ map pprHinted args ), space
             , case k of Nothing -> ptext SLIT("never returns")
                         Just k -> ptext SLIT("returns to") <+> ppr k
             , semi ]
        where
            target t@(CmmLit _) = ppr t
            target fn'          = parens (ppr fn')

genCall (CmmPrim op) args k =
    hcat [ text "%", text (show op), parens  ( commafy $ map pprHinted args ),
           ptext SLIT("returns to"), space, ppr k,
           semi ]

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
pprConvention (Argument c) = ppr c
pprConvention (Result c) = ppr c
pprConvention Local = text "<local>"

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs
