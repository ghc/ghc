

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
           , CmmCallTarget(..), CmmActuals, CmmFormals
           , CmmStmt(CmmSwitch) -- imported in order to call ppr
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

----------------------------------------------------------------------
----- Type synonyms and definitions

type CmmGraph  = LGraph Middle Last
type CmmAGraph = AGraph Middle Last
type CmmBlock  = Block  Middle Last
type CmmZ      = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ   = GenCmmTop CmmStatic CmmInfo CmmGraph

data Middle
  = MidComment FastString

  | MidAssign CmmReg CmmExpr     -- Assign to register

  | MidStore CmmExpr CmmExpr     -- Assign to memory location.  Size is
                                 -- given by cmmExprRep of the rhs.

  | MidUnsafeCall                -- An "unsafe" foreign call;
     CmmCallTarget               -- just a fat machine instructoin
     CmmFormals                  -- zero or more results
     CmmActuals                  -- zero or more arguments

  | CopyIn    -- Move incoming parameters or results from conventional
              -- locations to registers.  Note [CopyIn invariant]
        Convention 
        CmmFormals      -- eventually [CmmKind] will be used only for foreign
                        -- calls and will migrate into 'Convention' (helping to
                        -- drain "the swamp"), leaving this as [LocalReg]
        C_SRT           -- Static things kept alive by this block

  | CopyOut Convention CmmActuals
              -- Move outgoing parameters or results from registers to
              -- conventional locations.  Every 'LastReturn',
              -- 'LastJump', or 'LastCall' must be dominated by a
              -- matching 'CopyOut' in the same basic block.
              -- As above, '[CmmKind]' will migrate into the foreign calling
              -- convention, leaving the actuals as '[CmmExpr]'.

data Last
  = LastBranch BlockId  -- Goto another block in the same procedure

  | LastCondBranch {            -- conditional branch
        cml_pred :: CmmExpr,
        cml_true, cml_false :: BlockId
    }

  | LastReturn          -- Return from a function; values in a previous CopyOut node

  | LastJump CmmExpr    -- Tail call to another procedure; args in a CopyOut node

  | LastCall {                   -- A call (native or safe foreign); args in CopyOut node
        cml_target :: CmmExpr,   -- never a CmmPrim to a CallishMachOp!
        cml_cont   :: Maybe BlockId }  -- BlockId of continuation, if call returns

  | LastSwitch CmmExpr [Maybe BlockId]   -- Table branch
        -- The scrutinee is zero-based; 
        --      zero -> first block
        --      one  -> second block etc
        -- Undefined outside range, and when there's a Nothing

data Convention
  = ConventionStandard CCallConv ValueDirection
  | ConventionPrivate
                -- Used for control transfers within a (pre-CPS) procedure All
                -- jump sites known, never pushed on the stack (hence no SRT)
                -- You can choose whatever calling convention you please
                -- (provided you make sure all the call sites agree)!
                -- This data type eventually to be extended to record the convention. 

  deriving Eq

data ValueDirection = Arguments | Results
  -- Arguments go with procedure definitions, jumps, and arguments to calls
  -- Results go with returns and with results of calls.
  deriving Eq

{-
Note [CopyIn invariant]
~~~~~~~~~~~~~~~~~~~~~~~
One might wish for CopyIn to be a First node, but in practice, the
possibility raises all sorts of hairy issues with graph splicing,
rewriting, and so on.  In the end, NR finds it better to make the
placement of CopyIn a dynamic invariant; it should normally be the first
Middle node in the basic block in which it occurs.
-}

----------------------------------------------------------------------
----- Instance declarations for control flow

instance HavingSuccessors Last where
    succs = cmmSuccs
    fold_succs = fold_cmm_succs

instance LastNode Last where
    mkBranchNode id = LastBranch id
    isBranchNode (LastBranch _) = True
    isBranchNode _ = False
    branchNodeTarget (LastBranch id) = id
    branchNodeTarget _ = panic "asked for target of non-branch"

cmmSuccs :: Last -> [BlockId]
cmmSuccs (LastReturn {})        = []
cmmSuccs (LastJump {})          = [] 
cmmSuccs (LastBranch id)        = [id]
cmmSuccs (LastCall _ (Just id)) = [id]
cmmSuccs (LastCall _ Nothing)   = []
cmmSuccs (LastCondBranch _ t f) = [f, t]  -- meets layout constraint
cmmSuccs (LastSwitch _ edges)   = catMaybes edges

fold_cmm_succs :: (BlockId -> a -> a) -> Last -> a -> a
fold_cmm_succs _f (LastReturn {})          z = z
fold_cmm_succs _f (LastJump {})            z = z
fold_cmm_succs  f (LastBranch id)          z = f id z
fold_cmm_succs  f (LastCall _ (Just id))   z = f id z
fold_cmm_succs _f (LastCall _ Nothing)     z = z
fold_cmm_succs  f (LastCondBranch _ te fe) z = f te (f fe z)
fold_cmm_succs  f (LastSwitch _ edges)     z = foldl (flip f) z $ catMaybes edges

----------------------------------------------------------------------
----- Instance declarations for register use

instance UserOfLocalRegs Middle where
    foldRegsUsed f z m = middle m
      where middle (MidComment {})                = z
            middle (MidAssign _lhs expr)          = foldRegsUsed f z expr
            middle (MidStore addr rval)           = foldRegsUsed f (foldRegsUsed f z addr) rval
            middle (MidUnsafeCall tgt _ress args) = foldRegsUsed f (foldRegsUsed f z tgt) args
            middle (CopyIn _ _formals _)          = z
            middle (CopyOut _ actuals)            = foldRegsUsed f z actuals
--            fold = foldRegsUsed

instance UserOfLocalRegs Last where
    foldRegsUsed f z m = last m
      where last (LastReturn)           = z
            last (LastJump e)           = foldRegsUsed f z e
            last (LastBranch _id)       = z
            last (LastCall tgt _)       = foldRegsUsed f z tgt
            last (LastCondBranch e _ _) = foldRegsUsed f z e
            last (LastSwitch e _tbl)    = foldRegsUsed f z e

instance UserOfLocalRegs (ZLast Last) where
    foldRegsUsed  f z (LastOther l) = foldRegsUsed f z l
    foldRegsUsed _f z LastExit      = z


----------------------------------------------------------------------
----- Instance declarations for prettyprinting (avoids recursive imports)

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

    CopyIn conv args _ ->
        if null args then ptext SLIT("empty CopyIn")
        else commafy (map pprHinted args) <+> equals <+>
             ptext SLIT("foreign") <+> doubleQuotes(ppr conv) <+> ptext SLIT("...")

    CopyOut conv args ->
        ptext SLIT("next, pass") <+> doubleQuotes(ppr conv) <+>
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
    LastBranch ident          -> ptext SLIT("goto") <+> ppr ident <> semi
    LastCondBranch expr t f   -> genFullCondBranch expr t f
    LastJump expr             -> hcat [ ptext SLIT("jump"), space, pprFun expr
                                      , ptext SLIT("(...)"), semi]
    LastReturn                -> hcat [ ptext SLIT("return"), space 
                                      , ptext SLIT("(...)"), semi]
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
        hcat [ ptext SLIT("call"), space
             , pprFun fn, ptext SLIT("(...)"), space
             , case k of Nothing -> ptext SLIT("never returns")
                         Just k -> ptext SLIT("returns to") <+> ppr k
             , semi ]
        where

pprFun :: CmmExpr -> SDoc
pprFun f@(CmmLit _) = ppr f
pprFun f = parens (ppr f)

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
