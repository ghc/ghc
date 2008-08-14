 

-- This module is pure representation and should be imported only by
-- clients that need to manipulate representation and know what
-- they're doing.  Clients that need to create flow graphs should
-- instead import MkZipCfgCmm.

module ZipCfgCmmRep
  ( CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph
  , Middle(..), Last(..), MidCallTarget(..)
  , Convention(..), ForeignConvention(..)
  , ValueDirection(..), ForeignHint(..)
  , CmmBackwardFixedPoint, CmmForwardFixedPoint, pprHinted
  , insertBetween, mapExpMiddle, mapExpLast, mapExpDeepMiddle, mapExpDeepLast
  , foldExpMiddle, foldExpLast, foldExpDeepMiddle, foldExpDeepLast
  , joinOuts
  )
where

import BlockId
import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CallishMachOp(..), ForeignHint(..)
           , CmmActuals, CmmFormals, CmmHinted(..)
           , CmmStmt(..) -- imported in order to call ppr on Switch and to
                         -- implement pprCmmGraphLikeCmm
           )
import DFMonad
import PprCmm()
import CmmTx

import CLabel
import FastString
import ForeignCall
import qualified ZipCfg as Z
import qualified ZipDataflow as DF
import ZipCfg 
import MkZipCfg
import Util

import Maybes
import Monad
import Outputable
import Prelude hiding (zip, unzip, last)
import qualified Data.List as L
import UniqSupply

----------------------------------------------------------------------
----- Type synonyms and definitions

type CmmGraph                = LGraph Middle Last
type CmmAGraph               = AGraph Middle Last
type CmmBlock                = Block  Middle Last
type CmmZ                    = GenCmm    CmmStatic CmmInfo CmmGraph
type CmmTopZ                 = GenCmmTop CmmStatic CmmInfo CmmGraph
type CmmBackwardFixedPoint a = DF.BackwardFixedPoint Middle Last a ()
type CmmForwardFixedPoint  a = DF.ForwardFixedPoint  Middle Last a ()

data Middle
  = MidComment FastString

  | MidAssign CmmReg CmmExpr     -- Assign to register

  | MidStore  CmmExpr CmmExpr    -- Assign to memory location.  Size is
                                 -- given by cmmExprType of the rhs.

  | MidUnsafeCall                -- An "unsafe" foreign call;
     MidCallTarget               -- just a fat machine instructoin
     CmmFormals                  -- zero or more results
     CmmActuals                  -- zero or more arguments

  | MidAddToContext              -- Push a frame on the stack;
                                 --    I will return to this frame
     CmmExpr                     -- The frame's return address; it must be
                                 -- preceded by an info table that describes the
                                 -- live variables.
     [CmmExpr]                   -- The frame's live variables, to go on the 
                                 -- stack with the first one at the young end
  deriving Eq

data Last
  = LastBranch BlockId  -- Goto another block in the same procedure

  | LastCondBranch {            -- conditional branch
        cml_pred :: CmmExpr,
        cml_true, cml_false :: BlockId
    }
  | LastSwitch CmmExpr [Maybe BlockId]   -- Table branch
        -- The scrutinee is zero-based; 
        --      zero -> first block
        --      one  -> second block etc
        -- Undefined outside range, and when there's a Nothing
  | LastReturn Int       -- Return from a function; values in previous copy middles
  | LastJump CmmExpr Int -- Tail call to another procedure; args in a copy middles
  | LastCall {                      -- A call (native or safe foreign); args in copy middles
        cml_target :: CmmExpr,      -- never a CmmPrim to a CallishMachOp!
        cml_cont   :: Maybe BlockId,-- BlockId of continuation, if call returns
        cml_args   :: Int }     -- liveness info for outgoing args
  -- All the last nodes that pass arguments carry the size of the outgoing CallArea

data MidCallTarget	-- The target of a MidUnsafeCall
  = ForeignTarget 	-- A foreign procedure
	CmmExpr			-- Its address
	ForeignConvention	-- Its calling convention

  | PrimTarget		-- A possibly-side-effecting machine operation
	CallishMachOp		-- Which one
  deriving Eq

data Convention
  = Native 		-- Native C-- call/return

  | Foreign		-- Foreign call/return
	ForeignConvention

  | Private
        -- Used for control transfers within a (pre-CPS) procedure All
        -- jump sites known, never pushed on the stack (hence no SRT)
        -- You can choose whatever calling convention you please
        -- (provided you make sure all the call sites agree)!
        -- This data type eventually to be extended to record the convention. 
  deriving( Eq )

data ForeignConvention
  = ForeignConvention
	CCallConv 		-- Which foreign-call convention
	[ForeignHint]		-- Extra info about the args
	[ForeignHint]		-- Extra info about the result
  deriving Eq 

data ValueDirection = Arguments | Results
  -- Arguments go with procedure definitions, jumps, and arguments to calls
  -- Results go with returns and with results of calls.
  deriving Eq

----------------------------------------------------------------------
----- Splicing between blocks
-- Given a middle node, a block, and a successor BlockId,
-- we can insert the middle node between the block and the successor.
-- We return the updated block and a list of new blocks that must be added
-- to the graph.
-- The semantics is a bit tricky. We consider cases on the last node:
-- o For a branch, we can just insert before the branch,
--   but sometimes the optimizer does better if we actually insert
--   a fresh basic block, enabling some common blockification.
-- o For a conditional branch, switch statement, or call, we must insert
--   a new basic block.
-- o For a jump or return, this operation is impossible.

insertBetween :: MonadUnique m => CmmBlock -> [Middle] -> BlockId -> m (CmmBlock, [CmmBlock])
insertBetween b ms succId = insert $ goto_end $ unzip b
  where insert (h, LastOther (LastBranch bid)) =
          if bid == succId then
            do (bid', bs) <- newBlocks
               return (zipht h (ZLast (LastOther (LastBranch bid'))), bs)
          else panic "tried invalid block insertBetween"
        insert (h, LastOther (LastCondBranch c t f)) =
          do (t', tbs) <- if t == succId then newBlocks else return $ (t, [])
             (f', fbs) <- if f == succId then newBlocks else return $ (f, [])
             return (zipht h $ ZLast $ LastOther (LastCondBranch c t' f'), tbs ++ fbs)
        insert (h, LastOther (LastSwitch e ks)) =
          do (ids, bs) <- mapAndUnzipM mbNewBlocks ks
             return (zipht h $ ZLast $ LastOther (LastSwitch e ids), join bs)
        insert (_, LastOther (LastCall _ _ _)) =
          panic "unimp: insertBetween after a call -- probably not a good idea"
        insert (_, LastOther (LastReturn _)) = panic "cannot insert after return"
        insert (_, LastOther (LastJump _ _)) = panic "cannot insert after jump"
        insert (_, LastExit) = panic "cannot insert after exit"
        newBlocks = do id <- liftM BlockId $ getUniqueM
                       return $ (id, [Block id Nothing $
                                   foldr ZTail (ZLast (LastOther (LastBranch succId))) ms])
        mbNewBlocks (Just k) = if k == succId then liftM lift newBlocks
                               else return (Just k, [])
        mbNewBlocks Nothing  = return (Nothing, [])
        lift (id, bs) = (Just id, bs)

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
cmmSuccs (LastReturn _)           = []
cmmSuccs (LastJump {})            = [] 
cmmSuccs (LastBranch id)          = [id]
cmmSuccs (LastCall _ (Just id) _) = [id]
cmmSuccs (LastCall _ Nothing _)   = []
cmmSuccs (LastCondBranch _ t f)   = [f, t]  -- meets layout constraint
cmmSuccs (LastSwitch _ edges)     = catMaybes edges

fold_cmm_succs :: (BlockId -> a -> a) -> Last -> a -> a
fold_cmm_succs _f (LastReturn _)           z = z
fold_cmm_succs _f (LastJump {})            z = z
fold_cmm_succs  f (LastBranch id)          z = f id z
fold_cmm_succs  f (LastCall _ (Just id) _) z = f id z
fold_cmm_succs _f (LastCall _ Nothing _)   z = z
fold_cmm_succs  f (LastCondBranch _ te fe) z = f te (f fe z)
fold_cmm_succs  f (LastSwitch _ edges)     z = foldl (flip f) z $ catMaybes edges

----------------------------------------------------------------------
----- Instance declarations for register use

instance UserOfLocalRegs Middle where
    foldRegsUsed f z m = middle m
      where middle (MidComment {})            = z
            middle (MidAssign _lhs expr)      = fold f z expr
            middle (MidStore addr rval)       = fold f (fold f z addr) rval
            middle (MidUnsafeCall tgt _ args) = fold f (fold f z tgt) args
            middle (MidAddToContext ra args)  = fold f (fold f z ra) args
            fold f z m = foldRegsUsed f z m  -- avoid monomorphism restriction

instance UserOfLocalRegs MidCallTarget where
  foldRegsUsed _f z (PrimTarget _)      = z
  foldRegsUsed f  z (ForeignTarget e _) = foldRegsUsed f z e

instance UserOfSlots MidCallTarget where
  foldSlotsUsed _f z (PrimTarget _)      = z
  foldSlotsUsed f  z (ForeignTarget e _) = foldSlotsUsed f z e

instance UserOfLocalRegs Last where
    foldRegsUsed f z l = last l
      where last (LastReturn _)         = z
            last (LastJump e _)         = foldRegsUsed f z e
            last (LastBranch _id)       = z
            last (LastCall tgt _ _)     = foldRegsUsed f z tgt
            last (LastCondBranch e _ _) = foldRegsUsed f z e
            last (LastSwitch e _tbl)    = foldRegsUsed f z e

instance DefinerOfLocalRegs Middle where
    foldRegsDefd f z m = middle m
      where middle (MidComment {})         = z
            middle (MidAssign _lhs _)      = fold f z _lhs
            middle (MidStore _ _)          = z
            middle (MidUnsafeCall _ _ _)   = z
            middle (MidAddToContext _ _)   = z
            fold f z m = foldRegsDefd f z m  -- avoid monomorphism restriction

instance DefinerOfLocalRegs Last where
    foldRegsDefd _ z _ = z


----------------------------------------------------------------------
----- Instance declarations for stack slot use

instance UserOfSlots Middle where
    foldSlotsUsed f z m = middle m
      where middle (MidComment {})                = z
            middle (MidAssign _lhs expr)          = fold f z expr
            middle (MidStore addr rval)           = fold f (fold f z addr) rval
            middle (MidUnsafeCall tgt _ress args) = fold f (fold f z tgt) args
            middle (MidAddToContext ra args)      = fold f (fold f z ra) args
            fold f z e = foldSlotsUsed f z e  -- avoid monomorphism restriction

instance UserOfSlots Last where
    foldSlotsUsed f z l = last l
      where last (LastReturn _)         = z
            last (LastJump e _)         = foldSlotsUsed f z e
            last (LastBranch _id)       = z
            last (LastCall tgt _ _)     = foldSlotsUsed f z tgt
            last (LastCondBranch e _ _) = foldSlotsUsed f z e
            last (LastSwitch e _tbl)    = foldSlotsUsed f z e

instance UserOfSlots l => UserOfSlots (ZLast l) where
    foldSlotsUsed f z (LastOther l) = foldSlotsUsed f z l
    foldSlotsUsed _ z LastExit      = z

instance DefinerOfSlots Middle where
    foldSlotsDefd f z m = middle m
      where middle (MidComment {})       = z
            middle (MidAssign _ _)       = z
            middle (MidStore (CmmStackSlot a i) e) =
              f z (a, i, widthInBytes $ typeWidth $ cmmExprType e)
            middle (MidStore _ _)        = z
            middle (MidUnsafeCall _ _ _) = z
            middle (MidAddToContext _ _) = z

instance DefinerOfSlots Last where
    foldSlotsDefd _ z _ = z

instance DefinerOfSlots l => DefinerOfSlots (ZLast l) where
    foldSlotsDefd f z (LastOther l) = foldSlotsDefd f z l
    foldSlotsDefd _ z LastExit      = z

----------------------------------------------------------------------
----- Code for manipulating Middle and Last nodes

mapExpMiddle :: (CmmExpr -> CmmExpr) -> Middle -> Middle
mapExpMiddle _   m@(MidComment _)            = m
mapExpMiddle exp   (MidAssign r e)           = MidAssign r (exp e)
mapExpMiddle exp   (MidStore addr e)         = MidStore (exp addr) (exp e)
mapExpMiddle exp   (MidUnsafeCall tgt fs as) =
  MidUnsafeCall (mapExpMidcall exp tgt) fs (map exp as)
mapExpMiddle exp   (MidAddToContext e es)    = MidAddToContext (exp e) (map exp es)

foldExpMiddle :: (CmmExpr -> z -> z) -> Middle -> z -> z
foldExpMiddle _   (MidComment _)           z = z
foldExpMiddle exp (MidAssign _ e)          z = exp e z
foldExpMiddle exp (MidStore addr e)        z = exp addr $ exp e z
foldExpMiddle exp (MidUnsafeCall tgt _ as) z = foldExpMidcall exp tgt $ foldr exp z as
foldExpMiddle exp (MidAddToContext e es)   z = exp e $ foldr exp z es

mapExpLast :: (CmmExpr -> CmmExpr) -> Last -> Last
mapExpLast _   l@(LastBranch _)         = l
mapExpLast exp (LastCondBranch e ti fi) = LastCondBranch (exp e) ti fi
mapExpLast exp (LastSwitch e tbl)       = LastSwitch (exp e) tbl
mapExpLast exp (LastCall tgt mb_id s)   = LastCall (exp tgt) mb_id s
mapExpLast exp (LastJump e s)           = LastJump (exp e) s
mapExpLast _   (LastReturn s)           = LastReturn s

foldExpLast :: (CmmExpr -> z -> z) -> Last -> z -> z
foldExpLast _   (LastBranch _)         z = z
foldExpLast exp (LastCondBranch e _ _) z = exp e z
foldExpLast exp (LastSwitch e _)       z = exp e z
foldExpLast exp (LastCall tgt _ _)     z = exp tgt z
foldExpLast exp (LastJump e _)         z = exp e z
foldExpLast _   (LastReturn _)         z = z

mapExpMidcall :: (CmmExpr -> CmmExpr) -> MidCallTarget -> MidCallTarget 
mapExpMidcall exp   (ForeignTarget e c) = ForeignTarget (exp e) c
mapExpMidcall _   m@(PrimTarget _)      = m

foldExpMidcall :: (CmmExpr -> z -> z) -> MidCallTarget -> z -> z 
foldExpMidcall exp (ForeignTarget e _) z = exp e z
foldExpMidcall _   (PrimTarget _)      z = z

-- Take a transformer on expressions and apply it recursively.
wrapRecExp :: (CmmExpr -> CmmExpr) -> CmmExpr -> CmmExpr
wrapRecExp f (CmmMachOp op es)    = f (CmmMachOp op $ map f es)
wrapRecExp f (CmmLoad addr ty)    = f (CmmLoad (f addr) ty)
wrapRecExp f e                    = f e

mapExpDeepMiddle :: (CmmExpr -> CmmExpr) -> Middle -> Middle
mapExpDeepLast   :: (CmmExpr -> CmmExpr) -> Last   -> Last
mapExpDeepMiddle f = mapExpMiddle $ wrapRecExp f
mapExpDeepLast   f = mapExpLast   $ wrapRecExp f

-- Take a folder on expressions and apply it recursively.
wrapRecExpf :: (CmmExpr -> z -> z) -> CmmExpr -> z -> z
wrapRecExpf f e@(CmmMachOp _ es) z = foldr f (f e z) es
wrapRecExpf f e@(CmmLoad addr _) z = f addr  (f e z)
wrapRecExpf f e                  z = f e z

foldExpDeepMiddle :: (CmmExpr -> z -> z) -> Middle -> z -> z
foldExpDeepLast   :: (CmmExpr -> z -> z) -> Last   -> z -> z
foldExpDeepMiddle f = foldExpMiddle $ wrapRecExpf f
foldExpDeepLast   f = foldExpLast   $ wrapRecExpf f

----------------------------------------------------------------------
-- Compute the join of facts live out of a Last node. Useful for most backward
-- analyses.
joinOuts :: DataflowLattice a -> (BlockId -> a) -> Last -> a
joinOuts lattice env l =
  let bot  = fact_bot lattice
      join x y = txVal $ fact_add_to lattice x y
  in case l of
       (LastReturn _)          -> bot
       (LastJump _ _)          -> bot
       (LastBranch id)         -> env id
       (LastCall _ Nothing _)  -> bot
       (LastCall _ (Just k) _) -> env k
       (LastCondBranch _ t f)  -> join (env t) (env f)
       (LastSwitch _ tbl)      -> foldr join bot (map env $ catMaybes tbl)

----------------------------------------------------------------------
----- Instance declarations for prettyprinting (avoids recursive imports)

instance Outputable Middle where
    ppr s = pprMiddle s

instance Outputable Last where
    ppr s = pprLast s

instance Outputable Convention where
    ppr = pprConvention

instance Outputable ForeignConvention where
    ppr = pprForeignConvention

instance Outputable ValueDirection where
    ppr Arguments = ptext $ sLit "args"
    ppr Results   = ptext $ sLit "results"

instance DF.DebugNodes Middle Last

debugPpr :: Bool
debugPpr = debugIsOn

pprMiddle :: Middle -> SDoc    
pprMiddle stmt = pp_stmt <+> pp_debug
  where
    pp_stmt = case stmt of
    	--  // text
    	MidComment s -> text "//" <+> ftext s

    	-- reg = expr;
    	MidAssign reg expr -> ppr reg <+> equals <+> ppr expr <> semi

    	-- rep[lv] = expr;
    	MidStore lv expr -> rep <> brackets(ppr lv) <+> equals <+> ppr expr <> semi
    	    where
    	      rep = ppr ( cmmExprType expr )

    	-- call "ccall" foo(x, y)[r1, r2];
    	-- ToDo ppr volatile
    	MidUnsafeCall target results args ->
    	    hsep [ if null results
    	              then empty
    	              else parens (commafy $ map ppr results) <+> equals,
    	           ptext $ sLit "call", 
    	           ppr_call_target target <> parens (commafy $ map ppr args) <> semi]

    	MidAddToContext ra args ->
    	    hcat [ ptext $ sLit "return via "
    	         , ppr_target ra, parens (commafy $ map ppr args), semi ]
  
    pp_debug =
      if not debugPpr then empty
      else text " //" <+>
           case stmt of
             MidComment {} -> text "MidComment"
             MidAssign {}  -> text "MidAssign"
             MidStore {}   -> text "MidStore"
             MidUnsafeCall  {} -> text "MidUnsafeCall"
             MidAddToContext {} -> text "MidAddToContext"

ppr_fc :: ForeignConvention -> SDoc
ppr_fc (ForeignConvention c _ _) = doubleQuotes (ppr c)

ppr_call_target :: MidCallTarget -> SDoc
ppr_call_target (ForeignTarget fn c) = ppr_fc c <+> ppr_target fn
ppr_call_target (PrimTarget op)      = ppr (CmmLabel (mkForeignLabel (mkFastString (show op)) Nothing False))

ppr_target :: CmmExpr -> SDoc
ppr_target t@(CmmLit _) = ppr t
ppr_target fn'          = parens (ppr fn')

pprHinted :: Outputable a => CmmHinted a -> SDoc
pprHinted (CmmHinted a NoHint)     = ppr a
pprHinted (CmmHinted a AddrHint)   = doubleQuotes (text "address") <+> ppr a
pprHinted (CmmHinted a SignedHint) = doubleQuotes (text "signed")  <+> ppr a

pprLast :: Last -> SDoc    
pprLast stmt = pp_stmt <+> pp_debug
  where
    pp_stmt = case stmt of
       LastBranch ident          -> ptext (sLit "goto") <+> ppr ident <> semi
       LastCondBranch expr t f   -> genFullCondBranch expr t f
       LastJump expr _           -> hcat [ ptext (sLit "jump"), space, pprFun expr
                                         , ptext (sLit "(...)"), semi]
       LastReturn _              -> hcat [ ptext (sLit "return"), space 
                                         , ptext (sLit "(...)"), semi]
       LastSwitch arg ids        -> ppr $ CmmSwitch arg ids
       LastCall tgt k _          -> genBareCall tgt k

    pp_debug = text " //" <+> case stmt of
           LastBranch {} -> text "LastBranch"
           LastCondBranch {} -> text "LastCondBranch"
           LastJump {} -> text "LastJump"
           LastReturn {} -> text "LastReturn"
           LastSwitch {} -> text "LastSwitch"
           LastCall {} -> text "LastCall"

genBareCall :: CmmExpr -> Maybe BlockId -> SDoc
genBareCall fn k =
        hcat [ ptext (sLit "call"), space
             , pprFun fn, ptext (sLit "(...)"), space
             , case k of Nothing -> ptext (sLit "never returns")
                         Just k -> ptext (sLit "returns to") <+> ppr k
             , semi ]
        where

pprFun :: CmmExpr -> SDoc
pprFun f@(CmmLit _) = ppr f
pprFun f = parens (ppr f)

genFullCondBranch :: Outputable id => CmmExpr -> id -> id -> SDoc
genFullCondBranch expr t f =
    hsep [ ptext (sLit "if")
         , parens(ppr expr)
         , ptext (sLit "goto")
         , ppr t <> semi
         , ptext (sLit "else goto")
         , ppr f <> semi
         ]

pprConvention :: Convention -> SDoc
pprConvention (Native {})  = empty
pprConvention (Foreign c)  = ppr c
pprConvention (Private {}) = text "<private-convention>"

pprForeignConvention :: ForeignConvention -> SDoc
pprForeignConvention (ForeignConvention c as rs) = ppr c <> ppr as <> ppr rs

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs
