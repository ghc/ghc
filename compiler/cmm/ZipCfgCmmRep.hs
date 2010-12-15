{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
-- Norman likes local bindings

-- This module is pure representation and should be imported only by
-- clients that need to manipulate representation and know what
-- they're doing.  Clients that need to create flow graphs should
-- instead import MkZipCfgCmm.

module ZipCfgCmmRep
  ( CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph
  , Middle(..), Last(..), MidCallTarget(..), UpdFrameOffset
  , Convention(..), ForeignConvention(..), ForeignSafety(..)
  , ValueDirection(..), ForeignHint(..)
  , CmmBackwardFixedPoint, CmmForwardFixedPoint, pprHinted
  , insertBetween, mapExpMiddle, mapExpLast, mapExpDeepMiddle, mapExpDeepLast
  , foldExpMiddle, foldExpLast, foldExpDeepMiddle, foldExpDeepLast, joinOuts
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
import qualified ZipDataflow as DF
import ZipCfg 
import MkZipCfg
import Util

import BasicTypes
import Maybes
import Control.Monad
import Outputable
import Prelude hiding (zip, unzip, last)
import SMRep (ByteOff)
import UniqSupply

----------------------------------------------------------------------
----- Type synonyms and definitions

type CmmGraph                = LGraph Middle Last
type CmmAGraph               = AGraph Middle Last
type CmmBlock                = Block  Middle Last
type CmmStackInfo            = (ByteOff, Maybe ByteOff)
  -- probably want a record; (SP offset on entry, update frame space)
type CmmZ                    = GenCmm    CmmStatic CmmInfo (CmmStackInfo, CmmGraph)
type CmmTopZ                 = GenCmmTop CmmStatic CmmInfo (CmmStackInfo, CmmGraph)
type CmmBackwardFixedPoint a = DF.BackwardFixedPoint Middle Last a ()
type CmmForwardFixedPoint  a = DF.ForwardFixedPoint  Middle Last a ()

type UpdFrameOffset = ByteOff

data Middle
  = MidComment FastString

  | MidAssign CmmReg CmmExpr     -- Assign to register

  | MidStore  CmmExpr CmmExpr    -- Assign to memory location.  Size is
                                 -- given by cmmExprType of the rhs.

  | MidForeignCall               -- A foreign call; see Note [Foreign calls]
     ForeignSafety               -- Is it a safe or unsafe call?
     MidCallTarget               -- call target and convention
     CmmFormals                  -- zero or more results
     CmmActuals                  -- zero or more arguments
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
  | LastCall {                   -- A call (native or safe foreign)
        cml_target :: CmmExpr,  -- never a CmmPrim to a CallishMachOp!

        cml_cont :: Maybe BlockId,
            -- BlockId of continuation (Nothing for return or tail call)

        cml_args :: ByteOff, 
 	    -- Byte offset, from the *old* end of the Area associated with
            -- the BlockId (if cml_cont = Nothing, then Old area), of
            -- youngest outgoing arg.  Set the stack pointer to this before
	    -- transferring control.
  	    -- (NB: an update frame might also have been stored in the Old
	    --      area, but it'll be in an older part than the args.)

        cml_ret_args :: ByteOff,  
	    -- For calls *only*, the byte offset for youngest returned value
	    -- This is really needed at the *return* point rather than here
	    -- at the call, but in practice it's convenient to record it here.

        cml_ret_off :: Maybe ByteOff
          -- For calls *only*, the byte offset of the base of the frame that
	  -- must be described by the info table for the return point.  
 	  -- The older words are an update frames, which have their own
	  -- info-table and layout information

	  -- From a liveness point of view, the stack words older than
	  -- cml_ret_off are treated as live, even if the sequel of
	  -- the call goes into a loop.
	}

data MidCallTarget        -- The target of a MidUnsafeCall
  = ForeignTarget         -- A foreign procedure
        CmmExpr                  -- Its address
        ForeignConvention        -- Its calling convention

  | PrimTarget            -- A possibly-side-effecting machine operation
        CallishMachOp            -- Which one
  deriving Eq

data Convention
  = NativeDirectCall -- Native C-- call skipping the node (closure) argument
  
  | NativeNodeCall   -- Native C-- call including the node argument

  | NativeReturn     -- Native C-- return

  | Slow             -- Slow entry points: all args pushed on the stack

  | GC               -- Entry to the garbage collector: uses the node reg!

  | PrimOpCall       -- Calling prim ops

  | PrimOpReturn     -- Returning from prim ops

  | Foreign          -- Foreign call/return
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

data ForeignSafety
  = Unsafe              -- unsafe call
  | Safe BlockId        -- making infotable requires: 1. label 
         UpdFrameOffset --                            2. where the upd frame is
         Bool           -- is the call interruptible?
  deriving Eq

data ValueDirection = Arguments | Results
  -- Arguments go with procedure definitions, jumps, and arguments to calls
  -- Results go with returns and with results of calls.
  deriving Eq
 
{- Note [Foreign calls]
~~~~~~~~~~~~~~~~~~~~~~~
A MidForeign call is used *all* foreign calls, both *unsafe* and *safe*.
Unsafe ones are easy: think of them as a "fat machine instruction".

Safe ones are trickier.  A safe foreign call 
     r = f(x)
ultimately expands to
     push "return address"	-- Never used to return to; 
     	  	  		-- just points an info table
     save registers into TSO
     call suspendThread
     r = f(x)			-- Make the call
     call resumeThread
     restore registers
     pop "return address"
We cannot "lower" a safe foreign call to this sequence of Cmms, because
after we've saved Sp all the Cmm optimiser's assumptions are broken.
Furthermore, currently the smart Cmm constructors know the calling
conventions for Haskell, the garbage collector, etc, and "lower" them
so that a LastCall passes no parameters or results.  But the smart 
constructors do *not* (currently) know the foreign call conventions.

For these reasons use MidForeignCall for all calls. The only annoying thing
is that a safe foreign call needs an info table.
-}

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
        insert (_, LastOther (LastCall {})) =
          panic "unimp: insertBetween after a call -- probably not a good idea"
        insert (_, LastExit) = panic "cannot insert after exit"
        newBlocks = do id <- liftM BlockId $ getUniqueM
                       return $ (id, [Block id $
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
cmmSuccs (LastBranch id)              = [id]
cmmSuccs (LastCall _ Nothing   _ _ _) = []
cmmSuccs (LastCall _ (Just id) _ _ _) = [id]
cmmSuccs (LastCondBranch _ t f)       = [f, t]  -- meets layout constraint
cmmSuccs (LastSwitch _ edges)         = catMaybes edges

fold_cmm_succs :: (BlockId -> a -> a) -> Last -> a -> a
fold_cmm_succs  f (LastBranch id)              z = f id z
fold_cmm_succs  _ (LastCall _ Nothing _ _ _)   z = z
fold_cmm_succs  f (LastCall _ (Just id) _ _ _) z = f id z
fold_cmm_succs  f (LastCondBranch _ te fe)     z = f te (f fe z)
fold_cmm_succs  f (LastSwitch _ edges)         z = foldl (flip f) z $ catMaybes edges

----------------------------------------------------------------------
----- Instance declarations for register use

instance UserOfLocalRegs Middle where
    foldRegsUsed f z m = middle m
      where middle (MidComment {})               = z
            middle (MidAssign _lhs expr)         = fold f z expr
            middle (MidStore addr rval)          = fold f (fold f z addr) rval
            middle (MidForeignCall _ tgt _ args) = fold f (fold f z tgt) args
            fold f z m = foldRegsUsed f z m  -- avoid monomorphism restriction

instance UserOfLocalRegs MidCallTarget where
  foldRegsUsed _f z (PrimTarget _)      = z
  foldRegsUsed f  z (ForeignTarget e _) = foldRegsUsed f z e

instance UserOfSlots MidCallTarget where
  foldSlotsUsed  f z (ForeignTarget e _) = foldSlotsUsed f z e
  foldSlotsUsed _f z (PrimTarget _)      = z

instance (UserOfLocalRegs a) => UserOfLocalRegs (Maybe a) where
  foldRegsUsed f z (Just x) = foldRegsUsed f z x
  foldRegsUsed _ z Nothing  = z

instance (UserOfSlots a) => UserOfSlots (Maybe a) where
  foldSlotsUsed f z (Just x) = foldSlotsUsed f z x
  foldSlotsUsed _ z Nothing  = z

instance UserOfLocalRegs Last where
    foldRegsUsed f z l = last l
      where last (LastBranch _id)       = z
            last (LastCall tgt _ _ _ _) = foldRegsUsed f z tgt
            last (LastCondBranch e _ _) = foldRegsUsed f z e
            last (LastSwitch e _tbl)    = foldRegsUsed f z e

instance DefinerOfLocalRegs Middle where
    foldRegsDefd f z m = middle m
      where middle (MidComment {})           = z
            middle (MidAssign lhs _)         = fold f z lhs
            middle (MidStore _ _)            = z
            middle (MidForeignCall _ _ fs _) = fold f z fs
            fold f z m = foldRegsDefd f z m  -- avoid monomorphism restriction

instance DefinerOfLocalRegs Last where
    foldRegsDefd _ z _ = z


----------------------------------------------------------------------
----- Instance declarations for stack slot use

instance UserOfSlots Middle where
    foldSlotsUsed f z m = middle m
      where middle (MidComment {})                   = z
            middle (MidAssign _lhs expr)             = fold f z expr
            middle (MidStore addr rval)              = fold f (fold f z addr) rval
            middle (MidForeignCall _ tgt _ress args) = fold f (fold f z tgt) args
            fold f z e = foldSlotsUsed f z e  -- avoid monomorphism restriction

instance UserOfSlots Last where
    foldSlotsUsed f z l = last l
      where last (LastBranch _id)       = z
            last (LastCall tgt _ _ _ _) = foldSlotsUsed f z tgt
            last (LastCondBranch e _ _) = foldSlotsUsed f z e
            last (LastSwitch e _tbl)    = foldSlotsUsed f z e

instance UserOfSlots l => UserOfSlots (ZLast l) where
    foldSlotsUsed f z (LastOther l) = foldSlotsUsed f z l
    foldSlotsUsed _ z LastExit      = z

instance DefinerOfSlots Middle where
    foldSlotsDefd f z m = middle m
      where middle (MidComment {})    = z
            middle (MidAssign _ _)    = z
            middle (MidForeignCall {}) = z
            middle (MidStore (CmmStackSlot a i) e) =
              f z (a, i, widthInBytes $ typeWidth $ cmmExprType e)
            middle (MidStore _ _)     = z

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
mapExpMiddle exp   (MidForeignCall s tgt fs as) =
  MidForeignCall s (mapExpMidcall exp tgt) fs (map exp as)

foldExpMiddle :: (CmmExpr -> z -> z) -> Middle -> z -> z
foldExpMiddle _   (MidComment _)              z = z
foldExpMiddle exp (MidAssign _ e)             z = exp e z
foldExpMiddle exp (MidStore addr e)           z = exp addr $ exp e z
foldExpMiddle exp (MidForeignCall _ tgt _ as) z = foldExpMidcall exp tgt $ foldr exp z as

mapExpLast :: (CmmExpr -> CmmExpr) -> Last -> Last
mapExpLast _   l@(LastBranch _)           = l
mapExpLast exp (LastCondBranch e ti fi)   = LastCondBranch (exp e) ti fi
mapExpLast exp (LastSwitch e tbl)         = LastSwitch (exp e) tbl
mapExpLast exp (LastCall tgt mb_id o i s) = LastCall (exp tgt) mb_id o i s

foldExpLast :: (CmmExpr -> z -> z) -> Last -> z -> z
foldExpLast _   (LastBranch _)         z = z
foldExpLast exp (LastCondBranch e _ _) z = exp e z
foldExpLast exp (LastSwitch e _)       z = exp e z
foldExpLast exp (LastCall tgt _ _ _ _) z = exp tgt z

mapExpMidcall :: (CmmExpr -> CmmExpr) -> MidCallTarget -> MidCallTarget 
mapExpMidcall exp   (ForeignTarget e c) = ForeignTarget (exp e) c
mapExpMidcall _   m@(PrimTarget _)      = m

foldExpMidcall :: (CmmExpr -> z -> z) -> MidCallTarget -> z -> z 
foldExpMidcall exp (ForeignTarget e _) z = exp e z
foldExpMidcall _   (PrimTarget _)      z = z

-- Take a transformer on expressions and apply it recursively.
wrapRecExp :: (CmmExpr -> CmmExpr) -> CmmExpr -> CmmExpr
wrapRecExp f (CmmMachOp op es)    = f (CmmMachOp op $ map (wrapRecExp f) es)
wrapRecExp f (CmmLoad addr ty)    = f (CmmLoad (wrapRecExp f addr) ty)
wrapRecExp f e                    = f e

mapExpDeepMiddle :: (CmmExpr -> CmmExpr) -> Middle -> Middle
mapExpDeepLast   :: (CmmExpr -> CmmExpr) -> Last   -> Last
mapExpDeepMiddle f = mapExpMiddle $ wrapRecExp f
mapExpDeepLast   f = mapExpLast   $ wrapRecExp f

-- Take a folder on expressions and apply it recursively.
wrapRecExpf :: (CmmExpr -> z -> z) -> CmmExpr -> z -> z
wrapRecExpf f e@(CmmMachOp _ es) z = foldr (wrapRecExpf f) (f e z) es
wrapRecExpf f e@(CmmLoad addr _) z = wrapRecExpf f addr (f e z)
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
       (LastBranch id)             -> env id
       (LastCall _ Nothing _ _ _)  -> bot
       (LastCall _ (Just k) _ _ _) -> env k
       (LastCondBranch _ t f)      -> join (env t) (env f)
       (LastSwitch _ tbl)          -> foldr join bot (map env $ catMaybes tbl)

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
    	MidForeignCall safety target results args ->
    	    hsep [ ppUnless (null results) $
    	              parens (commafy $ map ppr results) <+> equals,
                   ppr_safety safety,
    	           ptext $ sLit "call", 
    	           ppr_call_target target <> parens (commafy $ map ppr args) <> semi]

    pp_debug =
      if not debugPpr then empty
      else text " //" <+>
           case stmt of
             MidComment     {} -> text "MidComment"
             MidAssign      {} -> text "MidAssign"
             MidStore       {} -> text "MidStore"
             MidForeignCall {} -> text "MidForeignCall"

ppr_fc :: ForeignConvention -> SDoc
ppr_fc (ForeignConvention c args res) =
  doubleQuotes (ppr c) <+> text "args: " <+> ppr args <+> text " results: " <+> ppr res

ppr_safety :: ForeignSafety -> SDoc
ppr_safety (Safe bid upd interruptible) =
    text (if interruptible then "interruptible" else "safe") <>
    text "<" <> ppr bid <> text ", " <> ppr upd <> text ">"
ppr_safety Unsafe         = text "unsafe"

ppr_call_target :: MidCallTarget -> SDoc
ppr_call_target (ForeignTarget fn c) = ppr_fc c <+> ppr_target fn
ppr_call_target (PrimTarget op) 
 -- HACK: We're just using a ForeignLabel to get this printed, the label
 --	  might not really be foreign.
 = ppr (CmmLabel (mkForeignLabel
 			(mkFastString (show op)) 
			Nothing ForeignLabelInThisPackage IsFunction))

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
       LastBranch ident                -> ptext (sLit "goto") <+> ppr ident <> semi
       LastCondBranch expr t f         -> genFullCondBranch expr t f
       LastSwitch arg ids              -> ppr $ CmmSwitch arg ids
       LastCall tgt k out res updfr_off -> genBareCall tgt k out res updfr_off

    pp_debug = text " //" <+> case stmt of
           LastBranch {} -> text "LastBranch"
           LastCondBranch {} -> text "LastCondBranch"
           LastSwitch {} -> text "LastSwitch"
           LastCall {} -> text "LastCall"

genBareCall :: CmmExpr -> Maybe BlockId -> ByteOff -> ByteOff ->
                          Maybe UpdFrameOffset -> SDoc
genBareCall fn k out res updfr_off =
        hcat [ ptext (sLit "call"), space
             , pprFun fn, ptext (sLit "(...)"), space
             , ptext (sLit "returns to") <+> ppr k <+> parens (ppr out)
                                                   <+> parens (ppr res)
             , ptext (sLit " with update frame") <+> ppr updfr_off
             , semi ]

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
pprConvention (NativeNodeCall   {}) = text "<native-node-call-convention>"
pprConvention (NativeDirectCall {}) = text "<native-direct-call-convention>"
pprConvention (NativeReturn {})     = text "<native-ret-convention>"
pprConvention  Slow                 = text "<slow-convention>"
pprConvention  GC                   = text "<gc-convention>"
pprConvention  PrimOpCall           = text "<primop-call-convention>"
pprConvention  PrimOpReturn         = text "<primop-ret-convention>"
pprConvention (Foreign c)           = ppr c
pprConvention (Private {})          = text "<private-convention>"

pprForeignConvention :: ForeignConvention -> SDoc
pprForeignConvention (ForeignConvention c as rs) = ppr c <> ppr as <> ppr rs

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs
