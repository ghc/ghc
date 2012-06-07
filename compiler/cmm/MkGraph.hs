{-# LANGUAGE GADTs #-}

-- ToDo: remove -fno-warn-warnings-deprecations
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- ToDo: remove -fno-warn-incomplete-patterns
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- Module for building CmmAGraphs.

-- As the CmmAGraph is a wrapper over Graph CmmNode O x, it is different
-- from Hoopl's AGraph. The current clients expect functions with the
-- same names Hoopl uses, so this module cannot be in the same namespace
-- as Compiler.Hoopl.

module MkGraph
  ( CmmAGraph
  , emptyAGraph, (<*>), catAGraphs, outOfLine
  , mkLabel, mkMiddle, mkLast
  , withFreshLabel, withUnique, lgraphOfAGraph, labelAGraph

  , stackStubExpr
  , mkNop, mkAssign, mkStore, mkCall, mkCmmCall, mkSafeCall, mkUnsafeCall, mkFinalCall
         , mkJump, mkDirectJump, mkForeignJump, mkJumpGC, mkCbranch, mkSwitch
         , mkReturn, mkReturnSimple, mkComment, mkCallEntry
         , mkBranch, mkCmmIfThenElse, mkCmmIfThen, mkCmmWhileDo
         , copyInOflow, copyInSlot, copyOutOflow, copyOutSlot
  )
where

import BlockId
import Cmm
import CmmCallConv (assignArgumentsPos, ParamLocation(..))

import Compiler.Hoopl hiding (Unique, (<*>), mkFirst, mkMiddle, mkLast, mkLabel, mkBranch, Shape(..))
import qualified Compiler.Hoopl as H
import Compiler.Hoopl.GHC (uniqueToLbl)
import FastString
import ForeignCall
import Outputable
import Prelude hiding (succ)
import SMRep (ByteOff)
import StaticFlags
import Unique
import UniqSupply
import Util

#include "HsVersions.h"

{-
A 'CmmAGraph' is an abstract version of a 'Graph CmmNode O x' from module
'Cmm'.  The difference is that the 'CmmAGraph' can be eigher open of closed at
exit and it can supply fresh Labels and Uniques.

It also supports a splicing operation <*>, which is different from the Hoopl's
<*>, because it splices two CmmAGraphs. Specifically, it can splice Graph
O C and Graph O x. In this case, the open beginning of the second graph is
thrown away.  In the debug mode this sequence is checked to be empty or
containing a branch (see note [Branch follows branch]).

When an CmmAGraph open at exit is being converted to a CmmGraph, the output
exit sequence is considered unreachable. If the graph consist of one block
only, if it not the case and we crash. Otherwise we just throw the exit
sequence away (and in debug mode we test that it really was unreachable).
-}

{-
Node [Branch follows branch]
============================
Why do we say it's ok for a Branch to follow a Branch?
Because the standard constructor mkLabel has fall-through
semantics. So if you do a mkLabel, you finish the current block,
giving it a label, and start a new one that branches to that label.
Emitting a Branch at this point is fine:
       goto L1; L2: ...stuff...
-}

data CmmGraphOC = Opened (Graph CmmNode O O)
                | Closed (Graph CmmNode O C)
type CmmAGraph = UniqSM CmmGraphOC     -- Graph open at entry

{-
MS: I began with
  newtype CmmAGraph = forall x. AG (UniqSM (Graph CmmNode O x))
but that does not work well, because we cannot take the graph
out of the monad -- we do not know the type of what we would take
out and pattern matching does not help, as we cannot pattern match
on a graph inside the monad.
-}

data Transfer = Call | Jump | Ret deriving Eq

---------- AGraph manipulation

emptyAGraph    :: CmmAGraph
(<*>)          :: CmmAGraph -> CmmAGraph -> CmmAGraph
catAGraphs     :: [CmmAGraph] -> CmmAGraph

mkLabel        :: BlockId     -> CmmAGraph  -- created a sequence "goto id; id:" as an AGraph
mkMiddle       :: CmmNode O O -> CmmAGraph  -- creates an open AGraph from a given node
mkLast         :: CmmNode O C -> CmmAGraph  -- created a closed AGraph from a given node

withFreshLabel :: String -> (BlockId -> CmmAGraph) -> CmmAGraph
withUnique     :: (Unique -> CmmAGraph) -> CmmAGraph

lgraphOfAGraph :: CmmAGraph -> UniqSM CmmGraph
  -- ^ allocate a fresh label for the entry point
labelAGraph    :: BlockId -> CmmAGraph -> UniqSM CmmGraph
  -- ^ use the given BlockId as the label of the entry point

---------- No-ops
mkNop        :: CmmAGraph
mkComment    :: FastString -> CmmAGraph

---------- Assignment and store
mkAssign     :: CmmReg  -> CmmExpr -> CmmAGraph
mkStore      :: CmmExpr -> CmmExpr -> CmmAGraph

---------- Calls
mkCall       :: CmmExpr -> (Convention, Convention) -> [CmmFormal] -> [CmmActual] ->
                  UpdFrameOffset -> CmmAGraph
mkCmmCall    :: CmmExpr ->              [CmmFormal] -> [CmmActual] ->
                  UpdFrameOffset -> CmmAGraph
  -- Native C-- calling convention
mkSafeCall    :: ForeignTarget -> [CmmFormal] -> [CmmActual] -> UpdFrameOffset -> Bool -> CmmAGraph
mkUnsafeCall  :: ForeignTarget -> [CmmFormal] -> [CmmActual] -> CmmAGraph
mkFinalCall   :: CmmExpr -> CCallConv -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
  -- Never returns; like exit() or barf()

---------- Control transfer
mkJump          ::               CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkDirectJump    ::               CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkJumpGC        ::               CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkForeignJump   :: Convention -> CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkCbranch       :: CmmExpr -> BlockId -> BlockId          -> CmmAGraph
mkSwitch        :: CmmExpr -> [Maybe BlockId]             -> CmmAGraph
mkReturn        :: CmmExpr -> [CmmActual] -> UpdFrameOffset -> CmmAGraph
mkReturnSimple  :: [CmmActual] -> UpdFrameOffset -> CmmAGraph

mkBranch        :: BlockId -> CmmAGraph
mkCmmIfThenElse :: CmmExpr -> CmmAGraph -> CmmAGraph -> CmmAGraph
mkCmmIfThen     :: CmmExpr -> CmmAGraph -> CmmAGraph
mkCmmWhileDo    :: CmmExpr -> CmmAGraph -> CmmAGraph

outOfLine       :: CmmAGraph -> CmmAGraph
-- ^ The argument is an CmmAGraph that must have an
-- empty entry sequence and be closed at the end.
-- The result is a new CmmAGraph that is open at the
-- end and goes directly from entry to exit, with the
-- original graph sitting to the side out-of-line.
--
-- Example:  mkMiddle (x = 3)
--           <*> outOfLine (mkLabel L <*> ...stuff...)
--           <*> mkMiddle (y = x)
-- Control will flow directly from x=3 to y=x;
-- the block starting with L is "on the side".
--
-- N.B. algebraically forall g g' : g <*> outOfLine g' == outOfLine g' <*> g

--------------------------------------------------------------------------

-- ================ IMPLEMENTATION ================--

--------------------------------------------------
-- Raw CmmAGraph handling

emptyAGraph = return $ Opened emptyGraph
ag <*> ah = do g <- ag
               h <- ah
               return (case (g, h) of
                 (Opened g, Opened h) -> Opened $ g H.<*> h
                 (Opened g, Closed h) -> Closed $ g H.<*> h
                 (Closed g, Opened GNil) -> Closed g
                 (Closed g, Opened (GUnit e)) -> note_unreachable e $ Closed g
                 (Closed g, Opened (GMany (JustO e) b x)) -> note_unreachable e $ Opened $ g H.|*><*| GMany NothingO b x
                 (Closed g, Closed (GMany (JustO e) b x)) -> note_unreachable e $ Closed $ g H.|*><*| GMany NothingO b x
                 :: CmmGraphOC)
catAGraphs = foldl (<*>) emptyAGraph

outOfLine ag = withFreshLabel "outOfLine" $ \l ->
               do g <- ag
                  return (case g of
                    Closed (GMany (JustO e) b _) -> note_unreachable e $ Opened $
                                                      GMany (JustO $ BLast $ CmmBranch l) b (JustO $ BFirst $ CmmEntry l)
                    _                            -> panic "outOfLine"
                    :: CmmGraphOC)

note_unreachable :: Block CmmNode O x -> a -> a
note_unreachable block graph =
  ASSERT (block_is_empty_or_label)  -- Note [Branch follows branch]
  graph
  where block_is_empty_or_label :: Bool
        block_is_empty_or_label = case blockToNodeList block of
                                    (NothingC, [], NothingC)            -> True
                                    (NothingC, [], JustC (CmmBranch _)) -> True
                                    _                                   -> False

mkLabel bid = return $ Opened $ H.mkLast (CmmBranch bid) |*><*| H.mkFirst (CmmEntry bid)
mkMiddle middle = return $ Opened $ H.mkMiddle middle
mkLast last = return $ Closed $ H.mkLast last

withUnique f = getUniqueM >>= f
withFreshLabel _name f = getUniqueM >>= f . uniqueToLbl . intToUnique . getKey

lgraphOfAGraph g = do u <- getUniqueM
                      labelAGraph (mkBlockId u) g

labelAGraph lbl ag = do g <- ag
                        return $ CmmGraph {g_entry=lbl, g_graph=H.mkFirst (CmmEntry lbl) H.<*> closed g}
  where closed :: CmmGraphOC -> Graph CmmNode O C
        closed (Closed g) = g
        closed (Opened g@(GMany entry body (JustO exit))) =
          ASSERT (entryLabel exit `notElem` map entryLabel (postorder_dfs g))
          GMany entry body NothingO
        closed (Opened _) = panic "labelAGraph"

--------------------------------------------------
-- CmmAGraph constructions

mkNop                     = emptyAGraph
mkComment fs              = mkMiddle $ CmmComment fs
mkStore  l r              = mkMiddle $ CmmStore  l r

-- NEED A COMPILER-DEBUGGING FLAG HERE
-- Sanity check: any value assigned to a pointer must be non-zero.
-- If it's 0, cause a crash immediately.
mkAssign l r = if opt_StubDeadValues then assign l r <*> check l else assign l r
  where assign l r = mkMiddle (CmmAssign l r)
        check (CmmGlobal _) = mkNop
        check l@(CmmLocal reg) = -- if a ptr arg is NULL, cause a crash!
          if isGcPtrType ty then
            mkCmmIfThen (CmmMachOp (MO_Eq w) [r, stackStubExpr w])
                        (assign l (CmmLoad (CmmLit (CmmInt 0 w)) ty))
          else mkNop
            where ty = localRegType reg
                  w  = typeWidth ty
                  r  = CmmReg l


-- Why are we inserting extra blocks that simply branch to the successors?
-- Because in addition to the branch instruction, @mkBranch@ will insert
-- a necessary adjustment to the stack pointer.
mkCbranch pred ifso ifnot = mkLast (CmmCondBranch pred ifso ifnot)
mkSwitch e tbl            = mkLast $ CmmSwitch e tbl

mkSafeCall   t fs as upd i = withFreshLabel "safe call" $ body
  where
    body k =
     (    mkStore (CmmStackSlot (CallArea (Young k)) (widthInBytes wordWidth))
                  (CmmLit (CmmBlock k))
      <*> mkLast (CmmForeignCall {tgt=t, res=fs, args=as, succ=k, updfr=upd, intrbl=i})
      <*> mkLabel k)
mkUnsafeCall t fs as = mkMiddle $ CmmUnsafeForeignCall t fs as

mkBranch bid = mkLast (CmmBranch bid)

mkCmmIfThenElse e tbranch fbranch =
  withFreshLabel "end of if"     $ \endif ->
  withFreshLabel "start of then" $ \tid ->
  withFreshLabel "start of else" $ \fid ->
    mkCbranch e tid fid <*>
    mkLabel tid <*> tbranch <*> mkBranch endif <*>
    mkLabel fid <*> fbranch <*> mkLabel endif

mkCmmIfThen e tbranch
  = withFreshLabel "end of if"     $ \endif ->
    withFreshLabel "start of then" $ \tid ->
      mkCbranch e tid endif <*>
      mkLabel tid <*> tbranch <*> mkLabel endif

mkCmmWhileDo e body =
  withFreshLabel "loop test" $ \test ->
  withFreshLabel "loop head" $ \head ->
  withFreshLabel "end while" $ \endwhile ->
    -- Forrest Baskett's while-loop layout
    mkBranch test <*> mkLabel head <*> body
                  <*> mkLabel test <*> mkCbranch e head endwhile
                  <*> mkLabel endwhile

-- For debugging purposes, we can stub out dead stack slots:
stackStubExpr :: Width -> CmmExpr
stackStubExpr w = CmmLit (CmmInt 0 w)

-- When we copy in parameters, we usually want to put overflow
-- parameters on the stack, but sometimes we want to pass
-- the variables in their spill slots.
-- Therefore, for copying arguments and results, we provide different
-- functions to pass the arguments in an overflow area and to pass them in spill slots.
copyInOflow  :: Convention -> Area -> [CmmFormal] -> (Int, CmmAGraph)
copyInSlot   :: Convention -> [CmmFormal] -> [CmmNode O O]
copyOutSlot  :: Convention -> [LocalReg] -> [CmmNode O O]

copyInOflow conv area formals = (offset, catAGraphs $ map mkMiddle nodes)
  where (offset, nodes) = copyIn oneCopyOflowI conv area formals
copyInSlot c f = snd $ copyIn oneCopySlotI c (panic "no area for copying to slots") f

type SlotCopier = Area -> (LocalReg, ByteOff) -> (ByteOff, [CmmNode O O]) ->
                          (ByteOff, [CmmNode O O])
type CopyIn  = SlotCopier -> Convention -> Area -> [CmmFormal] -> (ByteOff, [CmmNode O O])

-- Return the number of bytes used for copying arguments, as well as the
-- instructions to copy the arguments.
copyIn :: CopyIn
copyIn oflow conv area formals =
  foldr ci (init_offset, []) args'
  where ci (reg, RegisterParam r) (n, ms) =
          (n, CmmAssign (CmmLocal reg) (CmmReg $ CmmGlobal r) : ms)
        ci (r, StackParam off) (n, ms) = oflow area (r, off) (n, ms)
        init_offset = widthInBytes wordWidth -- infotable
        args  = assignArgumentsPos conv localRegType formals
        args' = foldl adjust [] args
          where adjust rst (v, StackParam off) = (v, StackParam (off + init_offset)) : rst
                adjust rst x@(_, RegisterParam _) = x : rst

-- Copy-in one arg, using overflow space if needed.
oneCopyOflowI, oneCopySlotI :: SlotCopier
oneCopyOflowI area (reg, off) (n, ms) =
  (max n off, CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot area off) ty) : ms)
  where ty = localRegType reg

-- Copy-in one arg, using spill slots if needed -- used for calling conventions at
-- a procpoint that is not a return point. The offset is irrelevant here...
oneCopySlotI _ (reg, _) (n, ms) =
  (n, CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg) w) ty) : ms)
  where ty = localRegType reg
        w  = widthInBytes (typeWidth ty)


-- Factoring out the common parts of the copyout functions yielded something
-- more complicated:

copyOutOflow :: Convention -> Transfer -> Area -> [CmmActual] -> UpdFrameOffset ->
                              (Int, CmmAGraph)
-- Generate code to move the actual parameters into the locations
-- required by the calling convention.  This includes a store for the return address.
--
-- The argument layout function ignores the pointer to the info table, so we slot that
-- in here. When copying-out to a young area, we set the info table for return
-- and adjust the offsets of the other parameters.
-- If this is a call instruction, we adjust the offsets of the other parameters.
copyOutOflow conv transfer area@(CallArea a) actuals updfr_off
  = foldr co (init_offset, emptyAGraph) args'
  where 
    co (v, RegisterParam r) (n, ms) = (n, mkAssign (CmmGlobal r) v <*> ms)
    co (v, StackParam off)  (n, ms) = (max n off, mkStore (CmmStackSlot area off) v <*> ms)

    (setRA, init_offset) =
      case a of Young id -> id `seq` -- Generate a store instruction for
                                     -- the return address if making a call
                  if transfer == Call then
                    ([(CmmLit (CmmBlock id), StackParam init_offset)],
                     widthInBytes wordWidth)
                  else ([], 0)
                Old -> ([], updfr_off)

    args :: [(CmmExpr, ParamLocation)]   -- The argument and where to put it
    args = assignArgumentsPos conv cmmExprType actuals

    args' = foldl adjust setRA args
      where adjust rst   (v, StackParam off)  = (v, StackParam (off + init_offset)) : rst
            adjust rst x@(_, RegisterParam _) = x : rst

copyOutOflow _ _ (RegSlot _) _ _ = panic "cannot copy arguments into a register slot"

-- Args passed only in registers and stack slots; no overflow space.
-- No return address may apply!
copyOutSlot conv actuals = foldr co [] args
  where co (v, RegisterParam r) ms = CmmAssign (CmmGlobal r) (toExp v) : ms
        co (v, StackParam off)  ms = CmmStore  (CmmStackSlot (RegSlot v) off) (toExp v) : ms
        toExp r = CmmReg (CmmLocal r)
        args = assignArgumentsPos conv localRegType actuals

mkCallEntry :: Convention -> [CmmFormal] -> (Int, CmmAGraph)
mkCallEntry conv formals = copyInOflow conv (CallArea Old) formals

lastWithArgs :: Transfer -> Area -> Convention -> [CmmActual] -> UpdFrameOffset ->
                (ByteOff -> CmmAGraph) -> CmmAGraph
lastWithArgs transfer area conv actuals updfr_off last =
  let (outArgs, copies) = copyOutOflow conv transfer area actuals updfr_off in
  copies <*> last outArgs

-- The area created for the jump and return arguments is the same area as the
-- procedure entry.
old :: Area
old = CallArea Old
toCall :: CmmExpr -> Maybe BlockId -> UpdFrameOffset -> ByteOff -> ByteOff -> CmmAGraph
toCall e cont updfr_off res_space arg_space =
  mkLast $ CmmCall e cont arg_space res_space updfr_off
mkJump e actuals updfr_off =
  lastWithArgs Jump old NativeNodeCall actuals updfr_off $ toCall e Nothing updfr_off 0
mkDirectJump e actuals updfr_off =
  lastWithArgs Jump old NativeDirectCall actuals updfr_off $ toCall e Nothing updfr_off 0
mkJumpGC e actuals updfr_off =
  lastWithArgs Jump old GC actuals updfr_off $ toCall e Nothing updfr_off 0
mkForeignJump conv e actuals updfr_off =
  lastWithArgs Jump old conv actuals updfr_off $ toCall e Nothing updfr_off 0
mkReturn e actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $ toCall e Nothing updfr_off 0
    -- where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord
mkReturnSimple actuals updfr_off =
  lastWithArgs Ret  old NativeReturn actuals updfr_off $ toCall e Nothing updfr_off 0
    where e = CmmLoad (CmmStackSlot (CallArea Old) updfr_off) gcWord

mkFinalCall f _ actuals updfr_off =
  lastWithArgs Call old NativeDirectCall actuals updfr_off $ toCall f Nothing updfr_off 0

mkCmmCall f results actuals = mkCall f (NativeDirectCall, NativeReturn) results actuals

-- I'm dropping the SRT, but that should be okay: we plan to reconstruct it later.
mkCall f (callConv, retConv) results actuals updfr_off =
  withFreshLabel "call successor" $ \k ->
    let area = CallArea $ Young k
        (off, copyin) = copyInOflow retConv area results
        copyout = lastWithArgs Call area callConv actuals updfr_off 
                               (toCall f (Just k) updfr_off off)
    in (copyout <*> mkLabel k <*> copyin)
