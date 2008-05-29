

-- This module is pure representation and should be imported only by
-- clients that need to manipulate representation and know what
-- they're doing.  Clients that need to create flow graphs should
-- instead import MkZipCfgCmm.

module ZipCfgCmmRep
  ( CmmZ, CmmTopZ, CmmGraph, CmmBlock, CmmAGraph, Middle(..), Last(..), Convention(..)
  , ValueDirection(..), CmmBackwardFixedPoint, CmmForwardFixedPoint
  , insertBetween, pprCmmGraphLikeCmm
  )
where

import BlockId
import CmmExpr
import Cmm ( GenCmm(..), GenCmmTop(..), CmmStatic, CmmInfo
           , CmmCallTarget(..), CmmActuals, CmmFormals, CmmKinded(..)
           , CmmStmt(..) -- imported in order to call ppr on Switch and to
                         -- implement pprCmmGraphLikeCmm
           , CmmSafety(CmmSafe) -- for pprCmmGraphLikeCmm
           , CmmReturnInfo(CmmMayReturn) -- for pprCmmGraphLikeCmm
           )
import PprCmm()

import CLabel
import CmmZipUtil
import ClosureInfo
import FastString
import ForeignCall
import MachOp
import qualified ZipCfg as Z
import qualified ZipDataflow as DF
import ZipCfg 
import MkZipCfg
import Util

import Maybes
import Monad
import Outputable
import Prelude hiding (zip, unzip, last)
import UniqSet
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
                                 -- given by cmmExprRep of the rhs.

  | MidUnsafeCall                -- An "unsafe" foreign call;
     CmmCallTarget               -- just a fat machine instruction
     CmmFormals                  -- zero or more results
     CmmActuals                  -- zero or more arguments

  | MidAddToContext              -- push a frame on the stack;
                                 -- I will return to this frame
     CmmExpr                     -- The frame's return address; it must be
                                 -- preceded by an info table that describes the
                                 -- live variables.
     [CmmExpr]                   -- The frame's live variables, to go on the 
                                 -- stack with the first one at the young end

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
  deriving Eq

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
-- o For a jump, or return, this operation is impossible.

insertBetween :: MonadUnique m => CmmBlock -> [Middle] -> BlockId -> m (CmmBlock, [CmmBlock])
insertBetween b ms succId = insert $ goto_end $ unzip b
  where insert (h, LastOther (LastBranch bid)) =
          if bid == succId then
            do (bid', bs) <- newBlocks
               return (zipht h $ ZLast $ LastOther (LastBranch bid'), bs)
          else panic "tried to insert between non-adjacent blocks"
        insert (h, LastOther (LastCondBranch c t f)) =
          do (t', tbs) <- if t == succId then newBlocks else return $ (t, [])
             (f', fbs) <- if f == succId then newBlocks else return $ (f, [])
             return (zipht h $ ZLast $ LastOther (LastCondBranch c t' f'), tbs ++ fbs)
        insert (h, LastOther (LastCall e (Just k))) =
          if k == succId then
            do (id', bs) <- newBlocks
               return (zipht h $ ZLast $ LastOther (LastCall e (Just id')), bs)
          else panic "tried to insert between non-adjacent blocks"
        insert (_, LastOther (LastCall _ Nothing)) =
          panic "cannot insert after non-returning call"
        insert (h, LastOther (LastSwitch e ks)) =
          do (ids, bs) <- mapAndUnzipM mbNewBlocks ks
             return (zipht h $ ZLast $ LastOther (LastSwitch e ids), join bs)
        insert (_, LastOther LastReturn)   = panic "cannot insert after return"
        insert (_, LastOther (LastJump _)) = panic "cannot insert after jump"
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
            middle (MidAssign _lhs expr)          = fold f z expr
            middle (MidStore addr rval)           = fold f (fold f z addr) rval
            middle (MidUnsafeCall tgt _ress args) = fold f (fold f z tgt) args
            middle (MidAddToContext ra args)      = fold f (fold f z ra) args
            middle (CopyIn _ _formals _)          = z
            middle (CopyOut _ actuals)            = fold f z actuals
            fold f z m = foldRegsUsed f z m  -- avoid monomorphism restriction

instance UserOfLocalRegs Last where
    foldRegsUsed f z l = last l
      where last (LastReturn)           = z
            last (LastJump e)           = foldRegsUsed f z e
            last (LastBranch _id)       = z
            last (LastCall tgt _)       = foldRegsUsed f z tgt
            last (LastCondBranch e _ _) = foldRegsUsed f z e
            last (LastSwitch e _tbl)    = foldRegsUsed f z e

instance DefinerOfLocalRegs Middle where
    foldRegsDefd f z m = middle m
      where middle (MidComment {})       = z
            middle (MidAssign _lhs _)    = fold f z _lhs
            middle (MidStore _ _)        = z
            middle (MidUnsafeCall _ _ _) = z
            middle (MidAddToContext _ _) = z
            middle (CopyIn _ _formals _) = fold f z _formals
            middle (CopyOut _ _)         = z
            fold f z m = foldRegsDefd f z m  -- avoid monomorphism restriction

instance DefinerOfLocalRegs Last where
    foldRegsDefd _ z l = last l
      where last (LastReturn)           = z
            last (LastJump _)           = z
            last (LastBranch _)         = z
            last (LastCall _ _)         = z
            last (LastCondBranch _ _ _) = z
            last (LastSwitch _ _)       = z

----------------------------------------------------------------------
----- Instance declarations for prettyprinting (avoids recursive imports)

instance Outputable Middle where
    ppr s = pprMiddle s

instance Outputable Last where
    ppr s = pprLast s

instance Outputable Convention where
    ppr = pprConvention

instance DF.DebugNodes Middle Last

debugPpr :: Bool
debugPpr = debugIsOn

pprMiddle :: Middle -> SDoc    
pprMiddle stmt = pp_stmt <+> pp_debug
 where
   pp_stmt = case stmt of

    CopyIn conv args _ ->
        if null args then ptext (sLit "empty CopyIn")
        else commafy (map pprKinded args) <+> equals <+>
             ptext (sLit "foreign") <+> doubleQuotes(ppr conv) <+> ptext (sLit "...")

    CopyOut conv args ->
        ptext (sLit "PreCopyOut: next, pass") <+> doubleQuotes(ppr conv) <+>
        parens (commafy (map pprKinded args))

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
                       ptext (sLit " = "),
               ptext (sLit "call"), space, 
               doubleQuotes(ppr cconv), space,
               ppr_target fn, parens  ( commafy $ map ppr args ),
               semi ]

    MidUnsafeCall (CmmPrim op) results args ->
        pprMiddle (MidUnsafeCall (CmmCallee (CmmLit lbl) CCallConv) results args)
        where
          lbl = CmmLabel (mkForeignLabel (mkFastString (show op)) Nothing False)

    MidAddToContext ra args ->
        hcat [ ptext (sLit "return via ")
             , ppr_target ra, parens (commafy $ map ppr args), semi ]

   pp_debug =
     if not debugPpr then empty
     else text " //" <+>
          case stmt of
            CopyIn {}     -> text "CopyIn"
            CopyOut {}    -> text "CopyOut"
            MidComment {} -> text "MidComment"
            MidAssign {}  -> text "MidAssign"
            MidStore {}   -> text "MidStore"
            MidUnsafeCall  {} -> text "MidUnsafeCall"
            MidAddToContext {} -> text "MidAddToContext"


ppr_target :: CmmExpr -> SDoc
ppr_target t@(CmmLit _) = ppr t
ppr_target fn'          = parens (ppr fn')


pprKinded :: Outputable a => CmmKinded a -> SDoc
pprKinded (CmmKinded a NoHint)     = ppr a
pprKinded (CmmKinded a PtrHint)    = doubleQuotes (text "address") <+> ppr a
pprKinded (CmmKinded a SignedHint) = doubleQuotes (text "signed")  <+> ppr a
pprKinded (CmmKinded a FloatHint)  = doubleQuotes (text "float")   <+> ppr a

pprLast :: Last -> SDoc    
pprLast stmt = (case stmt of
    LastBranch ident          -> ptext (sLit "goto") <+> ppr ident <> semi
    LastCondBranch expr t f   -> genFullCondBranch expr t f
    LastJump expr             -> hcat [ ptext (sLit "jump"), space, pprFun expr
                                      , ptext (sLit "(...)"), semi]
    LastReturn                -> hcat [ ptext (sLit "return"), space 
                                      , ptext (sLit "(...)"), semi]
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
pprConvention (ConventionStandard c _) = ppr c
pprConvention (ConventionPrivate {}  ) = text "<private-convention>"

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs


----------------------------------------------------------------
-- | The purpose of this function is to print a Cmm zipper graph "as if it were"
-- a Cmm program.  The objective is dodgy, so it's unsurprising parts of the
-- code are dodgy as well.

pprCmmGraphLikeCmm :: CmmGraph -> SDoc
pprCmmGraphLikeCmm g = vcat (swallow blocks)
    where blocks = Z.postorder_dfs g
          swallow :: [CmmBlock] -> [SDoc]
          swallow [] = []
          swallow (Z.Block id t : rest) = tail id [] Nothing t rest
          tail id prev' out (Z.ZTail (CopyOut conv args) t) rest =
              if isJust out then panic "multiple CopyOut nodes in one basic block"
              else
                  tail id (prev') (Just (conv, args)) t rest
          tail id prev' out (Z.ZTail m t) rest = tail id (mid m : prev') out t rest
          tail id prev' out (Z.ZLast Z.LastExit)      rest = exit id prev' out rest
          tail id prev' out (Z.ZLast (Z.LastOther l)) rest = last id prev' out l rest
          mid (CopyIn _ [] _) = text "// proc point (no parameters)"
          mid m@(CopyIn {}) = ppr m <+> text "(proc point)"
          mid m = ppr m
          block' id prev'
              | id == Z.lg_entry g, entry_has_no_pred =
                            vcat (text "<entry>" : reverse prev')
              | otherwise = hang (ppr id <> colon) 4 (vcat (reverse prev'))
          last id prev' out l n =
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case l of
                LastBranch tgt ->
                    case n of
                      Z.Block id' t : bs
                          | tgt == id', unique_pred id' 
                          -> tail id prev' out t bs  -- optimize out redundant labels
                      _ -> endblock (ppr $ CmmBranch tgt)
                l@(LastCondBranch expr tid fid) ->
                  let ft id = text "// fall through to " <> ppr id in
                  case n of
                    Z.Block id' t : bs
                      | id' == fid, isNothing out ->
                          tail id (ft fid : ppr (CmmCondBranch expr tid) : prev') Nothing t bs
                      | id' == tid, Just e' <- maybeInvertCmmExpr expr, isNothing out->
                          tail id (ft tid : ppr (CmmCondBranch e'   fid) : prev') Nothing t bs
                    _ -> endblock $ with_out out l
                l@(LastJump   {}) -> endblock $ with_out out l
                l@(LastReturn {}) -> endblock $ with_out out l
                l@(LastSwitch {}) -> endblock $ with_out out l
                l@(LastCall _ Nothing) -> endblock $ with_out out l
                l@(LastCall tgt (Just k))
                   | Z.Block id' (Z.ZTail (CopyIn _ ress srt) t) : bs <- n,
                     Just (conv, args) <- out,
                     id' == k ->
                         let call = CmmCall tgt' ress args (CmmSafe srt) CmmMayReturn
                             tgt' = CmmCallee tgt (cconv_of_conv conv)
                             ppcall = ppr call <+> parens (text "ret to" <+> ppr k)
                         in if unique_pred k then
                                tail id (ppcall : prev') Nothing t bs
                            else
                                endblock (ppcall)
                   | Z.Block id' t : bs <- n, id' == k, unique_pred k,
                     Just (conv, args) <- out,
                     Just (ress, srt) <- findCopyIn t ->
                         let call = CmmCall tgt' ress args (CmmSafe srt) CmmMayReturn
                             tgt' = CmmCallee tgt (cconv_of_conv conv)
                             delayed =
                                 ptext (sLit "// delayed CopyIn follows previous call")
                         in  tail id (delayed : ppr call : prev') Nothing t bs
                   | otherwise -> endblock $ with_out out l
          findCopyIn (Z.ZTail (CopyIn _ ress srt) _) = Just (ress, srt)
          findCopyIn (Z.ZTail _ t) = findCopyIn t
          findCopyIn (Z.ZLast _) = Nothing
          exit id prev' out n = -- highly irregular (assertion violation?)
              let endblock stmt = block' id (stmt : prev') : swallow n in
              case out of Nothing -> endblock (text "// <exit>")
                          Just (conv, args) -> endblock (ppr (CopyOut conv args) $$
                                                         text "// <exit>")
          preds = zipPreds g
          entry_has_no_pred = case lookupBlockEnv preds (Z.lg_entry g) of
                                Nothing -> True
                                Just s -> isEmptyUniqSet s
          single_preds =
              let add b single =
                    let id = Z.blockId b
                    in  case lookupBlockEnv preds id of
                          Nothing -> single
                          Just s -> if sizeUniqSet s == 1 then
                                        extendBlockSet single id
                                    else single
              in  Z.fold_blocks add emptyBlockSet g
          unique_pred id = elemBlockSet id single_preds
          cconv_of_conv (ConventionStandard conv _) = conv
          cconv_of_conv (ConventionPrivate {}) = CmmCallConv -- XXX totally bogus

with_out :: Maybe (Convention, CmmActuals) -> Last -> SDoc
with_out Nothing l = ptext (sLit "??no-arguments??") <+> ppr l
with_out (Just (conv, args)) l = last l
    where last (LastCall e k) =
              hcat [ptext (sLit "... = foreign "),
                    doubleQuotes(ppr conv), space,
                    ppr_target e, parens ( commafy $ map ppr args ),
                    ptext (sLit " \"safe\""),
                    case k of Nothing -> ptext (sLit " never returns")
                              Just _ -> empty,
                    semi ]
          last (LastReturn) = ppr (CmmReturn args)
          last (LastJump e) = ppr (CmmJump e args)
          last l = ppr (CopyOut conv args) $$ ppr l
          ppr_target (CmmLit lit) = ppr lit
          ppr_target fn'          = parens (ppr fn')
          commafy xs = hsep $ punctuate comma xs
