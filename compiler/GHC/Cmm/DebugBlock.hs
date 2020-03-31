{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- Debugging data
--
-- Association of debug data on the Cmm level, with methods to encode it in
-- event log format for later inclusion in profiling event logs.
--
-----------------------------------------------------------------------------

module GHC.Cmm.DebugBlock (

  DebugBlock(..),
  cmmDebugGen,
  cmmDebugLabels,
  cmmDebugLink,
  debugToMap,

  -- * Unwinding information
  UnwindTable, UnwindPoint(..),
  UnwindExpr(..), toUnwindExpr
  ) where

import GhcPrelude

import GHC.Platform
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Core
import FastString      ( nilFS, mkFastString )
import GHC.Types.Module
import Outputable
import GHC.Cmm.Ppr.Expr ( pprExpr )
import GHC.Types.SrcLoc
import Util            ( seqList )

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

import Data.Maybe
import Data.List     ( minimumBy, nubBy )
import Data.Ord      ( comparing )
import qualified Data.Map as Map
import Data.Either   ( partitionEithers )

-- | Debug information about a block of code. Ticks scope over nested
-- blocks.
data DebugBlock =
  DebugBlock
  { dblProcedure  :: !Label        -- ^ Entry label of containing proc
  , dblLabel      :: !Label        -- ^ Hoopl label
  , dblCLabel     :: !CLabel       -- ^ Output label
  , dblHasInfoTbl :: !Bool         -- ^ Has an info table?
  , dblParent     :: !(Maybe DebugBlock)
    -- ^ The parent of this proc. See Note [Splitting DebugBlocks]
  , dblTicks      :: ![CmmTickish] -- ^ Ticks defined in this block
  , dblSourceTick :: !(Maybe CmmTickish) -- ^ Best source tick covering block
  , dblPosition   :: !(Maybe Int)  -- ^ Output position relative to
                                   -- other blocks. @Nothing@ means
                                   -- the block was optimized out
  , dblUnwind     :: [UnwindPoint]
  , dblBlocks     :: ![DebugBlock] -- ^ Nested blocks
  }

instance Outputable DebugBlock where
  ppr blk = (if | dblProcedure blk == dblLabel blk
                -> text "proc"
                | dblHasInfoTbl blk
                -> text "pp-blk"
                | otherwise
                -> text "blk") <+>
            ppr (dblLabel blk) <+> parens (ppr (dblCLabel blk)) <+>
            (maybe empty ppr (dblSourceTick blk)) <+>
            (maybe (text "removed") ((text "pos " <>) . ppr)
                   (dblPosition blk)) <+>
            (ppr (dblUnwind blk)) $+$
            (if null (dblBlocks blk) then empty else nest 4 (ppr (dblBlocks blk)))

-- | Intermediate data structure holding debug-relevant context information
-- about a block.
type BlockContext = (CmmBlock, RawCmmDecl)

-- | Extract debug data from a group of procedures. We will prefer
-- source notes that come from the given module (presumably the module
-- that we are currently compiling).
cmmDebugGen :: ModLocation -> RawCmmGroup -> [DebugBlock]
cmmDebugGen modLoc decls = map (blocksForScope Nothing) topScopes
  where
      blockCtxs :: Map.Map CmmTickScope [BlockContext]
      blockCtxs = blockContexts decls

      -- Analyse tick scope structure: Each one is either a top-level
      -- tick scope, or the child of another.
      (topScopes, childScopes)
        = partitionEithers $ map (\a -> findP a a) $ Map.keys blockCtxs
      findP tsc GlobalScope = Left tsc -- top scope
      findP tsc scp | scp' `Map.member` blockCtxs = Right (scp', tsc)
                    | otherwise                   = findP tsc scp'
        where -- Note that we only following the left parent of
              -- combined scopes. This loses us ticks, which we will
              -- recover by copying ticks below.
              scp' | SubScope _ scp' <- scp      = scp'
                   | CombinedScope scp' _ <- scp = scp'
                   | otherwise                   = panic "findP impossible"

      scopeMap = foldr (uncurry insertMulti) Map.empty childScopes

      -- This allows us to recover ticks that we lost by flattening
      -- the graph. Basically, if the parent is A but the child is
      -- CBA, we know that there is no BA, because it would have taken
      -- priority - but there might be a B scope, with ticks that
      -- would not be associated with our child anymore. Note however
      -- that there might be other childs (DB), which we have to
      -- filter out.
      --
      -- We expect this to be called rarely, which is why we are not
      -- trying too hard to be efficient here. In many cases we won't
      -- have to construct blockCtxsU in the first place.
      ticksToCopy :: CmmTickScope -> [CmmTickish]
      ticksToCopy (CombinedScope scp s) = go s
        where go s | scp `isTickSubScope` s   = [] -- done
                   | SubScope _ s' <- s       = ticks ++ go s'
                   | CombinedScope s1 s2 <- s = ticks ++ go s1 ++ go s2
                   | otherwise                = panic "ticksToCopy impossible"
                where ticks = bCtxsTicks $ fromMaybe [] $ Map.lookup s blockCtxs
      ticksToCopy _ = []
      bCtxsTicks = concatMap (blockTicks . fst)

      -- Finding the "best" source tick is somewhat arbitrary -- we
      -- select the first source span, while preferring source ticks
      -- from the same source file.  Furthermore, dumps take priority
      -- (if we generated one, we probably want debug information to
      -- refer to it).
      bestSrcTick = minimumBy (comparing rangeRating)
      rangeRating (SourceNote span _)
        | srcSpanFile span == thisFile = 1
        | otherwise                    = 2 :: Int
      rangeRating note                 = pprPanic "rangeRating" (ppr note)
      thisFile = maybe nilFS mkFastString $ ml_hs_file modLoc

      -- Returns block tree for this scope as well as all nested
      -- scopes. Note that if there are multiple blocks in the (exact)
      -- same scope we elect one as the "branch" node and add the rest
      -- as children.
      blocksForScope :: Maybe CmmTickish -> CmmTickScope -> DebugBlock
      blocksForScope cstick scope = mkBlock True (head bctxs)
        where bctxs = fromJust $ Map.lookup scope blockCtxs
              nested = fromMaybe [] $ Map.lookup scope scopeMap
              childs = map (mkBlock False) (tail bctxs) ++
                       map (blocksForScope stick) nested

              mkBlock :: Bool -> BlockContext -> DebugBlock
              mkBlock top (block, prc)
                = DebugBlock { dblProcedure    = g_entry graph
                             , dblLabel        = label
                             , dblCLabel       = case info of
                                 Just (CmmStaticsRaw infoLbl _) -> infoLbl
                                 Nothing
                                   | g_entry graph == label -> entryLbl
                                   | otherwise              -> blockLbl label
                             , dblHasInfoTbl   = isJust info
                             , dblParent       = Nothing
                             , dblTicks        = ticks
                             , dblPosition     = Nothing -- see cmmDebugLink
                             , dblSourceTick   = stick
                             , dblBlocks       = blocks
                             , dblUnwind       = []
                             }
                where (CmmProc infos entryLbl _ graph) = prc
                      label = entryLabel block
                      info = mapLookup label infos
                      blocks | top       = seqList childs childs
                             | otherwise = []

              -- A source tick scopes over all nested blocks. However
              -- their source ticks might take priority.
              isSourceTick SourceNote {} = True
              isSourceTick _             = False
              -- Collect ticks from all blocks inside the tick scope.
              -- We attempt to filter out duplicates while we're at it.
              ticks = nubBy (flip tickishContains) $
                      bCtxsTicks bctxs ++ ticksToCopy scope
              stick = case filter isSourceTick ticks of
                []     -> cstick
                sticks -> Just $! bestSrcTick (sticks ++ maybeToList cstick)

-- | Build a map of blocks sorted by their tick scopes
--
-- This involves a pre-order traversal, as we want blocks in rough
-- control flow order (so ticks have a chance to be sorted in the
-- right order).
blockContexts :: RawCmmGroup -> Map.Map CmmTickScope [BlockContext]
blockContexts decls = Map.map reverse $ foldr walkProc Map.empty decls
  where walkProc :: RawCmmDecl
                 -> Map.Map CmmTickScope [BlockContext]
                 -> Map.Map CmmTickScope [BlockContext]
        walkProc CmmData{}                 m = m
        walkProc prc@(CmmProc _ _ _ graph) m
          | mapNull blocks = m
          | otherwise      = snd $ walkBlock prc entry (emptyLbls, m)
          where blocks = toBlockMap graph
                entry  = [mapFind (g_entry graph) blocks]
                emptyLbls = setEmpty :: LabelSet

        walkBlock :: RawCmmDecl -> [Block CmmNode C C]
                  -> (LabelSet, Map.Map CmmTickScope [BlockContext])
                  -> (LabelSet, Map.Map CmmTickScope [BlockContext])
        walkBlock _   []             c            = c
        walkBlock prc (block:blocks) (visited, m)
          | lbl `setMember` visited
          = walkBlock prc blocks (visited, m)
          | otherwise
          = walkBlock prc blocks $
            walkBlock prc succs
              (lbl `setInsert` visited,
               insertMulti scope (block, prc) m)
          where CmmEntry lbl scope = firstNode block
                (CmmProc _ _ _ graph) = prc
                succs = map (flip mapFind (toBlockMap graph))
                            (successors (lastNode block))
        mapFind = mapFindWithDefault (error "contextTree: block not found!")

insertMulti :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
insertMulti k v = Map.insertWith (const (v:)) k [v]

cmmDebugLabels :: (i -> Bool) -> GenCmmGroup d g (ListGraph i) -> [Label]
cmmDebugLabels isMeta nats = seqList lbls lbls
  where -- Find order in which procedures will be generated by the
        -- back-end (that actually matters for DWARF generation).
        --
        -- Note that we might encounter blocks that are missing or only
        -- consist of meta instructions -- we will declare them missing,
        -- which will skip debug data generation without messing up the
        -- block hierarchy.
        lbls = map blockId $ filter (not . allMeta) $ concatMap getBlocks nats
        getBlocks (CmmProc _ _ _ (ListGraph bs)) = bs
        getBlocks _other                         = []
        allMeta (BasicBlock _ instrs) = all isMeta instrs

-- | Sets position and unwind table fields in the debug block tree according to
-- native generated code.
cmmDebugLink :: [Label] -> LabelMap [UnwindPoint]
             -> [DebugBlock] -> [DebugBlock]
cmmDebugLink labels unwindPts blocks = map link blocks
  where blockPos :: LabelMap Int
        blockPos = mapFromList $ flip zip [0..] labels
        link block = block { dblPosition = mapLookup (dblLabel block) blockPos
                           , dblBlocks   = map link (dblBlocks block)
                           , dblUnwind   = fromMaybe mempty
                                         $ mapLookup (dblLabel block) unwindPts
                           }

-- | Converts debug blocks into a label map for easier lookups
debugToMap :: [DebugBlock] -> LabelMap DebugBlock
debugToMap = mapUnions . map go
   where go b = mapInsert (dblLabel b) b $ mapUnions $ map go (dblBlocks b)

{-
Note [What is this unwinding business?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unwinding tables are a variety of debugging information used by debugging tools
to reconstruct the execution history of a program at runtime. These tables
consist of sets of "instructions", one set for every instruction in the program,
which describe how to reconstruct the state of the machine at the point where
the current procedure was called. For instance, consider the following annotated
pseudo-code,

  a_fun:
    add rsp, 8            -- unwind: rsp = rsp - 8
    mov rax, 1            -- unwind: rax = unknown
    call another_block
    sub rsp, 8            -- unwind: rsp = rsp

We see that attached to each instruction there is an "unwind" annotation, which
provides a relationship between each updated register and its value at the
time of entry to a_fun. This is the sort of information that allows gdb to give
you a stack backtrace given the execution state of your program. This
unwinding information is captured in various ways by various debug information
formats; in the case of DWARF (the only format supported by GHC) it is known as
Call Frame Information (CFI) and can be found in the .debug.frames section of
your object files.

Currently we only bother to produce unwinding information for registers which
are necessary to reconstruct flow-of-execution. On x86_64 this includes $rbp
(which is the STG stack pointer) and $rsp (the C stack pointer).

Let's consider how GHC would annotate a C-- program with unwinding information
with a typical C-- procedure as would come from the STG-to-Cmm code generator,

  entry()
     { c2fe:
           v :: P64 = R2;
           if ((Sp + 8) - 32 < SpLim) (likely: False) goto c2ff; else goto c2fg;
       c2ff:
           R2 = v :: P64;
           R1 = test_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
       c2fg:
           I64[Sp - 8] = c2dD;
           R1 = v :: P64;
           Sp = Sp - 8;          // Sp updated here
           if (R1 & 7 != 0) goto c2dD; else goto c2dE;
       c2dE:
           call (I64[R1])(R1) returns to c2dD, args: 8, res: 8, upd: 8;
       c2dD:
           w :: P64 = R1;
           Hp = Hp + 48;
           if (Hp > HpLim) (likely: False) goto c2fj; else goto c2fi;
       ...
  },

Let's consider how this procedure will be decorated with unwind information
(largely by GHC.Cmm.LayoutStack). Naturally, when we enter the procedure `entry` the
value of Sp is no different from what it was at its call site. Therefore we will
add an `unwind` statement saying this at the beginning of its unwind-annotated
code,

  entry()
     { c2fe:
           unwind Sp = Just Sp + 0;
           v :: P64 = R2;
           if ((Sp + 8) - 32 < SpLim) (likely: False) goto c2ff; else goto c2fg;

After c2fe we may pass to either c2ff or c2fg; let's first consider the
former. In this case there is nothing in particular that we need to do other
than reiterate what we already know about Sp,

       c2ff:
           unwind Sp = Just Sp + 0;
           R2 = v :: P64;
           R1 = test_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;

In contrast, c2fg updates Sp midway through its body. To ensure that unwinding
can happen correctly after this point we must include an unwind statement there,
in addition to the usual beginning-of-block statement,

       c2fg:
           unwind Sp = Just Sp + 0;
           I64[Sp - 8] = c2dD;
           R1 = v :: P64;
           Sp = Sp - 8;
           unwind Sp = Just Sp + 8;
           if (R1 & 7 != 0) goto c2dD; else goto c2dE;

The remaining blocks are simple,

       c2dE:
           unwind Sp = Just Sp + 8;
           call (I64[R1])(R1) returns to c2dD, args: 8, res: 8, upd: 8;
       c2dD:
           unwind Sp = Just Sp + 8;
           w :: P64 = R1;
           Hp = Hp + 48;
           if (Hp > HpLim) (likely: False) goto c2fj; else goto c2fi;
       ...
  },


The flow of unwinding information through the compiler is a bit convoluted:

 * C-- begins life in StgToCmm without any unwind information. This is because we
   haven't actually done any register assignment or stack layout yet, so there
   is no need for unwind information.

 * GHC.Cmm.LayoutStack figures out how to layout each procedure's stack, and produces
   appropriate unwinding nodes for each adjustment of the STG Sp register.

 * The unwind nodes are carried through the sinking pass. Currently this is
   guaranteed not to invalidate unwind information since it won't touch stores
   to Sp, but this will need revisiting if CmmSink gets smarter in the future.

 * Eventually we make it to the native code generator backend which can then
   preserve the unwind nodes in its machine-specific instructions. In so doing
   the backend can also modify or add unwinding information; this is necessary,
   for instance, in the case of x86-64, where adjustment of $rsp may be
   necessary during calls to native foreign code due to the native calling
   convention.

 * The NCG then retrieves the final unwinding table for each block from the
   backend with extractUnwindPoints.

 * This unwind information is converted to DebugBlocks by Debug.cmmDebugGen

 * These DebugBlocks are then converted to, e.g., DWARF unwinding tables
   (by the Dwarf module) and emitted in the final object.

See also:
  Note [Unwinding information in the NCG] in AsmCodeGen,
  Note [Unwind pseudo-instruction in Cmm],
  Note [Debugging DWARF unwinding info].


Note [Debugging DWARF unwinding info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For debugging generated unwinding info I've found it most useful to dump the
disassembled binary with objdump -D and dump the debug info with
readelf --debug-dump=frames-interp.

You should get something like this:

  0000000000000010 <stg_catch_frame_info>:
    10:   48 83 c5 18             add    $0x18,%rbp
    14:   ff 65 00                jmpq   *0x0(%rbp)

and:

  Contents of the .debug_frame section:

  00000000 0000000000000014 ffffffff CIE "" cf=1 df=-8 ra=16
     LOC           CFA      rbp   rsp   ra
  0000000000000000 rbp+0    v+0   s     c+0

  00000018 0000000000000024 00000000 FDE cie=00000000 pc=000000000000000f..0000000000000017
     LOC           CFA      rbp   rsp   ra
  000000000000000f rbp+0    v+0   s     c+0
  000000000000000f rbp+24   v+0   s     c+0

To read it http://www.dwarfstd.org/doc/dwarf-2.0.0.pdf has a nice example in
Appendix 5 (page 101 of the pdf) and more details in the relevant section.

The key thing to keep in mind is that the value at LOC is the value from
*before* the instruction at LOC executes. In other words it answers the
question: if my $rip is at LOC, how do I get the relevant values given the
values obtained through unwinding so far.

If the readelf --debug-dump=frames-interp output looks wrong, it may also be
useful to look at readelf --debug-dump=frames, which is closer to the
information that GHC generated.

It's also useful to dump the relevant Cmm with -ddump-cmm -ddump-opt-cmm
-ddump-cmm-proc -ddump-cmm-verbose. Note [Unwind pseudo-instruction in Cmm]
explains how to interpret it.

Inside gdb there are a couple useful commands for inspecting frames.
For example:

  gdb> info frame <num>

It shows the values of registers obtained through unwinding.

Another useful thing to try when debugging the DWARF unwinding is to enable
extra debugging output in GDB:

  gdb> set debug frame 1

This makes GDB produce a trace of its internal workings. Having gone this far,
it's just a tiny step to run GDB in GDB. Make sure you install debugging
symbols for gdb if you obtain it through a package manager.

Keep in mind that the current release of GDB has an instruction pointer handling
heuristic that works well for C-like languages, but doesn't always work for
Haskell. See Note [Info Offset] in Dwarf.Types for more details.

Note [Unwind pseudo-instruction in Cmm]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the possible CmmNodes is a CmmUnwind pseudo-instruction. It doesn't
generate any assembly, but controls what DWARF unwinding information gets
generated.

It's important to understand what ranges of code the unwind pseudo-instruction
refers to.
For a sequence of CmmNodes like:

  A // starts at addr X and ends at addr Y-1
  unwind Sp = Just Sp + 16;
  B // starts at addr Y and ends at addr Z

the unwind statement reflects the state after A has executed, but before B
has executed. If you consult the Note [Debugging DWARF unwinding info], the
LOC this information will end up in is Y.
-}

-- | A label associated with an 'UnwindTable'
data UnwindPoint = UnwindPoint !CLabel !UnwindTable

instance Outputable UnwindPoint where
  ppr (UnwindPoint lbl uws) =
      braces $ ppr lbl<>colon
      <+> hsep (punctuate comma $ map pprUw $ Map.toList uws)
    where
      pprUw (g, expr) = ppr g <> char '=' <> ppr expr

-- | Maps registers to expressions that yield their "old" values
-- further up the stack. Most interesting for the stack pointer @Sp@,
-- but might be useful to document saved registers, too. Note that a
-- register's value will be 'Nothing' when the register's previous
-- value cannot be reconstructed.
type UnwindTable = Map.Map GlobalReg (Maybe UnwindExpr)

-- | Expressions, used for unwind information
data UnwindExpr = UwConst !Int                  -- ^ literal value
                | UwReg !GlobalReg !Int         -- ^ register plus offset
                | UwDeref UnwindExpr            -- ^ pointer dereferencing
                | UwLabel CLabel
                | UwPlus UnwindExpr UnwindExpr
                | UwMinus UnwindExpr UnwindExpr
                | UwTimes UnwindExpr UnwindExpr
                deriving (Eq)

instance Outputable UnwindExpr where
  pprPrec _ (UwConst i)     = ppr i
  pprPrec _ (UwReg g 0)     = ppr g
  pprPrec p (UwReg g x)     = pprPrec p (UwPlus (UwReg g 0) (UwConst x))
  pprPrec _ (UwDeref e)     = char '*' <> pprPrec 3 e
  pprPrec _ (UwLabel l)     = pprPrec 3 l
  pprPrec p (UwPlus e0 e1)  | p <= 0
                            = pprPrec 0 e0 <> char '+' <> pprPrec 0 e1
  pprPrec p (UwMinus e0 e1) | p <= 0
                            = pprPrec 1 e0 <> char '-' <> pprPrec 1 e1
  pprPrec p (UwTimes e0 e1) | p <= 1
                            = pprPrec 2 e0 <> char '*' <> pprPrec 2 e1
  pprPrec _ other           = parens (pprPrec 0 other)

-- | Conversion of Cmm expressions to unwind expressions. We check for
-- unsupported operator usages and simplify the expression as far as
-- possible.
toUnwindExpr :: Platform -> CmmExpr -> UnwindExpr
toUnwindExpr _ (CmmLit (CmmInt i _))       = UwConst (fromIntegral i)
toUnwindExpr _ (CmmLit (CmmLabel l))       = UwLabel l
toUnwindExpr _ (CmmRegOff (CmmGlobal g) i) = UwReg g i
toUnwindExpr _ (CmmReg (CmmGlobal g))      = UwReg g 0
toUnwindExpr platform (CmmLoad e _)               = UwDeref (toUnwindExpr platform e)
toUnwindExpr platform e@(CmmMachOp op [e1, e2])   =
  case (op, toUnwindExpr platform e1, toUnwindExpr platform e2) of
    (MO_Add{}, UwReg r x, UwConst y) -> UwReg r (x + y)
    (MO_Sub{}, UwReg r x, UwConst y) -> UwReg r (x - y)
    (MO_Add{}, UwConst x, UwReg r y) -> UwReg r (x + y)
    (MO_Add{}, UwConst x, UwConst y) -> UwConst (x + y)
    (MO_Sub{}, UwConst x, UwConst y) -> UwConst (x - y)
    (MO_Mul{}, UwConst x, UwConst y) -> UwConst (x * y)
    (MO_Add{}, u1,        u2       ) -> UwPlus u1 u2
    (MO_Sub{}, u1,        u2       ) -> UwMinus u1 u2
    (MO_Mul{}, u1,        u2       ) -> UwTimes u1 u2
    _otherwise -> pprPanic "Unsupported operator in unwind expression!"
                           (pprExpr platform e)
toUnwindExpr _ e
  = pprPanic "Unsupported unwind expression!" (ppr e)
