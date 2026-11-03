The native code generator
=========================

`compiler/GHC/CmmToAsm.hs` and this directory hold GHC's native code generator
(NCG), which turns a module's Cmm into assembly. This file is a way
in for anyone working with the NCG, whether reading its output and dumps,
tracking a fault to its pass, reviewing or making a change, or maintaining it.
What the code does is stated in its Notes and comments, which this file cites
by name.

Where to start reading
----------------------

Start with three Notes in `GHC.CmmToAsm`, the first in its module header
and the other two after its imports:

* Note [Native code generator], the overview: the NCG's scope, its interfaces
  to a target, the passes in the order `cmmNativeGen` runs them, and what
  the NCG takes from the Cmm pipeline, StgToCmm and the RTS. This file refers
  to the passes by the overview's step numbers.
* Note [Labels in NCG comments]: what BUG, WIP and TODO at the head of a comment
  mean, and what a comment without a label does and does not claim.
* Note [Norms of the NCG]: the norms GHC holds the NCG to.

Then, pass by pass, in the overview's order:

* step 3, the CFG: Note [CFG Edge Weights] in `GHC.CmmToAsm.CFG`;
* step 4, instruction selection: Note [Keeping track of the current block]
  in `GHC.CmmToAsm.X86.CodeGen`;
* step 5, liveness: Note [Register formats in liveness analysis]
  in `GHC.CmmToAsm.Reg.Liveness`;
* step 6, the linear allocator: the outline at the top
  of `GHC.CmmToAsm.Reg.Linear`, Note [Unique order in the linear allocator]
  and Note [Register preferences in the linear allocator] there,
  Note [The first jump into a block fixes its entry assignment]
  in `GHC.CmmToAsm.Reg.Linear.JoinToTargets`, and, for the order in which
  it allocates the blocks, Note [Frequencies before register allocation]
  in `GHC.CmmToAsm.CFG`;
* step 9, shortcutting: Note [supporting shortcutting] in `GHC.CmmToAsm.Monad`
  and Note [Updating the CFG during shortcutting] in `GHC.CmmToAsm.CFG`;
* step 10, static prediction: Note [Static Branch Prediction],
  Note [Back edges in loopInfo], Note [Static prediction of tag tests]
  and Note [Where a branch's likely side is known] in `GHC.CmmToAsm.CFG`;
* step 11, block layout: Note [CFG based code layout],
  Note [Chain based CFG serialization] and Note [Layout relevant edge weights]
  in `GHC.CmmToAsm.BlockLayout`;
* step 13, under `-g`: Note [Unwinding information in the NCG]
  in `GHC.CmmToAsm`.

Vocabulary
----------

Terms the NCG's comments use without defining them, each with the Note
or the code that defines or uses it:

* proc point: a block that starts a procedure of its own where proc points
  are split (Note [Proc-point analysis] in `GHC.Cmm.ProcPoint`). For the NCG
  they are not split where tables are next to code.
* continuation: the block a call returns to. It carries an info table,
  and the labels of continuations are the only block labels a Cmm expression can
  name (Note [Continuation BlockIds] in `GHC.Cmm.Node`).
* info table: the data the RTS and the garbage collector read about a closure's
  code or a stack frame, laid out by `GHC.Cmm.Info`.
* tables next to code: the layout in which an info table sits just before
  the code it describes (`platformTablesNextToCode` in `GHC.Platform`,
  `TABLES_NEXT_TO_CODE` in `rts/include/rts/storage/InfoTables.h`).
* virtual register: a register from an unbounded supply (`VirtualReg`
  in `GHC.Platform.Reg`), which the register allocator maps to real registers
  and spill slots.
* real register: a machine register, identified by its number (`RealReg`
  in `GHC.Platform.Reg`).
* register class: a set of registers the allocators treat as interchangeable
  (`RegClass` in `GHC.Platform.Reg.Class`). `RegArch` there says whether
  floating-point and vector registers form one class (`Unified`, as on x86
  and AArch64) or two (`Separate`, which `registerArch` gives no architecture
  yet), or whether there are no vector registers (`NoVectors`, as on RISC-V).
* format: the width and kind of the value a register holds (`Format`
  in `GHC.CmmToAsm.Format`). Liveness records the format of each register
  (Note [Register formats in liveness analysis] in `GHC.CmmToAsm.Reg.Liveness`).
* born, dying: what liveness records of a register at an instruction (`Liveness`
  in `GHC.CmmToAsm.Reg.Liveness`), the born registers only for the graph
  allocator and `-ddump-asm-liveness` (`regLivenessBorn`). A register is born
  where it is first written. It dies by a read where it is read for the last
  time, and dies by a write where it is written and not live afterwards.
* spill slot: a stack slot that holds a virtual register's value while
  no register does. The C stack area the RTS reserves holds `maxSpillSlots`
  of them, and `ncgAllocMoreStack` reserves more (Note [extra spill slots]
  in `GHC.CmmToAsm.X86.Instr`).
* InReg, InMem, InBoth: where the linear allocator keeps a virtual register:
  in a register, in a spill slot, or in both (`VLoc`
  in `GHC.CmmToAsm.Reg.Linear.Base`).
* entry assignment: where each virtual register live into a block is,
  as the linear allocator fixes it at the first of the block's incoming jumps
  that it processes
  (Note [The first jump into a block fixes its entry assignment]
  in `GHC.CmmToAsm.Reg.Linear.JoinToTargets`). The linear allocator's comments
  also call it the block's recorded assignment.
* fixup block: a block the linear allocator inserts between a jump
  and its target, to move each value to where the target's entry assignment has
  it (`joinToTargets_again` in `GHC.CmmToAsm.Reg.Linear.JoinToTargets`).
* clobber: to destroy a register's value as a side effect. A call instruction
  clobbers the target's `callClobberedRegs` (`callerSavedRegisters` on AArch64,
  RV64 and LA64), and an unsafe foreign call clobbers the STG registers that
  `callerSaves` names
  (Note [Unsafe foreign calls clobber caller-save registers] in `GHC.Cmm.Node`).
* shortcut: to retarget a jump to a block that is only a jump, so that it goes
  straight to that jump's target. For Cmm, see Note [What is shortcutting]
  in `GHC.Cmm.ContFlowOpt`; for the NCG, step 9 of Note [Native code generator].
* far branch: a branch whose target may lie beyond the reach of its offset.
  `ncgMakeFarBranches` (step 11) replaces it with instructions of longer reach;
  for AArch64, see Note [AArch64 far jumps] in `GHC.CmmToAsm.AArch64.CodeGen`.
* edge weight: a number on a CFG edge, higher for an edge taken more often
  (`GHC.CmmToAsm.CFG`). `getCfg` sets it from `-fblock-layout-weights`,
  and under `-fcmm-static-pred` it is replaced by the frequency that `calcFreqs`
  computes.
* back edge: an edge into a block still on the stack of a depth-first search
  from the entry (`findBackEdges`, which counts no self loop); `loopInfo` takes
  such an edge for a loop's back edge where the block dominates the edge's
  source. Note [Back edges in loopInfo] in `GHC.CmmToAsm.CFG` names one kind
  of such edge that it misses.
* irreducible: said of a block that has, when `calcFreqs` reaches it,
  a predecessor not yet visited whose edge into it is not one of `loopInfo`'s
  back edges.
* chain: a sequence of blocks that the layout places one after another, built
  along heavy edges (Note [Chain based CFG serialization]
  in `GHC.CmmToAsm.BlockLayout`).
* tag test: a test of a pointer's tag: whether it is tagged, the closure
  it points to then being evaluated (Note [diamond proc point]
  in `GHC.Cmm.LayoutStack`), or whether it, or its closure's info table, holds
  a given constructor's tag (Note [Static prediction of tag tests]
  in `GHC.CmmToAsm.CFG`).

Finding the pass at fault
-------------------------

First confirm the wrong result against an oracle:

* the LLVM back end (`-fllvm`, where LLVM is installed), which runs none
  of the NCG's passes but step 1's `fixStgRegisters`. Its Cmm pipeline differs
  from the NCG's in two settings only: it always splits proc points,
  so that stack layout splits no body off a tag test's continuation
  (Note [Tagged path skips the reloads] in `GHC.Cmm.LayoutStack`), and it leaves
  switches to LLVM rather than to switch plans.
* GHCi, for a Haskell program: its bytecode starts from STG.
* `-O0`, which turns off at once every pass below that has a `-fno-` flag.
  For a Haskell module it changes the Cmm as well.

Then switch off or swap the passes that have a flag, one at a time
and the latest first:

* step 11: `-fno-block-layout-cfg`, which lays the blocks out
  by `sequenceBlocks`, with the CFG's weights, or without them
  if `-fblock-layout-weightless` is given too;
* step 10: `-fno-cmm-static-pred`, which also takes from step 6 the linear
  allocator's estimate of block frequencies;
* step 9: `-fno-asm-shortcutting`;
* step 6: `-fregs-graph` or `-fregs-iterative` in place of the linear allocator;
* in the Cmm pipeline: `-fno-cmm-sink`, which also stops stack layout loading
  fields again after a call (Note [Fields loaded again after a call]
  in `GHC.Cmm.LayoutStack`), `-fno-cmm-elim-common-blocks`
  and `-fno-cmm-control-flow`.

The other steps have no flag of their own. `-dcmm-lint` and `-dasm-lint` catch
some faults where they arise, and `-ddump-asm-native` and `-ddump-asm-regalloc`
show the code before and after register allocation.

To cut a case down, write the procedure that `-ddump-cmm` shows as a `.cmm`
file, in the syntax `GHC.Cmm.Parser` accepts, and check it with `-dcmm-lint`.
A test in `testsuite/tests/cmm/should_run` or `codeGen/should_run` calls
its procedures from a Haskell driver and compares their output; a test
in `codeGen/should_gen_asm` compares the generated assembly.

One function through the dumps
------------------------------

The module

```haskell
module Loop (sumTo) where

-- The sum of 1 to n.
sumTo :: Int -> Int
sumTo n = n `seq` go 0 1
  where
    go acc i | i > n     = acc
             | otherwise = go (acc + i) (i + 1)
```

compiled on x86-64 by GHC 10.1.20261103, the compiler of commit 37ad471050, with

```
ghc -O -c Loop.hs -ddump-to-file -ddump-opt-cmm -ddump-asm-native \
    -ddump-asm-liveness -ddump-asm-regalloc -ddump-asm-stats \
    -ddump-cfg-weights -ddump-asm
```

gives the excerpts below; another compiler or target gives other code.

Labels such as `cIj` and `n4`, and virtual registers such as `%vI_tHU`,
are named by uniques. Labels and register names vary by compiler, and a change
to the module can rename them, so compare names only within the dumps of one
compilation.

`-ddump-opt-cmm` shows the Cmm after step 2. Here the loop is `cIj`, `cIm`
and `cIv`. `cIj` is a heap check: when it fails, `cIn` calls the garbage
collector, which returns to the continuation `cIi`, which jumps back to `cIj`.
`HpLim` has no machine register on x86-64, so step 1 has already turned
it into a load from `BaseReg`:

```
      cIj: // global
          Hp = Hp + 16;
          if (Hp > I64[BaseReg + 856]) (likely: False) goto cIn; else goto cIm;
      cIn: // global
          ...
          call stg_gc_ii(R2, R1) returns to cIi, args: 8, res: 8, upd: 8;
      cIi: // global
          ...
          goto cIj;
      cIm: // global
          if (%MO_S_Le_W64(_tHV::I64, _tHS::I64)) goto cIv; else goto cIu;
      cIv: // global
          ...
          goto cIj;
```

`-ddump-asm-native` shows the code after instruction selection (step 4),
over virtual registers. Each Cmm local keeps its unique: `_tHS` becomes
`%vI_tHS`. Every block ends in jumps that name all its successors:

```
.LcIm:
    cmpq %vI_tHS,%vI_tHV
    jle .LcIv
    jmp .LcIu
```

`-ddump-asm-liveness` shows the result of step 5. The blocks are grouped
into strongly connected components, `REC` for the loop and `NONREC` for a block
alone. Each instruction is annotated with the registers born and dying there,
and their formats. In this dump a real register appears as its number: `%r1`
is `%rbx`. At a jump, the registers live into its targets and not past the jump
are listed as dying too (the comment on `Liveness`
in `GHC.CmmToAsm.Reg.Liveness`):

```
     NONREC
        cIi:
                ...
                movq %rbx,%vI_tHU
                    # born:    %vI_tHU :: II64
                    # r_dying: %r1 :: II64
                jmp .LcIj
                    # r_dying: %vI_tHS :: II64 %vI_tHU :: II64 %vI_tHV :: II64
                     ,
     REC
        cIj:
```

`-ddump-asm-regalloc` shows the code after register allocation (step 6). Here
another jump into `cIj` was allocated before `cIi`'s, and it fixed `cIj`'s entry
assignment (Note [The first jump into a block fixes its entry assignment]).
`cIi`, where the garbage collector returns, conforms to that assignment through
the fixup block `n4`, whose label comes from the NCG's own uniques:

```
.LcIi:
    ...
    jmp .Ln4
.Ln4:
    movq %rbx,%rcx
    movq %r14,%rbx
    jmp .LcIj
```

`-ddump-asm-regalloc-stages` shows the graph allocator's stages (step 6),
and prints nothing unless `-fregs-graph` or `-fregs-iterative` is given.

`-ddump-asm-stats` counts what the linear allocator added (step 6). Here
it added no spill and no reload, and two register-to-register moves at a join
(`joinRR`), which are the two moves in `n4`:

```
-- spills-added-total
--    (allocs, clobbers, loads, joinRR, joinRM, reg_reg_moves_remaining)
(0, 0, 0, 2, 0, 5)
```

`-ddump-cfg-weights` prints three sections. "CFG Weights - Native" is the CFG
after step 4, with the weights of Note [CFG Edge Weights].
"CFG Update information" lists the blocks that register allocation added (step
7), here `n4` between `cIi` and `cIj`. "CFG Weights - Final" is the CFG after
step 10, with estimated frequencies (Note [Static Branch Prediction]). Here
the heap check's two edges keep the ratio of their native weights, 300 to 900,
in their final frequencies (Note [Where a branch's likely side is known]):

```
==================== CFG Weights - Native ====================
    ...
    cIj -> cIn[label="300.00000",weight="300.00000"];
    ...
    cIj -> cIm[label="900.00000",weight="900.00000"];
    ...
==================== CFG Update information ====================
    ...
linearAlloc: [(cIi, n4, cIj)]
==================== CFG Weights - Final ====================
    ...
    cIj -> cIn[label="1.00000",weight="1.00000"];
    ...
    cIj -> cIm[label="3.00000",weight="3.00000"];
```

`-ddump-asm` shows the assembly as printed (step 13), after block layout (step
11). The info tables sit just before their blocks. Here the layout has placed
`cIv` before `cIj`, so the loop's back edge falls through, has dropped the jumps
to the block placed next, `cIi`'s jump to `n4` among them, and has put
the unlikely blocks `cIn` and `cIb` last:

```
.align 8
    .quad 65
    .long 30
    .long 0
.Lblock_cIi_info:
.LcIi:
    movq 8(%rbp),%rax
    addq $8,%rbp
.Ln4:
    movq %rbx,%rcx
    movq %r14,%rbx
    jmp .LcIj
.LcIv:
    addq $-16,%r12
    addq %rbx,%rcx
    incq %rbx
.LcIj:
    addq $16,%r12
    cmpq 856(%r13),%r12
    ja .LcIn
.LcIm:
    cmpq %rax,%rbx
    jle .LcIv
.LcIu:
    ...
.LcIn:
    ...
.LcIb:
    leaq Loop.sumTo_closure(%rip),%rbx
    jmp *-8(%r13)
```

Reviewing a change
------------------

Questions to ask of a change, by its kind:

* An instruction selector that splits a block. Does it return its last new block
  as the current block, and add each new block to the CFG
  (Note [Keeping track of the current block] in `GHC.CmmToAsm.X86.CodeGen`)?
  Does each block it makes end in jumps that name all its successors (step 4
  of Note [Native code generator])? Can its update of the CFG meet a block
  that the CFG lacks (BUG #28088 at `getCfg` in `GHC.CmmToAsm.CFG`)?
* A new instruction. Does `regUsageOfInstr` state the registers it reads
  and writes as the allocators are to see them (the comment on `regUsageOfInstr`
  in `GHC.CmmToAsm.Instr`)? Does `jumpDestsOfInstr` name every block it jumps
  to (step 4 of Note [Native code generator])?
* A change that rewrites an instruction's registers after liveness. Does
  it compute again the usage that liveness keeps (`liveUsage`
  in `GHC.CmmToAsm.Reg.Liveness`), as `patchRegsLiveInstr` does? The allocators
  read that usage, and on x86 it can change when two registers become equal
  (Note [What x86's regUsageOfInstr reports] in `GHC.CmmToAsm.X86.Instr`).
  Does anything read `liveAcross` after it? `patchRegsLiveInstr` leaves that
  set, the registers live after a write of a real register, as liveness made
  it: only the linear allocator reads it, before any register is rewritten.
* A change that updates the CFG during or after instruction selection. Does
  it keep the CFG's edges in step with the code? `-dasm-lint` does not check
  the edges (Note [Updating the CFG during shortcutting] in `GHC.CmmToAsm.CFG`,
  and BUG #27694 at `genPrim` in `GHC.CmmToAsm.X86.CodeGen`).
* An order taken from uniques. Is the output still the same
  under `-fobject-determinism` (Note [Norms of the NCG],
  and Note [Unique order in the linear allocator] in `GHC.CmmToAsm.Reg.Linear`)?
* A change to the linear allocator at a jump. Does every jump into a block still
  conform to the entry assignment that the first one recorded
  (Note [The first jump into a block fixes its entry assignment]
  in `GHC.CmmToAsm.Reg.Linear.JoinToTargets`)?
* A change to the entry points of the linear allocator or the liveness pass,
  their types or their pragmas. Does the code generator for x86-64 still call
  their copies specialised to x86's instructions
  (Note [Specialising the linear allocator] in `GHC.CmmToAsm.Reg.Linear`)?
  Without them the code is the same and only the compiler is slower.
* A change to static prediction or block layout. Does it keep the trade-offs
  that Note [Where a branch's likely side is known] in `GHC.CmmToAsm.CFG` lists,
  or, where it revisits one, does it answer the reason the Note gives for it?
* A change to code that a labelled statement describes. Does it remove
  or rewrite the statement? Does a new statement carry the label its kind calls
  for (Note [Labels in NCG comments])?
* A test written in Cmm. Does `-dcmm-lint` accept it (Note [Norms of the NCG])?

What the labelled statements cover
----------------------------------

The labelled statements were first written from an examination
of the machine-independent passes (the CFG, block layout, liveness and both
register allocators), and of the x86-64, i386 and AArch64 code generators where
those passes depend on them. The examination left out the PowerPC, RISC-V,
LoongArch and wasm32 code generators and `GHC.CmmToAsm.Dwarf`, reading them only
where the overview or a labelled statement states a fact about them;
in that code, a missing label says nothing. Nor did the examination reach every
function of the parts it covered, so the absence of a BUG statement does
not mean that the code is correct.
Note [Labels in NCG comments] says what a comment older than the labels does
and does not claim.
