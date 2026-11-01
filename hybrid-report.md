Agent H's report: a cheaper library rule than lo2, and the reserved-frame join (rf). Nothing committed.

4. Summary
4.1. Library rule (task 1): no rule keeps most of lo2's gain for much less text. lo2's text is g8's copies at the small methods its gain comes from (eqChar, timesDouble, eqInt, the `==` selector, `Num Int` methods, integerGt#), so bounds cannot separate the two, and dropping the copies loses more than lo2 gains. The best trade measured is ln32: lo2 only in procedures of at most 32 Cmm middle nodes. It keeps about 3/4 of lo2's gain for about half of its text.
4.2. Reserved-frame join (task 2): built as rfa, reserved loop-head seeds in LayoutStack. It recovers the thunk loops: at -O2, wheel-sieve1, fft2, infer and expert come to under 0.8% above LF8s or below it, and bspt gains 1.6 points more. It costs no compile allocation.
4.3. Recommended: lo2,ln32,rfa (mw-hybrid3-ln32rfa.patch). It beats ghc-za2 on every column but T3294 at -O0 (+4.56% against +4.52%). lo2,rfa (mw-hybrid3-rfa.patch) is about 0.1% faster again, at lo2's full text.

5. Table
Every row is nofib with each compiler's own libraries, against LF8s (zl8s), primes left out (112 programs), geomean with programs up/down by 1%. "own" is the text of the programs' own objects against x2. gi is ghc-internal's text against x2. T3294 (-O / -O0) and T12707 (-O) are compile allocation against x2's compiler, all on x2's libraries (-B): x2 and LF8s by their own stage-1 compilers, the rest by ghc-y3's under the rule words. "lo2,X" means g8,ra4,ch8,lv12,lo,lor,mr,lo2,X.

```
rule                  -O2 vs LF8s       -O1 vs LF8s       own -O2/-O1     gi      T3294 -O/-O0    T12707
LF8s (zl8s)           base              base              +2.2% / +1.9%   +1.70%  +1.30% / +2.46%  +2.86%
za1, no lo2 (zaza1c)  -0.54% (14/26)    -1.60% (16/30)    +8.2% / +7.9%   +3.47%  +2.34% / +4.55%  +5.26%
za2, lo2 (zaza2b)     -1.10% (9/38)     -2.14% (11/42)    +15.1% / +15.2% +9.13%  +2.34% / +4.52%  +7.20%
lo1                   -0.66% (13/33)    -1.72% (15/35)    +10.9% / +11.1% +5.78%  +2.34% / +4.55%  +6.51%
lo2,lgn               -0.19% (36/25)    -1.30% (37/29)    +10.1% / +9.9%  +4.56%  +2.34% / +4.51%  +5.29%
lo2,ln16              -0.61% (14/31)    -1.67% (16/33)    +8.8% / +8.5%   +4.41%  +2.34% / +4.55%  +5.68%
lo2,ln24              -0.67% (13/33)    -1.72% (15/35)    +10.2% / +10.0% +5.72%  +2.34% / +4.55%  +5.68%
lo2,ln32              -0.96% (11/34)    -2.01% (14/38)    +11.8% / +11.6% +6.57%  +2.34% / +4.55%  +6.54%
lo2,rf                -1.23% (6/41)     -2.32% (7/47)     +14.6% / +14.7% +9.08%  +2.34% / +4.52%  +7.22%
lo2,rfx               -1.23% (6/43)     -2.27% (8/45)     +14.7% / +14.8% +9.09%  +2.34% / +4.52%  +7.22%
lo2,rfa               -1.29% (6/40)     -2.38% (7/47)     +14.6% / +14.7% +9.08%  +2.34% / +4.52%  +7.22%
lo2,ln32,rfa          -1.20% (6/37)     -2.30% (7/44)     +11.2% / +11.0% +6.51%  +2.34% / +4.56%  +6.55%
```

5.1. ghc-internal text alone (a probe that rebuilds only ghc-internal), as a share of what lo2 adds over za1. The probe reproduces za1's 7679047 and za2's 8099440 bytes exactly. ls4 83%, ls2 71%, ls1 48% (lo2 only in procedures of at most N sequences). ln64 78%, ln32 55%, ln24 40%, ln16 17% (at most N middle nodes). go4 87%, go2 72% (g8's copies cut to N nodes off cycles). lgn 19%. lo4,ln32 76%. lo3,ln32 68%.
5.2. Caveat on my rows: pretty and scc (under a million instructions each) run 20K to 23K instructions higher in every one of my runs, whatever the rule. Under callgrind outside nofib-run, pretty's za2 and rf binaries count the same to 0.2%. So my rows carry about +0.045% geomean and 2 extra "up" programs against rows from agent A's runs (zl8s, zaza1c, zaza2b). It does not affect comparisons among my own rows. Cause not traced.
5.3. Every build ran 113 programs at both levels. The only failures are real/eff's known compile errors (EffBench's `return`, GHC-22705); no output differed. Before each own-library run, `_build/stage1` was deleted, and every ghc-internal object postdates the compiler.

6. Task 1: candidate library rules
6.1. Where lo2's gain is. Callgrind, za1 against za2 binaries:
6.1.1. integrate: GHC.Float.timesDouble accounts for -48.5M of the -51M.
6.1.2. cryptarithm2 and cichelli: eqChar, the Classes `==` selector, eqInt, `++`, Num Int's `+`.
6.1.3. integer: integerGt#, the Num Int methods, Integral Int quot, eqInt, gtInt, geInt, leInt.
6.1.4. prolog: neChar and eqString, plus its own Subst.apply and Parse.orelse.
6.1.5. The common shape: tiny called functions, one or two evaluations each, the evaluated path frameless.
6.2. Where lo2's text is: derived-instance and class modules, the same kind of small methods: TH/Syntax +69.7K, Heap/Closures, Classes, Data/Data, TH/Lift, Float, Int, Word.
6.3. lo1 keeps 21% / 22% of lo2's gain for 41% of its library text. Weak.
6.4. lgn (lo2's sites kept, but no g8 copy at them) cuts 81% of the added text and is REFUTED.
6.4.1. It is +0.92% / +0.86% against za2 (46 up by 1%) and +0.35% / +0.31% against za1, which has no lo2 at all.
6.4.2. Mechanism (cichelli's eqChar, +6.4M, 14%): without the copy, the frameless path and the return path meet at the join, and the linear allocator's common assignment there puts moves on the evaluated path. g8's copy removes that join, so the copy is what makes MW's sequence pay outside loops.
6.5. Size bounds (lsN, lnN) and copy bounds (goN) cannot separate the gain from the text, because both sit in the same small methods (5.1). ls4 still costs 83%. The node bound has a knee between 24 and 32 nodes:
6.5.1. ln16 and ln24 are only -0.07% and -0.14% against za1 at -O2 (lo2 -0.57%); integrate and bspt lose all of lo2's gain, so timesDouble's procedure lies between 24 and 32 nodes.
6.5.2. ln32 keeps -0.42% / -0.42% against za1 (lo2 -0.57% / -0.55%), with 55% of lo2's library text, 52% of its own text and 66% of its T12707 cost.
6.5.3. ln32 against za2 at -O2: up integer +1.1%, power +1.1%, mandel +1.5%, lambda +1.7%; down infer -1.4% (lo2's infer cost gone).
6.6. T3294 stays within 0.04% under every rule here; lo2's compile cost is all T12707.

7. Task 2: the reserved-frame join (rf, rfx, rfi, rfa)
7.1. Design: Note [Reserved frames in loops], `reservedSeeds` in LayoutStack.hs. It is realised through LF8's seeding rather than by editing the join.
7.1.1. After layout's first run, a loop head that loopHeadSeeds did not seed, subject to LF8's own exclusions (the entry, proc points, a self-recursive loop without a diamond), gets a seed at the depth of the deepest call continuation in its strongly connected component. In that seed, the loop's invariants (live at the head, assigned nowhere in the loop) sit in the slots that continuation's frame gave them. The second run lays out with that seed.
7.1.2. In the second run, every call returning into that component keeps the stack's depth (allocate's `keep`, as LF8's diamonds do). The MW join then inherits the evaluated path's map, which already has the reserved depth: the return path's depth with the evaluated path's registers, as agent A's analysis asked. No Sp moves on the call path; the edge into the loop pays one Sp move and the invariants' stores.
7.1.3. Seeds that do not hold drop the run. A fallback added for all rf words: if they fail and loopHeadSeeds had seeded some head, a third run tries LF8's seeds alone, so a failed reservation no longer takes LF8's seeds with it.
7.2. The first form (only where the back edges need no fixup) did nothing on wheel-sieve1 and fft2. $wnotDivBy's head has a frameless back edge and one through g8's copy still at the frame's depth (`Sp = Sp + 24; goto` the head), which loopHeadSeeds also rejects for unequal depths. Relaxed, rf takes any back-edge depths.
7.3. Thunk loops, own code against za2's rules (-B x2 libraries), -O2 / -O1: wheel-sieve1 -3.44% / -3.39%, fft2 -2.38% / -2.15%, bspt -1.58% / -1.58%, knights 0 / -2.70%, mate -0.69% / -2.11%, simple 0 / -1.73%, last-piece -1.22% / -0.35%. The thunk loops' rise against LF8s was Sp moves: wheel-sieve1 +34.7M `add`, fft2 +27.4M `add`.
7.4. Its cost: loops that barely turn pay the edge in (`jb; add; jmp`) and an `add` to %rbp at every exit on each call.
7.4.1. queens' `safe` (own code, +1.6% at -O1): entered 10.1M times, nothing saved per iteration.
7.4.2. integerAdd and integerMul in the library (digits-of-e1 +3.2%, power +2.5% under rf): the "loop" is integerAdd's self tail call swapping its arguments.
7.5. Guards tried:
7.5.1. rfx (only where a back edge needs a fixup in the first run): digits-of-e1 and power rise as under rf, so integerAdd's loop qualifies too. No better than rf.
7.5.2. rfi (only with an invariant): loses fft2 and bspt (unsafeDrop's loop advances every variable). Wrong proxy, refuted.
7.5.3. rfa (only where the loop advances a variable live at its head: assigned from a load, arithmetic, literal or foreign call, through copies; a global register's value, a return's R1, counts as neither): excludes integerAdd's permutation and keeps every gain of rf.
7.6. rfa against za2: -0.20% / -0.24%. Up only puzzle (+2.1% / +1.5%) and queens at -O1 (+1.6%), besides the pretty/scc artefact. Against LF8s: wheel-sieve1 +4.18% to +0.60%, fft2 +2.22% to -0.24%, infer +2.81% to +0.47%, expert +1.73% to +0.71%, last-piece +4.70% to +3.41%, bspt -0.84% to -2.49%, digits-of-e1 unchanged at -10.70%, power -6.16% to -5.42%. Compile allocation: no measurable change.

8. Losers that remain against LF8s
8.1. Under lo2,ln32,rfa at -O2: puzzle +1.8%, bernouilli +1.8%, last-piece +3.4%, ben-raytrace +4.4%. At -O1: knights +1.1%, mate +1.2%, simple +1.6%, bernouilli +1.8%, puzzle +4.1%. lo2,rfa is the same except bernouilli +1.3% at both levels and mate +1.1% at -O1.
8.2. last-piece: Map insert's `$sinsert_$s$w$sgo12` (+81M against LF8s under za2).
8.2.1. 11 values are live across an evaluation that is a thunk 73% of the time. lv12 keeps MW's sequence (11 is at most 12), and so does lor, inferred from Map insert recursing non-tail into itself.
8.2.2. MW stores 11 values and the return address and moves Sp on each thunk call, and spills to the C stack, where LF8s's diamond keeps 8 in their slots. rf cannot reach it (no frameless loop).
8.3. puzzle and bernouilli: register shuffles in their own code. puzzle +12.3M `xchg` (rf against za2, own code); bernouilli's module-internal procedures +23.4M (+6.3M `mov`, +3.5M `xchg`) while its Integer calls gain. Cause in the allocator not traced.
8.4. knights -O1: tourFinished and the internal procedures after it +11.1M, +8.8M `mov`, -2.6M `add`. Register moves, not traced.
8.5. ben-raytrace: g8's cost (finding 548, a call just outside the copy reloading nine values); not re-traced.
8.6. mate and simple at -O1: not traced.

9. Refuted
9.1. lgn (6.4): MW's sequence off loops needs its copy.
9.2. lo1 (6.3): weak.
9.3. ls1/ls2/ls4, ln64, go2/go4 (6.5): text probes only; they cannot separate gain from text, because both sit in the same small methods. Not run in full.
9.4. ln16 and ln24 (6.5.1): below the knee.
9.5. rf's first form (7.2): no effect.
9.6. rfi (7.5.2): wrong proxy.
9.7. rfx (7.5.1): no better than rf.

10. Patches, trees, labels, scripts
10.1. Patches in dist-ghcho, each on MW e4f1d03f8c, touching LayoutStack.hs, Pipeline.hs and Linear.hs. Each applies to MW's files and reproduces the trees' sources exactly; GHC_MW_VARIANT still overrides the defaults.
10.1.1. mw-hybrid3-switch.patch: every rule word, no defaults. New words: lsN, lnN (ln16 .. ln256, incl. ln20/24/28/40/48), lgn, goN, rf, rfx, rfi, rfa, plus the third-run fallback, which is on under any rf word.
10.1.2. mw-hybrid3-ln32rfa.patch: defaults g8,ra4,ch8,lv12,lo,lor,mr,lo2,ln32,rfa.
10.1.3. mw-hybrid3-rfa.patch: defaults g8,ra4,ch8,lv12,lo,lor,mr,lo2,rfa. mj is left out of both defaults, since I did not measure it; finding 548 says it changes no instruction.
10.2. Trees:
10.2.1. ghc-za1: ln32rfa's sources; stage-1 compiler and libraries built with GHC_MW_VARIANT unset, ghc-internal 7905181 bytes, the same as the measured run's.
10.2.2. ghc-za2: rfa's sources, built the same way, ghc-internal 8095544 bytes, the same as the measured run's.
10.2.3. ghc-y3: switch's sources, stage-1 compiler built; its _build/stage1 holds only a probe's ghc-internal, so use it with -B (nbq.sh does).
10.2.4. Stage 2 is not built in any of them.
10.3. nofib labels (-o2/-o1): zhlo1, zhlgn, zhln16, zhln24, zhln32, zhrf, zhrfx, zhrfa, zhln32rfa. Subsets: zhq-za2/rf/rf2, zhs-za2/rf/rfx/rfi/rfa. zhrf was built before the fallback existed, so its code differs from later rf builds (rfx, rfa) wherever a reserved seed failed beside an LF8 seed.
10.4. Scripts and notes in /tmp/claude-1000/agH:
10.4.1. Building and running: chain.sh (library build, then nofib both levels), blib.sh / blibdef.sh, nbown.sh and nbq.sh (agA's with agH paths).
10.4.2. Probes: gi.sh / gi2.sh (ghc-internal text), qa.sh (compile allocation).
10.4.3. Reading and output: rows.py, table.py (this table), cgrun.sh, mkpatch.py.
10.4.4. notes.md.

11. Ranked recommendation
11.1. lo2,ln32,rfa (mw-hybrid3-ln32rfa.patch). Against ghc-za2 it is faster (-1.20% / -2.30% against LF8s, za2 -1.10% / -2.14%) and cheaper in every cost column but T3294 at -O0 (+4.56% against +4.52%): own text +11.2% against +15.1%, ghc-internal +6.51% against +9.13%, T12707 +6.55% against +7.20%.
11.2. lo2,rfa (mw-hybrid3-rfa.patch). Fastest measured (-1.29% / -2.38%), at lo2's full text and compile cost.
11.3. Whichever is taken, rfa over rf or rfx: it is the same machinery with the permutation guard.

12. Not reached, and what I may have missed
12.1. Not built: ln28 and ln40, so the knee is located only between 24 and 32. lo3,ln32 and lo4,ln32 were text-probed only (68% and 76% of lo2's text). ln24 with rfa was stopped as moot, and I deleted its -O2 results with the -O1 directory by mistake.
12.2. No static guard found for loops that rarely turn, or whose elements are evaluated (queens). rfa's permutation test is a heuristic. A loop advancing only through a non-evaluation call's R1 would be excluded; not counted.
12.3. Not counted: how often rfa's seeds fail to hold, or how often a frame does not fit the reserved holes. The latter still moves Sp, which is correct.
12.4. Not measured: containers, vector, megaparsec; -O0; the testsuite; nofib's own compile allocation; primes at -kc64k (primes left out); T8905 and GHC #27981's reproducer.
12.5. The pretty/scc offset (5.2) is not explained.
12.6. My compile allocation for za1 and za2 is +5.26% and +7.20% on T12707, where finding 548 gives +5.0% and +7.0%. All my rows come from my own measurement, the same way.
12.7. Process: I edited ghc-y3's source while a background probe rebuilt it; two probes failed and were redone. After that, edits went to copies in agH.
