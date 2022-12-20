# Google Summer of Code 2022: Project Summary
## MMTk Port for GHC: An alternative of the current GHC storage manager

This is the final report for the GSoC 2022 project - binding MMTk-GHC. The project is a continuation of the semester project of [Introduce NoGC from MMTk](https://gitlab.haskell.org/JunmingZhao42/ghc/-/tree/mmtk/nogc2.0/rts/mmtk/docs/introduce_nogc.pdf).


## Outline
- [Motivation](#motivation)
- [Completed Work](#completed-work)
- [Demo & Conclusion](#demo-conclusion)
- [Future Work](#future-work)
- [Acknowledgement](#acknowledgement)
- [References](#references)


## Motivation

### What is MMTk-GHC binding?

It's connection between the GHC runtime and a set of garbage collectors in MMTk. A successful binding will allow developers to choose MMTk's collectors, instead of the native GHC storage manager, providing a greater variety for their needs.

### Why do we need the binding?

- Introduce more garbage collection strategies to GHC, potentially improving performance; 
- Improve the generality of GHC's runtime;
- Allow more opportunities for further development of GHC's garbage collectors, such as multi-domain GC;
- Improve the compatibility of MMTk to different programming languages.


## Completed Work

We have been working on the implementations of the VM interface of MMTk by GHC's runtime and object model:
- Implement and refine a binding to GHC's object model in Rust
- Introduce a new RTS way and associated compiler flag, `-mmtk`, to select the MMTk-enabled runtime system
- Implement tracing (i.e. scavenge and evacuate) of GHC heap objects in Rust
- Connect MMTk's allocators and GC workers to GHC's runtime
- Integrate MMTk's garbage collection cycle into the runtime system as depicted below:
```
          GHC runtime                              MMTk

  Init runtime │                                    │
               │       Bind MMTk allocator(s)       │
               │ ────────────────────────────────►  │
               │                                    │
               │        Require allocation          │
               │ ────────────────────────────────►  │
               │                .                   │
               │                .                   │
               │                .                   │
               │                                    │
               │                                    │
               │        Require allocation          │
               │ ───────────────────────────────►   │
               │                                    │
               │                                    │
               │                                    │ Schedule GC
               │                                    │
               │        Stop runtime mutators       │
               │  ◄───────────────────────────────  │
               │                                    │ Trace objects from roots
               │                                    │
               │                                    │ MMTk GC
               │                                    │
               │      Resume runtime mutators       │
               │  ◄───────────────────────────────  │
               │                .                   │
               │                .                   │
               │                .                   │
               │                                    │
```

### Major Implementations
**Modifications to GHC's for MMTk integration:**
- MMTk is initialized in `rts/RtsStartup.c:hs_init_ghc`
- MMTk's mutators are bound in `rts/Task.c:newTask`
- The code generator was modified to populate the STG machine's `Hp` and `HpLim` registers from MMTk's BumpAllocator in `GHC.StgToCmm.Foreign`.
- `HeapStackCheck.cmm` gained some logic for handling heap overflows in `stg_no_regs` when using MMTk
- MMTk is entered in `rts/Schedule.c:scheduleDoGC` during the handling of heap overflows

**The main part of the binding is under `rts/mmtk/mmtk/src`, where we teach MMTk about the runtime logic and object model of GHC:**
- Various details to do with starting and stopping mutators around GC are found in `rts/mmtk/mmtk/rts/collection.rs`
- The scanning logic is found in `rts/mmtk/mmtk/src/{scanning,object_scanning,active_plan}.rs`
- The Rust representation of the GHC heap object model is found in `rts/mmtk/mmtk/src/{stg_closures,stg_info_table}.rs`
- The linking between GHC and MMTk is found in `rts/mmtk/mmtk/build.rs` and `rts/mmtk/mmtk/src/ghc.rs`
- Documentation about how to use the binding in `rts/mmtk/README.md`

## Demo & Conclusion
Currently, we can test the binding with a non-moving Immix GC plan from MMTk.

We have a demonstration program [`fibo.hs`](https://gitlab.haskell.org/JunmingZhao42/ghc/-/blob/mmtk/nogc2.0/fibo.hs), which can complete a few MMTk's garbage collection cycles.
To test it, follow the steps below:

0. [Build MMTk binding and GHC](https://gitlab.haskell.org/JunmingZhao42/ghc/-/blob/mmtk/nogc2.0/rts/mmtk/README.md#build)
1. Compile `fibo.hs`: 
        ```
        _build/stage1/bin/ghc -fforce-recomp -mmtk -rtsopts -g3 -threaded -debug -Lrts/mmtk/mmtk/target/debug -optl-lmmtk_ghc fibo.hs
        ```
2. Run:
        ```
        MMTK_THREADS=1 RUST_BACKTRACE=1 MMTK_PLAN=Immix ./fibo 5000 +RTS -M3M
        ```

Expected output:
```
[2022-09-11T11:57:00Z INFO  mmtk::policy::immix::immixspace] Creating non-moving ImmixSpace: immix. Block size: 2^15
[2022-09-11T11:57:02Z INFO  mmtk::memory_manager] Initialized MMTk with Immix
...
... # Program prints fibonacci number
...
[2022-09-11T16:13:25Z INFO  mmtk::plan::global]   [POLL] immortal: Triggering collection
[2022-09-11T16:13:26Z INFO  mmtk::scheduler::gc_work] End of GC
[2022-09-11T16:13:26Z INFO  mmtk::plan::global]   [POLL] immortal: Triggering collection
[2022-09-11T16:13:26Z INFO  mmtk::scheduler::gc_work] End of GC
Segmentation fault (core dumped)
```

`fibo.hs` can complete a few GC cycles, but unfortunately hits segfault in the end. The most likely cause is that the current tracing is non-exhaustive, which results in MMTk treating alive objects as dead.
MMTk then reuses the object space for allocation, but later GHC runtime attempts to read the already-overwritten space, causing segfault.


## Future Work
- [ ] Integrate support for clearing swept objects into MMTk's Immix policy
- [ ] Debug the current object tracing bug seen above
- [ ] Implement tests for object tracing model
- [ ] Refine object tracing model on objects such as weak pointers
- [ ] Support more GC plans from MMTk 


## Acknowledgement
I am grateful to Google for facilitating the whole program and providing such a fantastic experience to work on real-world open-source project.

Special thanks to my mentors Ben, Dominic from Haskell, my supervisor Steve from MMTk, and the GHC and MMTk communities who helped me throughout the summer.


## References
- [MMTk porting guide](https://www.mmtk.io/mmtk-core/portingguide/prefix.html)
- [MMTk-core](https://github.com/mmtk/mmtk-core)
- [GHCSM](https://well-typed.com/blog/aux/files/nonmoving-gc/design.pdf)
