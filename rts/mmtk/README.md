# MMTk-GHC

This repository provides binding between [MMTK](https://github.com/mmtk/mmtk-core) and [GHC](https://gitlab.haskell.org/ghc/ghc).

This binding is currently under development. We aim to make MMTk's garbage collectors work with GHC.

In order to use, follow the steps in [Contents](#contents).

---

## Contents

- [MMTk-GHC](#mmtk-ghc)
  - [Contents](#contents)
  - [Requirements](#requirements)
  - [Build](#build)
  - [Run](#run)
  - [Development Details](#development-details)
  

## Requirements

We maintain an up to date list of the prerequisites for building MMTk and its bindings in the [mmtk-dev-env](https://github.com/mmtk/mmtk-dev-env) repository. Please make sure your dev machine satisfies those prerequisites.

## Build

1. Clone the repository:
  
    ```
    git clone --recurse-submodules https://gitlab.haskell.org/JunmingZhao42/ghc.git
    cd ghc/
    git checkout mmtk/nogc2.0
    git submodule update --init --recursive
    ```
  

2. Build MMTk binding:
  
    ```
    cd rts/mmtk
    cargo build
    ```
  
3. [Build GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian):
  
    ```
    ./boot && ./configure
    hadrian/build --flavour=default+debug_info -j16
    ```
  

## Run

1. Specify MMTk's GC plan:
  
    ```
    export MMTK_PLAN=Immix
    ```
  
    Specify MMTk's thread number:
  
    ```
    export MMTK_THREADS=1
    ```
  
2. Compile your Haskell program (e.g. `hello.hs`) with MMTk (add `-rtsopts` to specify the heap size later):
  
    ```
    _build/stage1/bin/ghc -fforce-recomp -mmtk -rtsopts -threaded -debug -g3 -Lrts/mmtk/mmtk/target/debug -optl-lmmtk_ghc hello.hs
    ```
  
3. Run the program (e.g. with heap size 8M):
  
    ```
    ./hello +RTS -M8M
    ```

---
## Development Details
- University semester-project Report: [Introduce NoGC](https://gitlab.haskell.org/JunmingZhao42/ghc/-/tree/mmtk/nogc2.0/rts/mmtk/docs/introduce_nogc.pdf)
- [GSoC Report](https://gitlab.haskell.org/JunmingZhao42/ghc/-/tree/mmtk/nogc2.0/rts/mmtk/docs/GSoC_report.md)
- [Development Diary](https://edit.smart-cactus.org/n4X1UZK_TAOhI1LmmBs6Lg#Mmtk-Notes)
- IRC Channel: #ghc-mmtk:libera.chat