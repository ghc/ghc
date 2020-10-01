# Native Code Gen for AArch64. Naive.

- [ ] Give OpReg a width, and make sure we print the corresponding reg
- [ ] Separate out FDIV
- [ ] Switch statements may have rather large offsets, and not that large ranges.
      subtracting the offset to make them 0 might reduce the number of cmp calls.

- [ ] Make the compiler spit out to stdout with -o -
- [ ] Allow us to terminate after a phase.
- [ ] Add fuse phase to turn ldr,ldr itno ldp, str,str into stp.
- [ ] Haskell Grinder Repo with test cases that excessively test the compiler
      over a longer period of time.

- [ ] Clean up
- [ ] Document more

## Intro

The AArch64 architecture by Arm, is part of Armv8, the ISA is called A64.  This
is the best ressource on the arm website: https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools
The HTML view has an instruction lookup, the xml file is also good.

### Cmm

@GHC.CmmToAsm@ contains the top-level entry point @nativeCodeGen@,
which will be called from @GHC.Driver.CodeOutput.outputAsm@, which
is triggered via the @-fasm@ flag.

Thus, we'll not concern ourselves with anything in the pipeline
before that point.

```
outputAsm :: DynFlags -> Module -> ModLocation -> FilePath
          -> Stream IO RawCmmGroup a
          -> IO a

nativeCodeGen :: forall a . DynFlags -> Module -> ModLocation -> Handle -> UniqSupply
              -> Stream IO RawCmmGroup a
              -> IO a
```

@nativeCodeGen@ will produce a configuration through @initConfig dflags@,
and pass that usually to the architecture specific implementation. E.g.
@x86NcgImpl config@ or @ppcNcgImpl config@.  Those are expected to produce

```
nCG' :: ( Outputable statics, Outputable instr
        , Outputable jumpDest, Instruction instr)
     => NcgImpl statics instr jumpDest
```

this is then fed into @nativeCodeGen'@, which will produce the final output,
in the form of @IO a@.

Therefore the job of a *new* NCG is to provide and @NcgImpl@, that can
take a @config :: NCGConfig@, that contains platform, alignment, debug, PIC,
and other configuraiton specific information.

@NcgImpl@ looks like the following:

```
data NcgImpl statics instr jumpDest = NcgImpl {
    ncgConfig                 :: !NCGConfig,
    cmmTopCodeGen             :: RawCmmDecl -> NatM [NatCmmDecl statics instr],
    generateJumpTableForInstr :: instr -> Maybe (NatCmmDecl statics instr),
    getJumpDestBlockId        :: jumpDest -> Maybe BlockId,
    canShortcut               :: instr -> Maybe jumpDest,
    shortcutStatics           :: (BlockId -> Maybe jumpDest) -> statics -> statics,
    shortcutJump              :: (BlockId -> Maybe jumpDest) -> instr -> instr,
    pprNatCmmDecl             :: NatCmmDecl statics instr -> SDoc,
    maxSpillSlots             :: Int,
    allocatableRegs           :: [RealReg],
    ncgExpandTop              :: [NatCmmDecl statics instr] -> [NatCmmDecl statics instr],
    ncgAllocMoreStack         :: Int -> NatCmmDecl statics instr
                              -> UniqSM (NatCmmDecl statics instr, [(BlockId,BlockId)]),
    -- ^ The list of block ids records the redirected jumps to allow us to update
    -- the CFG.
    ncgMakeFarBranches        :: LabelMap RawCmmStatics -> [NatBasicBlock instr] -> [NatBasicBlock instr],
    extractUnwindPoints       :: [instr] -> [UnwindPoint],
    -- ^ given the instruction sequence of a block, produce a list of
    -- the block's 'UnwindPoint's
    -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock"
    -- and Note [Unwinding information in the NCG] in this module.
    invertCondBranches        :: Maybe CFG -> LabelMap RawCmmStatics -> [NatBasicBlock instr]
                              -> [NatBasicBlock instr]
    -- ^ Turn the sequence of `jcc l1; jmp l2` into `jncc l2; <block_l1>`
    -- when possible.
    }
```

We'll thus start with the bare minimum.  Ignoring most flags for now.  The
approach we'll take is the following which has proven to work for the llvm-ng
backend.  We'll re-use the llvm code gen, and start with a trivial Main module:

```
  main = putStrLn "Hello AArch64 NCG!"
```

and compile this with @ghc -fasm -c Main.hs@ and then link it manually (pot.
after inspection).

To build GHC, we'll use a cross compiler and nix and hadrian:

```bash
nix-shell --pure -p '[ python3 haskellPackages.alex haskellPackages.happy (haskell.compiler.ghc883.override { ghcFlavour = "prof"; }) cabal-install autoconf automake gmp.dev ripgrep zlib llvmPackages_9.llvm llvmPackages_9.clang pkgsCross.aarch64-multiplatform.buildPackages.binutils pkgsCross.aarch64-multiplatform.stdenv.cc perl git linuxHeaders elf-header pkgsCross.aarch64-multiplatform.gmp.dev file qemu htop ]'
$ NM=aarch64-unknown-linux-gnu-nm LD=aarch64-unknown-linux-gnu-ld.gold AR=aarch64-unknown-linux-gnu-ar AS=aarch64-unknown-linux-gnu-as CC=aarch64-unknown-linux-gnu-cc CXX=aarch64-unknown-linux-gnu-cxx ./configure --target=aarch64-unknown-linux-gnu
-- hadrian produces a dud :-/
$ ./hadrian/build  --flavour=quickest -j
-- useing make (ensure mk/build.mk is set to quick-llvm)
$ make -j -s
```