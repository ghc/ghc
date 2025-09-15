

ctlextra_files = extra_files(['src/',
                              'ActionsAndObservations.hs', 'BitConsumer.hs', 'CmmPaths.hs',
                              'ControlTestMonad.hs', 'EntropyTransducer.hs', 'LoadCmmGroup.hs',
                              'RunCmm.hs', 'RunWasm.hs',])

basenames = ['Church.hs',
             'Closure.hs',
             'FailingLint.hs',
             'Irr.hs',
             'Irr2.hs',
             'Irr3.hs',
             'Irr4.hs',
             'Length.hs',
             'Map.hs',
             'Max.hs',
             'PJIf.hs',
             'dec.cmm',
             'dloop.cmm',
             'ex9.cmm',
             'ex10.cmm',
             'fig1b.cmm',
             'hardswitch.cmm',
             'idmerge.cmm',
             'ifloop.cmm',
             'irr.cmm',
             'irrbad.cmm',
             'loop.cmm',
             'looptail.cmm',
             'multiswitch.cmm',
             'noloop.cmm',
             'panic.cmm',
             'panic2.cmm',
             'self.cmm',
             'selfloop.cmm',
             'switch.cmm',
             'webexample.cmm'
             ]


sources = ['src/' + basename for basename in basenames]

test('WasmControlFlow',
     [extra_run_opts(" ".join(['"' + config.libdir + '"', '-r'] + sources)), ctlextra_files, req_cmm, ignore_stdout],
     multimod_compile_and_run,
     ['WasmControlFlow', '-package ghc'])

# Note [WasmControlFlow]
# ~~~~~~~~~~~~~~~~~~~~~~
# WasmControlFlow tests GHC.Wasm.ControlFlow.FromCmm.structuredControl
# which translates a CmmGraph to a wasm CFG (only structured control
# flow allowed, no gotos).
# https://www.tweag.io/blog/2023-06-01-translation-testing describes
# the testing strategy in more details, though you probably don't need
# to read it if you're just scratching your head over a
# WasmControlFlow failure in your patch; hopefully the below quick
# explanation suffices.
#
# WasmControlFlow is a GHC API program that:
#
# 1. Takes a series of .hs/.cmm files as test input.
# 2. Compiles each test to Cmm. Each test case is now represented as
#    (FilePath, [CmmGroup]).
# 3. Iterate over each CmmGroup, find the CmmGraphs and test them.
# 4. The pass/fail status of testing each CmmGraph is represented by
#    ExitCode. All ExitCodes are combined in the end and
#    WasmControlFlow exits with ExitSuccess iff all tests return
#    ExitSuccess.
#
# Other than the exit code, WasmControlFlow also emits diagnostic info
# to stdout like:
#
# src/Irr.hs: 1 reducible, 1 irreducible
# src/Irr.hs: 1 reducible, 1 irreducible
# src/Irr.hs, node splitting: 872 paths are identical, as expected
# src/Irr.hs, mutilation: all 552 paths are different, as expected
# src/Irr.hs, mutilation: all 6 paths are different, as expected
# src/Irr.hs, WebAssembly translation: 872 paths are identical, as expected
# src/Irr.hs, WebAssembly translation: 6 paths are identical, as expected
#
# We used to check in WasmControlFlow.stdout in the tree that contains
# such diagnostics. But we shouldn't, for a couple of reasons:
#
# 1. WasmControlFlow uses the host GHC's libdir and settings. On
#    different hosts with different configure options, different
#    CmmGraphs are generated from the same test inputs.
# 2. WasmControlFlow tests CmmGraphs and emits diagnostics
#    sequentially. But in GHC there's never any guarantee that the Cmm
#    declarations will be generated in the same order! In fact, even
#    frontend changes can trigger the ordering difference, e.g.
#    !10479.
#
# The main reasons that WasmControlFlow seemed to be stable/portable
# for quite a while are:
#
# 1. It doesn't care about instructions in each basic block, only
#    cares about the shape of the CmmGraph, which has been stable
#    enough.
# 2. It doesn't run any optimization passes on Core/STG/Cmm. The
#    entire Cmm pipeline that may do platform-dependent passes like
#    proc point splitting is skipped, the stg2cmm output is used
#    directly.
#
# That being said, the stdout is still fragile to GHC refactorings and
# we should only check the exit code, hence the ignore_stdout. There
# are still a couple of potential ways to fail:
#
# 1. WasmControlFlow fails to compile. That's GHC API churn, the test
#    needs to be fixed accordingly.
# 2. WasmControlFlow compiles but reports GHC API panics when
#    compiling stuff to Cmm. Same treatment as 1 though much less
#    likely.
# 3. WasmControlFlow actually reports test failure. Might be backend
#    refactorings broke wasm logic, or the refactorings resulted in a
#    brand new CmmGraph that revealed an unspotted bug. In this case,
#    please ping the wasm backend maintainers and we're glad to help.
