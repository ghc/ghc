# Tests that call 'collect_compiler_stats' are skipped when debugging is on.
# See testsuite/driver/testlib.py.
setTestOpts(no_lint)

test('T1969',
     [# expect_broken(12437),
      collect_compiler_residency(20),
      extra_run_opts('+RTS -A64k -RTS'),
          # The default RESIDENCY_OPTS is 256k and we need higher sampling
          # frequency. Incurs a slow-down by about 2.
      collect_compiler_stats('bytes allocated', 1),
      only_ways(['normal']),

      extra_hc_opts('-dcore-lint -static'),
          # Leave -dcore-lint on for this one test, so that we have something
          # that will catch a regression in -dcore-lint performance.
     ],
     compile,
     [''])

# This one tests for the space leak in the native code generator,
# where it holds onto the entire asm output until the end.  The space
# leak reappears from time to time, so it's a good idea to have a test
# for it.  The residency of this test will jump by 10MB to 40MB or so
# on x86-64 if the space leak appears.

# Only run this one if we have an NCG:
if 'optasm' in config.compile_ways:
   conf_3294 = only_ways(['normal'])
else:
   conf_3294 = skip

test('T3294',
     [collect_compiler_residency(15),
      collect_compiler_stats('bytes allocated', 1),
      conf_3294,
      ],
     compile,
     [''])

test('T4801',
     [collect_compiler_stats('bytes allocated',2),
      only_ways(['normal']),
      extra_hc_opts('-static'),
      when(arch('wasm32') and unregisterised(), fragile(23290))
      ],
     compile,
     [''])

test('T3064',
     [collect_compiler_residency(20),
      collect_compiler_stats('bytes allocated',2),
      only_ways(['normal']),
      ],
     compile,
     [''])

# The foldr/build rule is the important one
test('T4007', normal, makefile_test, ['T4007'])

test('T5030',
     [collect_compiler_stats('bytes allocated', 2),

       only_ways(['normal'])
      ],
     compile,
     ['-freduction-depth=300'])

test('T5631',
     [collect_compiler_stats('bytes allocated',2),
      only_ways(['normal'])
      ],
     compile,
     [''])

test('parsing001',
     [collect_compiler_stats('bytes allocated',2),
       only_ways(['normal']),
      ],
     compile_fail, [''])


test('T783',
     [ only_ways(['normal']),  # no optimisation for this one
      collect_compiler_stats('bytes allocated',2),
      extra_hc_opts('-static')
      ],
      compile,[''])

test('T5321Fun',
     [ only_ways(['normal']),  # no optimisation for this one
       collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T5321FD',
     [ only_ways(['normal']),  # no optimisation for this one
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T5642',
     [ only_ways(['normal']),
       normal,
       collect_compiler_stats('bytes allocated',2)
      ],
      compile,['-O'])

test('T5837',
     [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated',5)
      ],
      compile, ['-freduction-depth=50'])

test('T6048',
     [ only_ways(['optasm']),
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T9020',
     [ only_ways(['optasm']),
      collect_compiler_stats('bytes allocated',2)
      ],
      compile,[''])

test('T9675',
     [ only_ways(['optasm']),
       collect_compiler_residency(15),
       collect_compiler_stats('bytes allocated',2),
      ],
     compile,
     [''])

test('T9872a',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
       high_memory_usage
      ],
     compile_fail,
     [''])

test('T9872b',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
       high_memory_usage
      ],
     compile_fail,
     [''])
test('T9872b_defer',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
       high_memory_usage
      ],
     compile,
     ['-fdefer-type-errors'])
test('T9872c',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
       high_memory_usage
      ],
     compile_fail,
     [''])
test('T9872d',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile,
     [''])
# Since major improvements to T8095 in in
# 4bf9fa0f216bb294c1bd3644363b008a8643a653 it is subject to
# Note [Sensitivity to unique increment] in T12545.hs; spread was 1.7%
test ('T8095',
      [ only_ways(['normal']),
        collect_compiler_stats('bytes allocated',4) ],
      compile,
      ['-v0 -O'])
# See Note [Sensitivity to unique increment] in T12545.hs; spread was 1.5%
test ('T13386',
      [ only_ways(['normal']),
        collect_compiler_stats('bytes allocated',3) ],
      compile,
      ['-v0 -O0'])

#########
# The following tests are very sensitive
# to coercion optimisation.

test ('T15703',
      [ only_ways(['normal']),
        collect_compiler_stats('bytes allocated',1)
      , extra_files(['T15703_aux.hs'])
      ],
      multimod_compile,
      ['T15703', '-v0 -O'])

test ('CoOpt_Read',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])

test('CoOpt_Singletons',
     [ collect_compiler_stats('bytes allocated',2) ],
     compile,
     ['-v0 -O'])

#########

# Moved from simplCore/should_compile
test('T18730',
      [ only_ways(['optasm'])
      , collect_compiler_stats('bytes allocated',1)
      , extra_files(['T18730_A.hs'])
      ],
      multimod_compile,
      ['T18730', '-v0 -O'])

# LargeRecord is subject to Note [Sensitivity to unique increment] in T12545.hs
# observed spread was 2.2%
test ('LargeRecord',
      [ only_ways(['normal']),
        collect_compiler_stats('bytes allocated',5)
      , extra_files(['SuperRecord.hs'])
      ],
      multimod_compile,
      ['LargeRecord', '-v0 -O'])

test('T9961',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
      ],
     compile,
     ['-O'])

test('T9233',
    [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated', 1)
    ],
    multimod_compile,
    ['T9233', '-v0 -O2 -fno-spec-constr'])

test('T10370',
     [ only_ways(['optasm']),
       collect_compiler_residency(15),
     ],
     compile,
     [''])

test('T11068', normal, makefile_test, ['T11068'])

test('T10547',
     [ collect_compiler_stats('bytes allocated', 4),
     ],
     compile_fail,
     ['-fprint-expanded-synonyms'])

test('T12227',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1)
     ],
     compile,
     # Use `-M1G` to prevent memory thrashing with ghc-8.0.1.
     ['-O2 -ddump-hi -ddump-to-file +RTS -M1G'])

test('T12425',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 1)
     ],
     compile,
     [''])

test('T12234',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 2),
     ],
     compile,
     [''])

# See Note [Sensitivity to unique increment] in T12545.hs; spread was 4.8%
test('T12545',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 10), #
     ],
     multimod_compile,
     ['T12545', '-v0'] )

test('T13035',
     [ only_ways(['normal']),
       collect_compiler_stats('bytes allocated', 1),
     ],
     compile,
     [''] )

test('T13056',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated',2),
     ],
     compile,
     ['-O1'])

test('T12707',
     [ collect_compiler_stats('bytes allocated', 1),
     ],
     compile,
     [''])

# This test is meant to test regressions involving the
# pattern match checker. Any regression there will show
# up massively, but otherwise it hardly allocates. So we
# are slightly more generous with the allocation threshold
# to avoid spurious errors.
test('T12150',
     [ only_ways(['optasm']),
       collect_compiler_stats('bytes allocated', 2)
     ],
    compile,
     [''])

test('T13379',
     [ collect_compiler_stats('bytes allocated',2),
     ],
     compile,
     [''])

# Note [Increased initial stack size for MultiLayerModules]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The MultiLayerModules test, and a few other tests like it, had a
# tendency to fluctuate wildly with the default initial stack size of
# 1k (see #19293), presumably because it creates threads that happen
# to need very close to 1k of peak stack space. Increasing the initial
# stack size to 2k via the -ki RTS option seems to avoid this problem
# (though the size of the fluctuations does perhaps suggest that it
# would be valuable to solve the pathology in a more general way).

test('MultiLayerModules',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genMultiLayerModules'),
       extra_files(['genMultiLayerModules']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['MultiLayerModules', '-v0 +RTS -ki2k -kb2k -RTS'])


test('MultiLayerModulesRecomp',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('$MAKE -s --no-print-directory MultiModulesRecomp'),
       extra_files(['genMultiLayerModules']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['MultiLayerModules', '-v0 +RTS -ki2k -kb2k -RTS'])


# A performance test for calculating link dependencies in --make mode.
test('MultiLayerModulesTH_Make',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('$MAKE -s --no-print-directory MultiLayerModulesTH_Make_Prep'),
       extra_files(['genMultiLayerModulesTH']),
       unless(have_dynamic(),skip),
       compile_timeout_multiplier(5),
       # We skip the test on darwin due to recent regression due to toolchain
       # upgrade (tracked in #24177)
       when(opsys('darwin'), skip)
     ],
     multimod_compile_fail,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['MultiLayerModules', '-v0 +RTS -ki2k -kb2k -RTS'])

# A performance test for calculating link dependencies in -c mode.
test('MultiLayerModulesTH_OneShot',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('$MAKE -s --no-print-directory MultiLayerModulesTH_OneShot_Prep'),
       extra_files(['genMultiLayerModulesTH']),
       unless(have_dynamic(),skip),
       compile_timeout_multiplier(5),
       # We skip the test on darwin due to recent regression due to toolchain
       # upgrade (tracked in #24177)
       when(opsys('darwin'), skip)
     ],
     compile_fail,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['-v0 +RTS -ki2k -kb2k -RTS'])

test('MultiLayerModulesDefsGhci',
     [ collect_compiler_residency(15),
       pre_cmd('./genMultiLayerModulesDefs'),
       extra_files(['../../ghci/shell.hs', 'genMultiLayerModulesDefs']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     ghci_script,
     ['MultiLayerModulesDefsGhci.script'])

test('MultiLayerModulesDefsGhciWithCore',
     [ collect_compiler_residency(15),
       pre_cmd('$MAKE -s --no-print-directory MultiModulesRecompDefsWithCore'),
       extra_files(['genMultiLayerModulesCore', 'MultiLayerModulesDefsGhciWithCore.script']),
       compile_timeout_multiplier(5),
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
       extra_run_opts('-fwrite-if-simplified-core MultiLayerModules')
     ],
     ghci_script,
     ['MultiLayerModulesDefsGhciWithCore.script'])

test('MultiLayerModulesDefsGhcWithCore',
     [ collect_compiler_residency(15),
       pre_cmd('$MAKE -s --no-print-directory MultiModulesDefsWithCore'),
       extra_files(['genMultiLayerModulesCore']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['MultiLayerModules', '-v0 -fwrite-if-simplified-core'])

test('MultiLayerModulesDefsGhciReload',
     [ collect_compiler_residency(15),
       pre_cmd('./genMultiLayerModulesDefs'),
       extra_files(['genMultiLayerModulesDefs']),
       compile_timeout_multiplier(10)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     ghci_script,
     ['MultiLayerModulesDefsGhciReload.script'])

test('InstanceMatching',
     [ collect_compiler_stats('bytes allocated',3),
       pre_cmd('$MAKE -s --no-print-directory InstanceMatching'),
       extra_files(['genMatchingTest']),
       compile_timeout_multiplier(5),
       grep_errmsg('F001')],
     multimod_compile_fail,
     ['Match', '-fno-code -fwrite-interface -v0'])

# This module stresses the KnownTC and NoKnownTC (skolem variable) cases
test('InstanceMatching1',
     [ collect_compiler_stats('bytes allocated',3),
       pre_cmd('./genMatchingTest 1'),
       extra_files(['genMatchingTest']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['Defs', '-fno-code -fwrite-interface -v0'])


test('MultiLayerModulesNoCode',
     [ collect_compiler_residency(15),
       pre_cmd('./genMultiLayerModulesNoCode'),
       extra_files(['genMultiLayerModulesNoCode']),
       compile_timeout_multiplier(5)
       # this is _a lot_
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     ghci_script,
     ['MultiLayerModulesNoCode.script'])

test('MultiComponentModulesRecomp',
     [ collect_compiler_stats('bytes allocated', 2),
       pre_cmd('$MAKE -s --no-print-directory MultiComponentModulesRecomp'),
       extra_files(['genMultiComp.py']),
       compile_timeout_multiplier(5)
     ],
     multiunit_compile,
     [['unitp%d' % n for n in range(20)], '-fno-code -fwrite-interface -v0'])

test('MultiComponentModules',
     [ collect_compiler_stats('bytes allocated', 2),
       pre_cmd('$PYTHON ./genMultiComp.py'),
       extra_files(['genMultiComp.py']),
       compile_timeout_multiplier(5)
     ],
     multiunit_compile,
     [['unitp%d' % n for n in range(20)], '-fno-code -fwrite-interface -v0'])

test('ManyConstructors',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genManyConstructors'),
       extra_files(['genManyConstructors']),
     ],
     multimod_compile,
     ['ManyConstructors', '-v0'])

test('ManyAlternatives',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genManyAlternatives'),
       extra_files(['genManyAlternatives']),
     ],
     multimod_compile,
     ['ManyAlternatives', '-v0'])

test('T13701',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genT13701'),
       extra_files(['genT13701']),
       compile_timeout_multiplier(4)
       # 4 is _a lot_ (timeout after 1200s),
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['T13701', '-v0 +RTS -ki2k -kb2k -RTS'])

test('T13719',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genT13719'),
       extra_files(['genT13719']),
       compile_timeout_multiplier(2)
       # 2 is _a lot_ (timeout after 600s, to build 600 modules),
       # but this test has been failing every now and then,
       # especially on i386. Let's just give it some room
       # to complete successfully reliably everywhere.
     ],
     multimod_compile,
     ['T13719', '-v0'])

test('T14697',
     [ collect_compiler_stats('bytes allocated',2),
       # This generates too large of a command-line for poor Windows and
       # Darwin. The solution is to teach the testsuite driver to
       # invoke GHC with a response file.
       when(opsys('mingw32') or opsys('darwin'), fragile(15072)),
       pre_cmd('./genT14697'),
       extra_files(['genT14697']),
       extra_hc_opts('$(cat T14697-flags)'), # 10k -optP arguments
     ],
     multimod_compile,
     # see Note [Increased initial stack size for MultiLayerModules]
     ['T14697', '-v0 +RTS -ki2k -kb2k -RTS'])

test('T14683',
     [ collect_compiler_stats('bytes allocated',2),
     ],
     multimod_compile,
     ['T14683', '-v0'])

test ('T9630',
      [ collect_compiler_residency(15),
        collect_compiler_stats('bytes allocated', 2),
      ],
      multimod_compile,
      ['T9630', '-v0 -O'])

test ('T15164',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])
test('T15630',
      [collect_compiler_stats()
      ],
      compile,
      ['-O2'])
test('T15630a',
      [collect_compiler_stats()
      ],
      compile,
      ['-O2'])

# See https://gitlab.haskell.org/ghc/ghc/merge_requests/312#note_186960
test ('WWRec',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])

test('T16190',
      [ req_th,
        unless(have_ncg(), skip), # T16190 tests a NCG feature
        collect_compiler_stats('bytes allocated',20)
      ],
      multimod_compile,
      ['T16190.hs', '-v0'])

# Run this program. If specialisation fails, it'll start to allocate much more
test ('T16473',
      [ collect_stats('bytes allocated',5)
      , only_ways(['normal'])
      , js_broken(22261)
      ],
      compile_and_run,
      ['-O2 -flate-specialise'])

test('T17516',
      [ collect_compiler_stats('bytes allocated', 5),
      ],
      multimod_compile,
      ['T17516', '-O -v0'])


test ('T18304',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])

test ('T18282',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])
test ('T18140',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])
test('T10421',
    [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated', 1)
    ],
    multimod_compile,
    ['T10421', '-v0 -O'])
test('T10421a',
    [ only_ways(['normal']),
      collect_compiler_stats('bytes allocated', 10)
    ],
    multimod_compile,
    ['T10421a', '-v0 -O'])
test ('T13253',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])
test ('T13253-spj',
      [ collect_compiler_stats('bytes allocated',2)
      ],
      compile,
      ['-v0 -O'])
test ('T14766',
      [ collect_compiler_stats('bytes allocated',2),
        pre_cmd('python3 genT14766.py > T14766.hs'),
        extra_files(['genT14766.py']),
      ],
      compile,
      ['-v0'])
test ('T18223',
      [ collect_compiler_stats('bytes allocated',2)
      , compile_timeout_multiplier(2)
        # needed for the unregistered build (#18795)
      ],
      compile,
      ['-v0 -O'])
test ('T18923',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])
test ('T16577',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      ['-v0 -O'])
test ('T9198',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      [''])

test('T11545',
     [ collect_compiler_stats('all', 15) ],
     compile, ['-O'])

test('T15304',
     [ collect_compiler_stats('all', 10) ],
     compile, ['-O'])
test ('T20049',
      [ collect_compiler_stats('bytes allocated',2) ],
      compile,
      [''])

test ('T19695',
      [ collect_compiler_stats('bytes allocated',2)],
      compile,
      ['-v0 -O2'])

test('hard_hole_fits', # Testing multiple hole-fits with lots in scope for #16875
     collect_compiler_stats('bytes allocated', 2), # 1 is 300s, 0.010 is 3s. Without  hole-fits it takes 1s
     compile, ['-fdefer-type-errors -fno-max-valid-hole-fits -package ghc'])

test('T16875',  # Testing one hole-fit with a lot in scope for #16875
     # This test is very sensitive to environmental differences.. we should fix
     # that but for now the failure threshold is 4% (see #21557)
     collect_compiler_stats('bytes allocated', 4),
     compile, ['-fdefer-type-errors -fno-max-valid-hole-fits -package ghc'])

test ('T20261',
      [collect_compiler_stats('all')],
      compile,
      [''])

# Track perf of generics based binary instances
# See also T21839r in perf/should_run... the testsuite
# does not sensibly handle one test acting as both
# a compile-time and a run-time performance test
test('T21839c',
    [   collect_compiler_stats('all', 10),
        collect_compiler_stats('bytes allocated', 1),
        only_ways(['normal'])],
    compile,
    ['-O'])

test ('InfiniteListFusion',
      [collect_stats('bytes allocated',2), when(wordsize(32), skip)],
      compile_and_run,
      ['-O2 -package ghc'])

# Track performance of record update renaming/typechecking
test('RecordUpdPerf',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('./genRecordUpdPerf'),
       extra_files(['genRecordUpdPerf']),
     ],
     multimod_compile,
     ['RecordUpdPerf', '-fno-code -v0'])

test('T22744',
     [ collect_compiler_stats('peak_megabytes_allocated',20),
       req_interp,
       pre_cmd('$MAKE -s --no-print-directory T22744'),
       extra_files(['genT22744']),
       compile_timeout_multiplier(5)
     ],
     multimod_compile,
     ['T22744', '-v0'])

test ('LookupFusion',
      [collect_stats('bytes allocated',2), when(wordsize(32), skip)],
      compile_and_run,
      ['-O2 -package base'])

test('T24471',
     [req_th, collect_compiler_stats('all', 5)],
     multimod_compile, ['T24471', '-v0 -O'])

test ('T24582',
      [ collect_compiler_stats('bytes allocated',5) ],
      compile,
      ['-O'])

test ('MultilineStringsPerf',
      [ collect_compiler_stats('peak_megabytes_allocated', 20),
        collect_compiler_stats('bytes allocated', 5),
      ],
      compile,
      ['-O'])
