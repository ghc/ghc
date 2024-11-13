test('testblockalloc',
     [c_src, only_ways(['normal','threaded1']), extra_run_opts('+RTS -I0')],
     compile_and_run, [''])

test('testmblockalloc',
     [c_src, only_ways(['normal','threaded1']), extra_run_opts('+RTS -I0 -xr0.125T'),
      when(arch('wasm32'), skip)], # MBlocks can't be freed on wasm32, see Note [Megablock allocator on wasm] in rts
     compile_and_run, [''])
# -I0 is important: the idle GC will run the memory leak detector,
# which will crash because the mblocks we allocate are not in a state
# the leak detector is expecting.


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010',
     # Non-moving GC doesn't support -c
     omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc']),
     compile_and_run, ['+RTS -c -RTS'])

def normalise_address(str):
    return re.sub('Access violation in generated code when reading [0]+',
                  'Access violation in generated code when reading ADDRESS',
                  str)

test('derefnull',
     [# The LLVM backend in particular does not guarantee any particular
      # behavior on null dereference (similar to #10332).
      omit_ways(llvm_ways),
      when(not(have_ncg()), skip),
      # SIGSEGV on Linux (which we make the default)
      exit_code(139),
      # Apparently the output can be different on different
      # Linux setups, so just ignore it. As long as we get
      # the right exit code we're OK.
      when(opsys('linux'), ignore_stderr),
      # On OpenBSD the output varies depending on ulimit, exit code is enough.
      when(opsys('openbsd'), ignore_stderr),
      # SIGBUS on OX X (PPC and x86 only; amd64 gives SEGV)
      # The output under OS X is too unstable to readily compare
      when(platform('x86_64-apple-darwin'), [ignore_stderr, exit_code(139)]),
      when(platform('aarch64-apple-darwin'), [ignore_stderr, exit_code(139)]),
      when(opsys('mingw32'), [ignore_stderr, exit_code(11)]),
      when(opsys('mingw32'), [fragile(18548)]),
      when(arch('javascript'), [ignore_stderr, exit_code(1)]),
      # On wasm32, 0x0 is a valid linear memory address
      when(arch('wasm32'), [ignore_stdout, ignore_stderr, exit_code(0)]),
      # ThreadSanitizer changes the output
      when(have_thread_sanitizer(), skip),
      # since these test are supposed to crash the
      # profile report will be empty always.
      # so disable the check for profiling
      when(opsys('mingw32'), omit_ways(prof_ways))],
     compile_and_run, ['-with-rtsopts="--generate-stack-traces=no"'])
test('divbyzero',
     [# SIGFPE on Linux
      exit_code(136),
      # The LLVM backend in particular does not guarantee any particular
      # behavior on division-by-zero (#10332).
      omit_ways(llvm_ways),
      when(not(have_ncg()), skip),
      # Aarch64 does not have div-by-zero exceptions for sdiv/udiv.
      # The only option would be to implement this in assembly with checks for
      # each devision. Neither gcc, nor llvm do this as of right now.  Microsoft
      # apparently does so though?
      when(arch('aarch64'), skip),
      # RISCV64 has no exception for this, too. Instead all bits of the result
      # are set. As with Aarch64 neither GCC nor LLVM translate this result to
      # an exception.
      when(arch('riscv64'), skip),
      # Apparently the output can be different on different
      # Linux setups, so just ignore it. As long as we get
      # the right exit code we're OK.
      when(opsys('linux'), ignore_stderr),
      # On OpenBSD the output varies depending on ulimit, exit code is enough.
      when(opsys('openbsd'), ignore_stderr),
      # PowerPC 64 bit and most likely PowerPC 32 bit processors
      # do not generate an exception (interrupt) for integer
      # division by zero but the result is undefined.
      # C programs compiled with gcc exit normally, so do we.
      when(platform('powerpc64-unknown-linux'), [ignore_stdout, exit_code(0)]),
      when(platform('powerpc64le-unknown-linux'), [ignore_stdout, exit_code(0)]),
      when(opsys('mingw32'), [ignore_stderr, exit_code(8)]),
      when(opsys('mingw32'), [fragile(18548)]),
      # The output under OS X is too unstable to readily compare
      when(platform('x86_64-apple-darwin'), [ignore_stderr, exit_code(136)]),
      # ThreadSanitizer changes the output
      when(have_thread_sanitizer(), skip),
      # wasmtime returns sigabrt error code upon wasm traps
      when(arch('wasm32'), [ignore_stdout, ignore_stderr, exit_code(134)]),
      # since these test are supposed to crash the
      # profile report will be empty always.
      # so disable the check for profiling
      when(opsys('mingw32'), omit_ways(prof_ways))],
     compile_and_run, ['-with-rtsopts="--generate-stack-traces=no"'])

def remove_parenthesis(s):
    return re.sub(r'\s+\([^)]*\)', '', s)

test('outofmem', [ when(opsys('darwin'), skip),
                   # this is believed to cause other processes to die
                   # that happen concurrently while the outofmem test
                   # runs in CI. As such we'll need to disable it on
                   # windows, to prevent absolute exhaustion of memory
                   # and subsequent termination (and failure) of unrelated
                   # tests.
                   js_skip,
                   # similarly for the JS backend
                   when(opsys('mingw32'), skip),
		   normalise_errmsg_fun(remove_parenthesis)],
                 makefile_test, ['outofmem'])
test('outofmem2',
  [ js_skip # JS backend doesn't detect heap exhaustion
  ], makefile_test, ['outofmem2'])

test('T2047',
     [ignore_stdout,
      extra_run_opts('+RTS -c -RTS'),
      # Non-moving collector doesn't support -c
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_sanity', 'nonmoving_thr_ghc'])],
     compile_and_run, ['-package containers'])

# Blackhole-detection test.
# Skip GHCi due to #2786
test('T2783', [ omit_ghci, exit_code(1)
              , fragile_for(2783, ['threaded1'])
              ], compile_and_run, [''])

# Test the work-stealing deque implementation.  We run this test in
# both threaded1 (-threaded -debug) and threaded2 (-threaded) ways.
test('testwsdeque', [extra_files(['../../../rts/WSDeque.h']),
                     unless(in_tree_compiler(), skip),
                    c_src, only_ways(['threaded1', 'threaded2'])],
                    compile_and_run, [''])

test('T3236', [c_src, only_ways(['normal','threaded1']), exit_code(1)], compile_and_run, [''])

test('stack001', extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('stack002', [extra_files(['stack001.hs']),
                  extra_run_opts('+RTS -K32m -k4m -RTS')],
     compile_and_run, [''])

# run this test with very small stack chunks to exercise the stack
# overflow/underflow machinery.
test('stack003', [ omit_ghci, # parameter 50000 is not passed
                   extra_run_opts('500000 +RTS -kc1k -kb100 -K96m -RTS') ],
                 compile_and_run, [''])

# Test that +RTS -K0 (e.g. no stack limit) parses correctly
test('stack004', [ extra_run_opts('+RTS -K0 -RTS')
                 , js_broken(22374)
                 , expect_broken_for(14913, ['ghci'])
                 ], compile_and_run, [''])

test('atomicinc', [ c_src, only_ways(['normal','threaded1', 'threaded2']) ], compile_and_run, [''])
test('atomicxchg', [ c_src, only_ways(['threaded1', 'threaded2']) ],
compile_and_run, [''])

test('T3424',
     [ when(unregisterised(), fragile(16349)),
       when(fast(), skip),
       only_ways(['normal','threaded1','ghci'])
     ],
     compile_and_run, [''])

# Test for out-of-range heap size
test('rtsflags001', [ only_ways(['normal']),
                      exit_code(1),
                      extra_run_opts('+RTS -H0m -RTS'),
                      js_skip # JS backend uses its own rts.
                    ],
                     compile_and_run, [''])

# Crashed with 7.2 and earlier
test('rtsflags002', [ only_ways(['normal']), js_broken(22261) ], compile_and_run, ['-with-rtsopts="-B -B -B"'])

# omit dyn and profiling ways, because we don't build dyn_l or p_l
# variants of the RTS by default
test('traceEvent', [ omit_ways(['dyn'] + ghci_ways + prof_ways),
                     extra_run_opts('+RTS -ls -RTS'),
                     js_skip # JS backend has no payload size limit
                   ],
                   compile_and_run, [''])

test('traceBinaryEvent', [ omit_ways(['dyn'] + ghci_ways + prof_ways),
                           extra_run_opts('+RTS -ls -RTS'),
                           js_skip # traceBinaryEvent not supported
                           ],
                         compile_and_run, [''])

# Test that -ol flag works as expected
test('EventlogOutput1',
     [ extra_files(["EventlogOutput.hs"]),
       omit_ways(['dyn'] + prof_ways),
       js_skip
     ],
     makefile_test, ['EventlogOutput1'])

# Test that -ol flag defaults to <program>.eventlog
test('EventlogOutput2',
     [ extra_files(["EventlogOutput.hs"]),
       omit_ways(['dyn'] + prof_ways),
       js_skip
     ],
     makefile_test, ['EventlogOutput2'])

test('EventlogOutputNull',
     [ extra_files(["EventlogOutput.hs"]),
       omit_ways(['dyn'] + prof_ways) ],
     makefile_test, ['EventlogOutputNull'])

# Test that Info Table Provenance (IPE) events are emitted.
test('EventlogOutput_IPE',
     [ extra_files(["EventlogOutput.hs"]),
       only_ways(debug_ways),
       ignore_stdout,
       ignore_stderr
     ],
     makefile_test, ['EventlogOutput_IPE'])

test('T4059', req_c, makefile_test, ['T4059'])

# Test for #4274
test('exec_signals',
     [when(opsys('mingw32'), skip),
      pre_cmd('$MAKE -s --no-print-directory exec_signals-prep'),
      cmd_prefix('./exec_signals_prepare'),
      req_process,
      req_c], # C compiler is used to build exec_signals_prepare/child programs.
              # With the JS backend, it fails because these programs are
              # compiled with Emscipten and they can't be directly executed
              # (permission denied error); they must be loaded in nodejs (or
              # compiled with the host CC compiler).
     compile_and_run, [''])

test('return_mem_to_os', normal, compile_and_run, [''])

test('T4850',
  [ when(opsys('mingw32'), expect_broken(4850))
  , js_broken(22261) # FFI "dynamic" convention unsupported
  ], makefile_test, ['T4850'])

def config_T5250(name, opts):
    if not (config.arch in ['i386','x86_64']):
        opts.skip = 1;

test('T5250', [extra_files(['spalign.c']),
                config_T5250,
                when(platform('i386-unknown-linux'),
                            expect_broken_for(4211,['llvm']))],
              compile_and_run, ['spalign.c'])

test('T5423', cmm_src, makefile_test, ['T5423'])

test('T5993', extra_run_opts('+RTS -k8 -RTS'), compile_and_run, [''])

test('T6006', [ omit_ways(prof_ways + ghci_ways),
                 pre_cmd('$MAKE -s --no-print-directory T6006_setup'),
                 req_c
                 ],
                 # The T6006_setup hack is to ensure that we generate
                 # T6006_stub.h before compiling T6006_c.c, which
                 # needs it.
               compile_and_run, ['T6006_c.c -no-hs-main'])

test('T7037', req_c, makefile_test, ['T7037'])

test('T7087', exit_code(1), compile_and_run, [''])
test('T7160', [ # finalization order is different in the nonmoving
                omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc', 'nonmoving_thr_sanity'])
              , js_broken(22261)
              ], compile_and_run, [''])

test('T7040', [req_c], compile_and_run, ['T7040_c.c'])

test('T7040_ghci',
     [extra_files(['T7040_c.h']),
      only_ways(['ghci']),
      # Fragile when unregisterised; see #16085
      when(unregisterised(), skip),
      pre_cmd('$MAKE -s --no-print-directory T7040_ghci_setup ghciWayFlags=' + config.ghci_way_flags),
      when(opsys('linux') and not ghc_dynamic() and not arch('wasm32'), fragile(20706))],
     compile_and_run, ['T7040_ghci_c.o'])

test('T7227', [extra_run_opts('+RTS -tT7227.stat --machine-readable -RTS')],
     compile_and_run, [''])

test('T7636', [ exit_code(1), extra_run_opts('100000') ], compile_and_run, [''] )

test('stablename001', [expect_fail_for(['hpc'])], compile_and_run, [''])
# hpc should fail this, because it tags every variable occurrence with
# a different tick.  It's probably a bug if it works, hence expect_fail.

test('T7815', [ multi_cpu_race,
                extra_run_opts('50000 +RTS -N2 -RTS'),
                req_target_smp, req_ghc_smp,
                only_ways(['threaded1', 'threaded2']) ], compile_and_run, [''] )

# ignore_stderr because it contains a unique:
#   ffishutdown: Main_dul: interrupted
test('ffishutdown', [ignore_stderr, only_ways(['threaded1','threaded2'])],
     compile_and_run, [''])

# Times out in ghci way.
# Also times out on x86_64-linux from time to time.
test('T7919', [ when(fast(), skip)
              , omit_ghci
              , req_th
              , when(platform('x86_64-unknown-linux'), fragile(22283))
              , js_fragile(24259)
              ]
              , compile_and_run, [config.ghc_th_way_flags])

test('T8035', normal, compile_and_run, [''])

test('T8209', [ req_target_smp, req_ghc_smp, only_ways(threaded_ways), ignore_stdout ],
              compile_and_run, [''])

test('T8242', [ req_target_smp, req_ghc_smp, only_ways(threaded_ways), ignore_stdout ],
              compile_and_run, [''])

test('T8124', [ only_ways(threaded_ways), omit_ghci,
                 when(opsys('mingw32'), skip), # uses pthreads
                 pre_cmd('$MAKE -s --no-print-directory T8124_setup') ],
                 # The T8124_setup hack is to ensure that we generate
                 # T8124_stub.h before compiling T8124_c.c, which
                 # needs it.
               compile_and_run, ['T8124_c.c -no-hs-main'])

# +RTS -A8k makes it fail faster
# The ghci way gets confused by the RTS options
test('T9045', [ omit_ghci, extra_run_opts('10000 +RTS -A8k -RTS') ], compile_and_run, [''])

# T9078 needs to be compiled with -debug, which threaded1 does for us.
# Assert failure is reproducible with ghc-7.8.2.
test('T9078', only_ways(['threaded1']), compile_and_run, [''])

test('T10017', [ when(opsys('mingw32'), skip)
               , when(unregisterised(), skip)
               , req_target_smp
               , req_ghc_smp
               , only_ways(threaded_ways), extra_run_opts('+RTS -N2 -RTS') ], compile_and_run, [''])

test('T11108', normal, compile_and_run, [''])

test('GcStaticPointers', [ when(doing_ghci()
                         , extra_hc_opts('-fobject-code'))
                         , js_broken(22261)
                         ],
     compile_and_run, [''])
test('ListStaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])

# 251 = RTS exit code for "out of memory"
test('overflow1', [ js_skip, exit_code(251), when(wordsize(32), expect_broken(15255)) ],
     compile_and_run, [''])
test('overflow2', [ js_skip,
                    exit_code(1 if arch('wasm32') else 251),
                    when(arch('wasm32'), ignore_stderr) ], compile_and_run, [''])
test('overflow3', [ js_skip,
                    exit_code(1 if arch('wasm32') else 251),
                    when(arch('wasm32'), ignore_stderr) ], compile_and_run, [''])

def grep_stderr(pattern):
    def wrapper(cmd, pattern=pattern):
        swap12 = '3>&1 1>&2 2>&3 3>&-' # Swap file descriptors 1 and 2.
        return('{cmd} {swap12} | grep "{pattern}" {swap12}'.format(**locals()))
    return cmd_wrapper(wrapper)

# The ghci way gets confused by the RTS options
test('T9839_01',
     [ omit_ghci, extra_run_opts('+RTS -T-s'), no_check_hp
     , grep_stderr('given an argument when none was expected')
     , js_broken(22261)
     ],
     compile_and_run, [''])

test('T9839_02',
     [only_ways(prof_ways), extra_run_opts('+RTS -Pax'), no_check_hp,
      grep_stderr('given an argument when none was expected')],
     compile_and_run, [''])

test('T9839_03',
     [only_ways(prof_ways), extra_run_opts('+RTS -Px'), no_check_hp,
      grep_stderr('given an argument when none was expected')],
     compile_and_run, [''])

test('T9839_04',
     [only_ways(prof_ways), extra_run_opts('+RTS -xc')],
     compile_and_run, [''])

test('T9839_05',
     [only_ways(prof_ways), extra_run_opts('+RTS -xcx'), no_check_hp,
      grep_stderr('given an argument when none was expected')],
     compile_and_run, [''])

# ignore_stderr as RTS reports slightly different error messages
# in 'epoll' and 'select' backends on reading from EBADF
# mingw32 skip as UNIX pipe and close(fd) is used to exercise the problem
test('T10590', [ ignore_stderr
               , when(opsys('mingw32'), skip)
               , js_skip # JS backend doesn't support pipes
               , req_process
               ], compile_and_run, [''])

# 20000 was easily enough to trigger the bug with 7.10
test('T10904', [ extra_run_opts('20000'), req_c ],
               compile_and_run, ['T10904lib.c'])

test('T10728', [extra_run_opts('+RTS -maxN3 -RTS'), only_ways(['threaded2'])],
               compile_and_run, [''])

test('T9405', [when(opsys('mingw32'), fragile(21361)), js_broken(22261)], makefile_test, ['T9405'])

test('T11788', [ when(ghc_dynamic(), skip)
               , req_interp
               ], makefile_test, ['T11788'])

test('T10296a', [req_ghc_smp, req_c], makefile_test, ['T10296a'])

test('T10296b', [only_ways(['threaded2'])], compile_and_run, [''])

test('numa001', [ extra_run_opts('8'), unless(unregisterised(), extra_ways(['debug_numa'])), req_ghc_with_threaded_rts ]
                , compile_and_run, [''])

test('T12497', [ unless(opsys('mingw32'), skip), expect_broken(22694)
               ],
               makefile_test, ['T12497'])

test('T13617', [ unless(opsys('mingw32'), skip)],
               makefile_test, ['T13617'])

# Test is fragile on all systems. #21184 links to various other reports through
# its history.
test('T12903', [ fragile(21184)
               , js_broken(22374)
               , req_process]
               , compile_and_run, [''])

test('T13832', [exit_code(1), req_ghc_with_threaded_rts], compile_and_run, ['-threaded'])
# js_skip T13894 because the JS backend only allocates pinned arrays so this
# test will always fail
test('T13894', js_skip, compile_and_run, [''])
# this test fails with the profasm way on some machines but not others,
# so we just skip it.
test('T14497', [omit_ways(['profasm']), multi_cpu_race], compile_and_run, ['-O'])
test('T14695', [js_broken(22359), ignore_stderr], makefile_test, ['T14695'])
test('T14702', [ ignore_stdout
               , when(unregisterised(), skip)
               , when(opsys('mingw32'), fragile(18953))
               , only_ways(['threaded1', 'threaded2'])
               , extra_run_opts('+RTS -A32m -N8 -T -RTS')
               ]
               , compile_and_run, [''])

test('T14900',
  [ js_skip # Compact regions not supported by the JS backend yet
  ], compile_and_run, ['-package ghc-compact'])

test('InternalCounters',
  [ js_skip # JS backend doesn't support internal counters
    # The ways which build against the debug RTS are built with PROF_SPIN and
    # therefore differ in output
  , omit_ways(['nonmoving_thr_sanity', 'threaded2_sanity', 'sanity'])
  ], makefile_test, ['InternalCounters'])
test('alloccounter1', js_broken(22261), compile_and_run,
  [
    # avoid allocating stack chunks, which counts as
    # allocation and messes up the results:
    '-with-rtsopts=-k1m'
  ])

test('nursery-chunks1',
  [ extra_run_opts('4 100 +RTS -n32k -A1m -RTS')
  , req_ghc_smp
  , req_target_smp
  , only_ways(['threaded1','threaded2'])
  ],
  compile_and_run,
  [''])

# Test for the "Evaluated a CAF that was GC'd" assertion in the debug
# runtime, by dynamically loading code that re-evaluates the CAF.
# Also tests the -rdynamic and -fwhole-archive-hs-libs flags for constructing
# binaries that support runtime dynamic loading.
test('keep-cafs-fail',
  [ extra_files(['KeepCafsBase.hs', 'KeepCafs1.hs',
                 'KeepCafs2.hs', 'KeepCafsMain.hs']),
    when(opsys('mingw32'), expect_broken (5987)),
    when(opsys('freebsd') or opsys('openbsd'), expect_broken(16035)),
    filter_stdout_lines('Evaluated a CAF|exit.*'),
    ignore_stderr, # on OS X the shell emits an "Abort trap" message to stderr
    req_rts_linker,
  ],
  makefile_test, ['KeepCafsFail'])

# Test the -fkeep-cafs flag
test('keep-cafs',
  [ extra_files(['KeepCafsBase.hs', 'KeepCafs1.hs',
                 'KeepCafs2.hs', 'KeepCafsMain.hs']),
    when(opsys('mingw32'), expect_broken (5987)),
    when(opsys('freebsd') or opsys('openbsd'), expect_broken(16035)),
    req_rts_linker
  ],
  makefile_test, ['KeepCafs'])

# Test proper functioning of C++ exceptions within a C++ program.
# On darwin, this requires -fcompact-unwind.
test('T11829', [ req_c, check_errmsg("This is a test"), when(arch('wasm32'), fragile(23244)) ], compile_and_run,
     ['T11829_c.cpp -package system-cxx-std-lib'])

test('T16514', [req_c, omit_ghci], compile_and_run, ['T16514_c.c'])
test('test-zeroongc', extra_run_opts('-DZ'), compile_and_run, ['-debug'])

test('T13676',
     [when(opsys('mingw32'), expect_broken(17447)),
      extra_files(['T13676.hs']),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     ghci_script, ['T13676.script'])
test('InitEventLogging',
     [ only_ways(['normal'])
     , extra_run_opts('+RTS -RTS')
     , req_c
     ],
     compile_and_run, ['InitEventLogging_c.c'])
test('RestartEventLogging',
     [only_ways(['threaded1','threaded2']), extra_run_opts('+RTS -la -RTS')],
     compile_and_run, ['RestartEventLogging_c.c'])

test('T17088',
     [only_ways(['normal']), extra_run_opts('+RTS -c -A256k -RTS')],
     compile_and_run, ['-rtsopts -O2'])

test('T15427', js_broken(22374), compile_and_run, [''])

test('T19481',
     [extra_run_opts('+RTS -T -RTS'),
      js_broken(22374),
      # MBlocks can't be freed on wasm32, see Note [Megablock
      # allocator on wasm] in rts
      when(arch('wasm32'), skip),
      # memory behavior changes appreciably with the nonmoving collector
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_sanity'] + ghci_ways)],
     compile_and_run, [''])
test('T19381',
     [extra_run_opts('+RTS -T -RTS'),
      js_broken(22374),
      # MBlocks can't be freed on wasm32, see Note [Megablock
      # allocator on wasm] in rts
      when(arch('wasm32'), skip),
      # memory behavior changes appreciably with the nonmoving collector
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_sanity'] + ghci_ways)],
     compile_and_run, [''])
test('T20199', [ grep_errmsg('Hello'), req_c ]
      , makefile_test, [])

# We need to be precise about the used way here as different ways may lead to
# different closures (and their orders) on the stack (which is checked by this
# test).
test('cloneMyStack', [req_c,only_ways(['normal']), extra_files(['cloneStackLib.c'])],
     compile_and_run, ['cloneStackLib.c'])

test('cloneMyStack2',
  [ ignore_stdout
  , js_broken(22261) # cloneMyStack# not yet implemented
  ], compile_and_run, [''])

test('cloneMyStack_retBigStackFrame', [req_c, extra_files(['cloneStackLib.c']), ignore_stdout], compile_and_run, ['cloneStackLib.c'])

test('cloneThreadStack', [req_c, only_ways(['threaded1']), extra_ways(['threaded1']), extra_files(['cloneStackLib.c']), req_ghc_with_threaded_rts], compile_and_run, ['cloneStackLib.c -threaded'])

test('decodeMyStack',
  [ omit_ghci, js_broken(22261) # cloneMyStack# not yet implemented
  ], compile_and_run, ['-finfo-table-map'])

# Options:
#   - `-kc8K`: Set stack chunk size to it's minimum to provoke underflow stack frames.
test('decodeMyStack_underflowFrames',
  [ extra_run_opts('+RTS -kc8K -RTS')
  , omit_ghci, js_broken(22261) # cloneMyStack# not yet implemented
  ], compile_and_run, ['-finfo-table-map -rtsopts'])

# -finfo-table-map intentionally missing
test('decodeMyStack_emptyListForMissingFlag',
  [ ignore_stdout
  , ignore_stderr
  , js_broken(22261) # cloneMyStack# not yet implemented
  ], compile_and_run, [''])

# Tests RTS flag parsing. Skipped on JS as it uses a distinct RTS.
test('T20201a', [js_skip, exit_code(1)], compile_and_run, ['-with-rtsopts -AturtlesM'])
test('T20201b', [js_skip, exit_code(1)], compile_and_run, ['-with-rtsopts -A64z'])

test('T22012', [js_skip, extra_ways(['ghci'])], compile_and_run, ['T22012_c.c'])

# Skip for JS platform as the JS RTS is always single threaded
test('T22795a', [only_ways(['normal']), js_skip, req_ghc_with_threaded_rts], compile_and_run, ['-threaded'])
test('T22795b', [only_ways(['normal']), js_skip], compile_and_run, ['-single-threaded'])
test('T22795c', [only_ways(['normal']), js_skip], compile_and_run, ['-threaded -single-threaded'])

test('T17574', [js_skip], compile_and_run, ['-with-rtsopts -T'])

test('T23221',
     [js_skip,
      # This test is highly dependent upon GC behavior
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_sanity', 'nonmoving_thr_ghc']),
      high_memory_usage,
      extra_run_opts('1500000'),
      unless(wordsize(64), skip),
      omit_ghci],
     compile_and_run,
     ['-O -with-rtsopts -T'])

test('T23142', [unless(debug_rts(), skip), req_interp], makefile_test, ['T23142'])

test('T23400', [], compile_and_run, ['-with-rtsopts -A8k'])

test('IOManager', [js_skip, when(arch('wasm32'), skip), when(opsys('mingw32'), skip),
                   pre_cmd('$MAKE -s --no-print-directory IOManager.hs')],
                  compile_and_run, [''])

test('T24142', [req_target_smp], compile_and_run, ['-threaded -with-rtsopts "-N2"'])

test('T25232', [unless(have_profiling(), skip), only_ways(['normal','nonmoving','nonmoving_prof','nonmoving_thr_prof']), extra_ways(['nonmoving', 'nonmoving_prof'] + (['nonmoving_thr_prof'] if have_threaded() else []))], compile_and_run, [''])
test('T25280', [unless(opsys('linux'),skip),req_process,js_skip], compile_and_run, [''])
