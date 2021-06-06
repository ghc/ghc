test('testblockalloc',
     [c_src, only_ways(['normal','threaded1']), extra_run_opts('+RTS -I0')],
     compile_and_run, [''])

test('testmblockalloc',
     [c_src, only_ways(['normal','threaded1']), extra_run_opts('+RTS -I0')],
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
      # SIGBUS on OX X (PPC and x86 only; amd64 gives SEGV)
      # The output under OS X is too unstable to readily compare
      when(platform('i386-apple-darwin'), [ignore_stderr, exit_code(139)]),
      when(platform('x86_64-apple-darwin'), [ignore_stderr, exit_code(139)]),
      when(platform('aarch64-apple-darwin'), [ignore_stderr, exit_code(139)]),
      when(opsys('mingw32'), [ignore_stderr, exit_code(11)]),
      when(opsys('mingw32'), [fragile(18548)]),
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
      # Apparently the output can be different on different
      # Linux setups, so just ignore it. As long as we get
      # the right exit code we're OK.
      when(opsys('linux'), ignore_stderr),
      # PowerPC 64 bit and most likely PowerPC 32 bit processors
      # do not generate an exception (interrupt) for integer
      # division by zero but the result is undefined.
      # C programs compiled with gcc exit normally, so do we.
      when(platform('powerpc64-unknown-linux'), [ignore_stdout, exit_code(0)]),
      when(platform('powerpc64le-unknown-linux'), [ignore_stdout, exit_code(0)]),
      when(opsys('mingw32'), [ignore_stderr, exit_code(8)]),
      when(opsys('mingw32'), [fragile(18548)]),
      # The output under OS X is too unstable to readily compare
      when(platform('i386-apple-darwin'), [ignore_stderr, exit_code(136)]),
      when(platform('x86_64-apple-darwin'), [ignore_stderr, exit_code(136)]),
      # ThreadSanitizer changes the output
      when(have_thread_sanitizer(), skip),
      # since these test are supposed to crash the
      # profile report will be empty always.
      # so disable the check for profiling
      when(opsys('mingw32'), omit_ways(prof_ways))],
     compile_and_run, ['-with-rtsopts="--generate-stack-traces=no"'])

test('outofmem', [ when(opsys('darwin'), skip),
                   # this is believed to cause other processes to die
                   # that happen concurrently while the outofmem test
                   # runs in CI. As such we'll need to disable it on
                   # windows, to prevent absolute exhaustion of memory
                   # and subsequent termination (and failure) of unrelated
                   # tests.
                   when(opsys('mingw32'), skip) ],
                 makefile_test, ['outofmem'])
test('outofmem2', normal, makefile_test, ['outofmem2'])

test('T2047',
     [ignore_stdout,
      extra_run_opts('+RTS -c -RTS'),
      # Non-moving collector doesn't support -c
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc'])],
     compile_and_run, ['-package containers'])

# Blackhole-detection test.
# Skip GHCi due to #2786
test('T2783', [ omit_ways(['ghci']), exit_code(1)
              , fragile_for(2783, ['threaded1'])
              ], compile_and_run, [''])

# Test the work-stealing deque implementation.  We run this test in
# both threaded1 (-threaded -debug) and threaded2 (-threaded) ways.
test('testwsdeque', [extra_files(['../../../rts/WSDeque.h']),
                     unless(in_tree_compiler(), skip),
                    req_smp, # needs atomic 'cas'
                    c_src, only_ways(['threaded1', 'threaded2'])],
                    compile_and_run, [''])

test('T3236', [c_src, only_ways(['normal','threaded1']), exit_code(1)], compile_and_run, [''])

test('stack001', extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('stack002', [extra_files(['stack001.hs']),
                  extra_run_opts('+RTS -K32m -k4m -RTS')],
     compile_and_run, [''])

# run this test with very small stack chunks to exercise the stack
# overflow/underflow machinery.
test('stack003', [ omit_ways(['ghci']), # uses unboxed tuples
                   extra_run_opts('500000 +RTS -kc1k -kb100 -K96m -RTS') ],
                 compile_and_run, [''])

# Test that +RTS -K0 (e.g. no stack limit) parses correctly
test('stack004', [ extra_run_opts('+RTS -K0 -RTS')
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
test('rtsflags001', [ only_ways(['normal']), exit_code(1), extra_run_opts('+RTS -H0m -RTS') ], compile_and_run, [''])

# Crashed with 7.2 and earlier
test('rtsflags002', [ only_ways(['normal']) ], compile_and_run, ['-with-rtsopts="-B -B -B"'])

# omit dyn and profiling ways, because we don't build dyn_l or p_l
# variants of the RTS by default
test('traceEvent', [ omit_ways(['dyn', 'ghci'] + prof_ways),
                     extra_run_opts('+RTS -ls -RTS') ],
                   compile_and_run, ['-eventlog'])

test('traceBinaryEvent', [ omit_ways(['dyn', 'ghci'] + prof_ways),
                           extra_run_opts('+RTS -ls -RTS') ],
                         compile_and_run, ['-eventlog'])

# Test that -ol flag works as expected
test('EventlogOutput1',
     [ extra_files(["EventlogOutput.hs"]),
       omit_ways(['dyn', 'ghci'] + prof_ways) ],
     makefile_test, ['EventlogOutput1'])

# Test that -ol flag defaults to <program>.eventlog
test('EventlogOutput2',
     [ extra_files(["EventlogOutput.hs"]),
       omit_ways(['dyn', 'ghci'] + prof_ways) ],
     makefile_test, ['EventlogOutput2'])

test('T4059', [], makefile_test, ['T4059'])

# Test for #4274
test('exec_signals',
     [when(opsys('mingw32'), skip),
      pre_cmd('$MAKE -s --no-print-directory exec_signals-prep'),
      cmd_prefix('./exec_signals_prepare')],
     compile_and_run, [''])

test('return_mem_to_os', normal, compile_and_run, [''])

test('T4850', when(opsys('mingw32'), expect_broken(4850)), makefile_test, ['T4850'])

def config_T5250(name, opts):
    if not (config.arch in ['i386','x86_64']):
        opts.skip = 1;

test('T5250', [extra_files(['spalign.c']),
                config_T5250,
                # stack ptr is not 16-byte aligned on 32-bit Windows
                when(platform('i386-unknown-mingw32'), expect_fail),
                when(platform('i386-unknown-linux'),
                            expect_broken_for(4211,['llvm'])),
                extra_clean(['spalign.o']),
                omit_ways(['ghci']) ],
              compile_and_run, ['spalign.c'])

test('T5423', [], makefile_test, ['T5423'])

test('T5993', extra_run_opts('+RTS -k8 -RTS'), compile_and_run, [''])

test('T6006', [ omit_ways(prof_ways + ['ghci']),
                 extra_clean(['T6006_c.o']),
                 pre_cmd('$MAKE -s --no-print-directory T6006_setup') ],
                 # The T6006_setup hack is to ensure that we generate
                 # T6006_stub.h before compiling T6006_c.c, which
                 # needs it.
               compile_and_run, ['T6006_c.c -no-hs-main'])

test('T7037', [], makefile_test, ['T7037'])

test('T7087', exit_code(1), compile_and_run, [''])
test('T7160', omit_ways(['nonmoving_thr', 'nonmoving_thr_ghc']), compile_and_run, [''])

test('T7040', [omit_ways(['ghci'])], compile_and_run, ['T7040_c.c'])

test('T7040_ghci',
     [extra_files(['T7040_c.h']),
      only_ways(['ghci']),
      # Fragile when unregisterised; see #16085
      when(unregisterised(), skip),
      pre_cmd('$MAKE -s --no-print-directory T7040_ghci_setup')],
     compile_and_run, ['T7040_ghci_c.o'])

test('T7227', [extra_run_opts('+RTS -tT7227.stat --machine-readable -RTS')],
     compile_and_run, [''])

test('T7636', [ exit_code(1), extra_run_opts('100000') ], compile_and_run, [''] )

test('stablename001', expect_fail_for(['hpc']), compile_and_run, [''])
# hpc should fail this, because it tags every variable occurrence with
# a different tick.  It's probably a bug if it works, hence expect_fail.

test('T7815', [ multi_cpu_race,
                extra_run_opts('50000 +RTS -N2 -RTS'),
                req_smp,
                only_ways(['threaded1', 'threaded2']) ], compile_and_run, [''] )

# ignore_stderr because it contains a unique:
#   ffishutdown: Main_dul: interrupted
test('ffishutdown', [ignore_stderr, only_ways(['threaded1','threaded2'])],
     compile_and_run, [''])

# Times out in ghci way
test('T7919', [when(fast(), skip), omit_ways(['ghci'] + prof_ways)], compile_and_run,
     [config.ghc_th_way_flags])

test('T8035', normal, compile_and_run, [''])

test('T8209', [ req_smp, only_ways(threaded_ways), ignore_stdout ],
              compile_and_run, [''])

test('T8242', [ req_smp, only_ways(threaded_ways), ignore_stdout ],
              compile_and_run, [''])

test('T8124', [ only_ways(threaded_ways), omit_ways(['ghci']),
                 when(opsys('mingw32'), skip), # uses pthreads
                 extra_clean(['T8124_c.o']),
                 pre_cmd('$MAKE -s --no-print-directory T8124_setup') ],
                 # The T8124_setup hack is to ensure that we generate
                 # T8124_stub.h before compiling T8124_c.c, which
                 # needs it.
               compile_and_run, ['T8124_c.c -no-hs-main'])

# +RTS -A8k makes it fail faster
# The ghci way gets confused by the RTS options
test('T9045', [ omit_ways(['ghci']), extra_run_opts('10000 +RTS -A8k -RTS') ], compile_and_run, [''])

# T9078 needs to be compiled with -debug, which threaded1 does for us.
# Assert failure is reproducible with ghc-7.8.2.
test('T9078', only_ways(['threaded1']), compile_and_run, [''])

test('T10017', [ when(opsys('mingw32'), skip)
               , when(unregisterised(), skip)
               , req_smp
               , only_ways(threaded_ways), extra_run_opts('+RTS -N2 -RTS') ], compile_and_run, [''])

test('T11108', normal, compile_and_run, [''])

test('GcStaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])
test('ListStaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])

# 251 = RTS exit code for "out of memory"
test('overflow1', [ exit_code(251), when(wordsize(32), expect_broken(15255)) ],
     compile_and_run, [''])
test('overflow2', [ exit_code(251) ], compile_and_run, [''])
test('overflow3', [ exit_code(251) ], compile_and_run, [''])

def grep_stderr(pattern):
    def wrapper(cmd, pattern=pattern):
        swap12 = '3>&1 1>&2 2>&3 3>&-' # Swap file descriptors 1 and 2.
        return('{cmd} {swap12} | grep "{pattern}" {swap12}'.format(**locals()))
    return cmd_wrapper(wrapper)

# The ghci way gets confused by the RTS options
test('T9839_01',
     [omit_ways(['ghci']), extra_run_opts('+RTS -T-s'), no_check_hp,
      grep_stderr('given an argument when none was expected')],
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
test('T10590', [ignore_stderr, when(opsys('mingw32'), skip)], compile_and_run, [''])

# 20000 was easily enough to trigger the bug with 7.10
test('T10904', [ omit_ways(['ghci']), extra_run_opts('20000') ],
               compile_and_run, ['T10904lib.c'])

test('T10728', [extra_run_opts('+RTS -maxN3 -RTS'), only_ways(['threaded2'])],
               compile_and_run, [''])

test('T9405', [when(msys(), expect_broken(12714))], makefile_test, ['T9405'])

test('T11788', when(ghc_dynamic(), skip),
              makefile_test, ['T11788'])

test('T10296a', [req_smp], makefile_test, ['T10296a'])

test('T10296b', [only_ways(['threaded2'])], compile_and_run, [''])

test('numa001', [ extra_run_opts('8'), unless(unregisterised(), extra_ways(['debug_numa'])) ]
                , compile_and_run, [''])

test('T12497', [ unless(opsys('mingw32'), skip)
               ],
               makefile_test, ['T12497'])

test('T13617', [ unless(opsys('mingw32'), skip)],
               makefile_test, ['T13617'])

# This test sometimes produces out of sequence samples in the profasm way, but
# not reliably, so we just skip it. See ticket #15065.
# Test is being skipped on darwin due to it's flakiness.
test('T12903', [ when(opsys('mingw32'), skip)
               , when(opsys('darwin'), skip)
               , omit_ways(['ghci', 'profasm'])]
               , compile_and_run, [''])

test('T13832', exit_code(1), compile_and_run, ['-threaded'])
test('T13894', normal, compile_and_run, [''])
# this test fails with the profasm way on some machines but not others,
# so we just skip it.
test('T14497', [omit_ways(['profasm']), multi_cpu_race], compile_and_run, ['-O'])
test('T14695', [normal, ignore_stderr], makefile_test, ['T14695'])
test('T14702', [ ignore_stdout
               , when(unregisterised(), skip)
               , when(opsys('mingw32'), fragile(18953))
               , only_ways(['threaded1', 'threaded2'])
               , extra_run_opts('+RTS -A32m -N8 -T -RTS')
               ]
               , compile_and_run, [''])

test('T14900', normal, compile_and_run, ['-package ghc-compact'])
test('InternalCounters', normal, makefile_test, ['InternalCounters'])
test('alloccounter1', normal, compile_and_run,
  [
    # avoid allocating stack chunks, which counts as
    # allocation and messes up the results:
    '-with-rtsopts=-k1m'
  ])

test('nursery-chunks1',
  [ extra_run_opts('4 100 +RTS -n32k -A1m -RTS')
  , req_smp
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
    when(opsys('freebsd'), expect_broken(16035)),
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
    when(opsys('freebsd'), expect_broken(16035)),
    req_rts_linker
  ],
  makefile_test, ['KeepCafs'])

test('T16514', unless(opsys('mingw32'), skip), compile_and_run, ['T16514_c.cpp -lstdc++'])
test('test-zeroongc', extra_run_opts('-DZ'), compile_and_run, ['-debug'])

test('T13676',
     [when(opsys('mingw32'), expect_broken(17447)),
      extra_files(['T13676.hs'])],
     ghci_script, ['T13676.script'])
test('InitEventLogging',
     [only_ways(['normal']), extra_run_opts('+RTS -RTS')],
     compile_and_run, ['-eventlog InitEventLogging_c.c'])
test('RestartEventLogging',
     [only_ways(['threaded1','threaded2']), extra_run_opts('+RTS -la -RTS')],
     compile_and_run, ['-eventlog RestartEventLogging_c.c'])

test('T17088',
     [only_ways(['normal']), extra_run_opts('+RTS -c -A256k -RTS')],
     compile_and_run, ['-rtsopts -O2'])

test('T15427', normal, compile_and_run, [''])

test('T19481', extra_run_opts('+RTS -T  -RTS'), compile_and_run, [''])
test('T19381', extra_run_opts('+RTS -T -RTS'), compile_and_run, [''])

test('ipeMap', [c_src], compile_and_run, [''])
