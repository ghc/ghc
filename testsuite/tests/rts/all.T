test('testblockalloc',
     [c_src, only_ways(['normal','threaded1']), extra_run_opts('+RTS -I0')], 
     compile_and_run, [''])


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010', normal, compile_and_run, ['+RTS -c -RTS'])
test('derefnull',
     [when(opsys('mingw32'), expect_broken(6079)),
      # LLVM Optimiser considers dereference of a null pointer
      # undefined and marks the code as unreachable which means
      # that later optimisations remove it altogether.
      omit_ways(['optllvm']),
      # SIGSEGV on Linux (which we make the default)
      exit_code(139),
      # Apparently the output can be different on different
      # Linux setups, so just ignore it. As long as we get
      # the right exit code we're OK.
      when(opsys('linux'), ignore_output),
      # SIGBUS on OX X (PPC and x86 only; amd64 gives SEGV)
      when(platform('i386-apple-darwin'), exit_code(138)),
      when(platform('powerpc-apple-darwin'), exit_code(138)),
      when(opsys('mingw32'), exit_code(1))],
     compile_and_run, [''])
test('divbyzero',
     [when(opsys('mingw32'), expect_broken(6079)),
      # SIGFPE on Linux
      exit_code(136),
      # Apparently the output can be different on different
      # Linux setups, so just ignore it. As long as we get
      # the right exit code we're OK.
      when(opsys('linux'), ignore_output),
      when(opsys('mingw32'), exit_code(1))],
     compile_and_run, [''])

test('outofmem', when(opsys('darwin'), skip), 
                 run_command, ['$MAKE -s --no-print-directory outofmem'])
test('outofmem2', extra_run_opts('+RTS -M5m -RTS'),
                  run_command, ['$MAKE -s --no-print-directory outofmem2'])

test('T2047', [ignore_output, extra_run_opts('+RTS -c -RTS')],
              compile_and_run, ['-package containers'])

# Blackhole-detection test.
# Skip GHCi due to #2786
test('T2783', [ omit_ways(['ghci']), exit_code(1) ], compile_and_run, [''])

# Test the work-stealing deque implementation.  We run this test in
# both threaded1 (-threaded -debug) and threaded2 (-threaded) ways.
test('testwsdeque', [unless(in_tree_compiler(), skip),
                    c_src, only_ways(['threaded1', 'threaded2'])],
                    compile_and_run, ['-I../../../rts'])

test('T3236', [c_src, only_ways(['normal','threaded1']), exit_code(1)], compile_and_run, [''])

test('stack001', extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('stack002', extra_run_opts('+RTS -K32m -k4m -RTS'), compile_and_run, [''])

# run this test with very small stack chunks to exercise the stack
# overflow/underflow machinery.
test('stack003', [ omit_ways('ghci'), # uses unboxed tuples
                   extra_run_opts('500000 +RTS -kc1k -kb100 -K96m -RTS') ],
                 compile_and_run, [''])

test('atomicinc', [ c_src, only_ways(['normal','threaded1', 'threaded2']) ], compile_and_run, [''])

test('T3424', # it's slow:
              [ when(fast(), skip), only_ways(['normal','threaded1','ghci']) ],
              compile_and_run, [''])

# Test for out-of-range heap size
test('rtsflags001', [ only_ways(['normal']), exit_code(1), extra_run_opts('+RTS -H0m -RTS') ], compile_and_run, [''])

# Crashed with 7.2 and earlier
test('rtsflags002', [ only_ways(['normal']) ], compile_and_run, ['-with-rtsopts="-B -B -B"'])

# Test to see if linker scripts link properly to real ELF files
test('T2615',
     [ when(opsys('mingw32'), skip),
       # OS X doesn't seem to support linker scripts
       when(opsys('darwin'), skip),
       # Solaris' linker does not support GNUish linker scripts
       when(opsys('solaris2'), skip),
       pre_cmd('$MAKE -s --no-print-directory T2615-prep'),
       # Add current directory to dlopen search path
       cmd_prefix('LD_LIBRARY_PATH=$LD_LIBRARY_PATH:. '),
       extra_clean(['libfoo_T2615.so', 'libfoo_T2615.o'])],
     compile_and_run,
     ['-package ghc'])

# omit dyn and profiling ways, because we don't build dyn_l or p_l
# variants of the RTS by default
test('traceEvent', [ omit_ways(['dyn'] + prof_ways), 
                     extra_run_opts('+RTS -ls -RTS') ], 
                   compile_and_run, ['-eventlog'])

test('T4059',
     extra_clean(['T4059_c.o']),
     run_command,
     ['$MAKE -s --no-print-directory T4059'])

# Test for #4274
test('exec_signals', [
     when(opsys('mingw32'), skip),
     pre_cmd('$MAKE -s --no-print-directory exec_signals-prep'),
     cmd_prefix('./exec_signals_prepare'),
     extra_clean(['exec_signals_child', 'exec_signals_prepare'])
    ], compile_and_run, [''])

test('return_mem_to_os', normal, compile_and_run, [''])

test('T4850', when(opsys('mingw32'), expect_broken(4850)), run_command, ['$MAKE -s --no-print-directory T4850'])

def config_T5250(name, opts):
    if not (config.arch in ['i386','x86_64']):
        opts.skip = 1;

test('T5250', [ config_T5250,
                # stack ptr is not 16-byte aligned on 32-bit Windows
                when(platform('i386-unknown-mingw32'), expect_fail),
                when(platform('i386-unknown-linux'),
                            expect_broken_for(4211,['llvm'])),
                extra_clean(['spalign.o']),
                omit_ways(['ghci']) ],
              compile_and_run, ['spalign.c'])

test('T5423',
     extra_clean(['T5423_cmm.o']),
     run_command,
     ['$MAKE -s --no-print-directory T5423'])

test('T5435_v',
     extra_clean(['T5435_c_v.o']),
     run_command,
     ['$MAKE -s --no-print-directory T5435_v'])

test('T5435_dyn',
     extra_clean(['T5435_c_dyn.o']),
     run_command,
     ['$MAKE -s --no-print-directory T5435_dyn'])

test('T5993', extra_run_opts('+RTS -k8 -RTS'), compile_and_run, [''])

test('T6006', [ omit_ways(prof_ways + ['ghci']),
                 extra_clean(['T6006_c.o']),
                 pre_cmd('$MAKE -s --no-print-directory T6006_setup') ],
                 # The T6006_setup hack is to ensure that we generate
                 # T6006_stub.h before compiling T6006_c.c, which
                 # needs it.
               compile_and_run, ['T6006_c.c -no-hs-main'])

test('T7037',
     [ extra_clean(['T7037_main.o','T7037_main']) ],
     run_command,
     ['$MAKE -s --no-print-directory T7037'])

test('T7087', exit_code(1), compile_and_run, [''])
test('T7160', normal, compile_and_run, [''])

test('T7040', [ extra_clean(['T7040_c.o']), omit_ways(['ghci']) ],
     compile_and_run, ['T7040_c.c'])

test('T7040_ghci', [ only_ways(['ghci']),
                      pre_cmd('$MAKE -s --no-print-directory T7040_ghci_setup'),
                      extra_clean(['T7040_ghci_c.o']) ],
                    compile_and_run, ['T7040_ghci_c.o'])

test('T7227', [ extra_run_opts('+RTS -tT7227.stat --machine-readable -RTS'),
                extra_clean(['T7227.stat']) ]
            , compile_and_run, [''] )

test('T7636', [ exit_code(1), extra_run_opts('100000') ], compile_and_run, [''] )

test('stablename001', expect_fail_for(['hpc']), compile_and_run, [''])
# hpc should fail this, because it tags every variable occurrence with
# a different tick.  It's probably a bug if it works, hence expect_fail.

test('T7815', [ multi_cpu_race,
                extra_run_opts('50000 +RTS -N2 -RTS'),
                only_ways(['threaded1', 'threaded2']) ], compile_and_run, [''] )

# ignore_output because it contains a unique:
#   ffishutdown: Main_dul: interrupted
test('ffishutdown', [ ignore_output, only_ways(['threaded1','threaded2']) ], compile_and_run, [''])

test('T7919', [extra_clean(['T7919A.o','T7919A.hi',
                            'T7919A.dyn_o','T7919A.dyn_hi']),
               when(fast(),skip) ],
             compile_and_run, [''])

test('T8035', normal, compile_and_run, [''])

test('linker_unload',
     [ extra_clean(['Test.o','Test.hi', 'linker_unload']), when(opsys('mingw32'), expect_broken(8292)), expect_broken(8291) ],
     run_command,
     ['$MAKE -s --no-print-directory linker_unload'])

test('T8209', [ only_ways(threaded_ways), ignore_output ],
              compile_and_run, [''])

test('T8242', [ when(opsys('mingw32'), expect_broken(8242)), only_ways(threaded_ways), ignore_output ],
              compile_and_run, [''])
