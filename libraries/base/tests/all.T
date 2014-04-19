
test('readFloat', exit_code(1), compile_and_run, [''])
test('enumDouble', normal, compile_and_run, [''])
test('enumRatio', normal, compile_and_run, [''])
test('tempfiles', normal, compile_and_run, [''])
test('fixed', normal, compile_and_run, [''])
test('quotOverflow', normal, compile_and_run, [''])
test('assert', exit_code(1), compile_and_run, ['-fno-ignore-asserts'])
test('CPUTime001', normal, compile_and_run, [''])
test('readLitChar',   normal, compile_and_run, [''])
test('unicode001',
     when(platform('i386-unknown-openbsd'), expect_fail),
     compile_and_run,
     [''])
test('unicode002',
     when(platform('i386-unknown-openbsd'), expect_fail),
     compile_and_run,
     [''])
test('data-fixed-show-read', normal, compile_and_run, [''])
test('showDouble', normal, compile_and_run, [''])
test('readDouble001', normal, compile_and_run, [''])
test('readInteger001', normal, compile_and_run, [''])
test('readFixed001', normal, compile_and_run, [''])
test('lex001', normal, compile_and_run, [''])
test('take001', extra_run_opts('1'), compile_and_run, [''])
test('genericNegative001', extra_run_opts('-1'), compile_and_run, [''])
test('ix001', normal, compile_and_run, [''])

# need to add -K64m to the compiler opts, so that GHCi gets it too
test('ioref001',
     [when(fast(), skip),extra_run_opts('+RTS -K64m -RTS')],
     compile_and_run,
     ['+RTS -K64m -RTS'])

test('echo001', set_stdin("echo001.hs"), compile_and_run, [''])

test('hTell001', normal, compile_and_run, [''])

test('hTell002', normal, compile_and_run, [''])

test('performGC001', normal, compile_and_run, [''])

# optimisation screws up this test because some of the traces get commoned up
test('trace001', normal, compile_and_run, [''])

test('hGetBuf002', normal, compile_and_run, [''])
test('hGetBuf003', normal, compile_and_run, [''])
test('hPutBuf001', normal, compile_and_run, [''])
test('hPutBuf002', extra_clean(['hPutBuf002.out']), compile_and_run, [''])

test('char001', 	normal, compile_and_run, [''])
test('char002', 	normal, compile_and_run, [''])

test('cstring001', 	normal, compile_and_run, [''])

test('length001',
     # length001 depends on a RULE being applied, and without -O takes
     # excessive amounts of stack space. So we specifically set a low
     # stack limit and mark it as failing under a few conditions.
     [extra_run_opts('+RTS -K8m -RTS'),
     expect_fail_for(['normal', 'threaded1', 'llvm'])],
     compile_and_run, [''])

test('ratio001', 	normal, compile_and_run, [''])

test('rand001',		reqlib('random'), compile_and_run, [''])
test('reads001',		normal, compile_and_run, [''])
test('show001',		normal, compile_and_run, [''])
test('text001',		normal, compile_and_run, [''])

test('tup001',		normal, compile_and_run, [''])

test('addr001',		normal, compile_and_run, [''])
test('dynamic001',      normal, compile_and_run, [''])
test('dynamic002',      normal, compile_and_run, [''])
test('dynamic003',      extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('dynamic004',      omit_ways(['normal', 'threaded1', 'ghci']), compile_and_run, [''])
test('dynamic005',      normal, compile_and_run, [''])
test('enum01',          when(fast(), skip), compile_and_run, ['-cpp'])
test('enum02', 		when(fast(), skip), compile_and_run, ['-cpp'])
test('enum03', 		when(fast(), skip), compile_and_run, ['-cpp'])
test('enum04', 		normal, compile_and_run, [''])
test('exceptionsrun001', 	normal, compile_and_run, [''])
test('exceptionsrun002', 	normal, compile_and_run, [''])
test('list001'	,	when(fast(), skip), compile_and_run, [''])
test('list002', when(fast(), skip), compile_and_run, [''])
test('list003', when(fast(), skip), compile_and_run, [''])

test('memo001',
     [extra_run_opts('+RTS -A10k -RTS'),
      extra_clean(['Memo1.hi', 'Memo1.o'])],
     multimod_compile_and_run,
     ['memo001',''])

test('memo002',
     [extra_run_opts('20'),
      extra_clean(['Memo2.hi', 'Memo2.o'])],
	multimod_compile_and_run, ['memo002',''])

test('packedstring001', reqlib('packedstring'), compile_and_run, ['-package packedstring'])

test('stableptr001',
     [when(fast(), skip), extra_run_opts('+RTS -K8m -RTS')],
     compile_and_run, [''])
test('stableptr003', normal, compile_and_run, [''])
test('stableptr004', extra_run_opts('+RTS -K4m -RTS'), compile_and_run, [''])
test('stableptr005', normal, compile_and_run, [''])

test('weak001', normal, compile_and_run, [''])

# In the 65001 codepage, we can't even cat the expected output on msys:
#     $ cat 4006.stdout
#     It works here
#     cat: write error: Permission denied
# Seems to be a known problem, e.g.
#     http://mingw-users.1079350.n2.nabble.com/Bug-re-Unicode-on-the-console-td3121717.html
test('T4006', when(msys(), expect_fail), compile_and_run, [''])

test('T5943', normal, compile_and_run, [''])
test('T5962', normal, compile_and_run, [''])
test('T7034', normal, compile_and_run, [''])

test('qsem001', normal, compile_and_run, [''])
test('qsemn001', normal, compile_and_run, [''])

test('T7457', normal, compile_and_run, [''])

test('T7773', when(opsys('mingw32'), skip), compile_and_run, [''])
# Andreas says that T7773 will not (and should not) work on Windows

# Tests for kind-polymorphic Category
test('CatPairs', normal, compile, [''])
test('CatEntail', normal, compile, [''])

test('T7653', normal, compile_and_run, [''])
test('T7787', normal, compile_and_run, [''])

test('topHandler01', when(opsys('mingw32'), skip), compile_and_run, [''])
test('topHandler02',
     [when(opsys('mingw32'), skip),
      omit_ways(['ghci']),
      # Irritatingly, the test driver calls the programs via a shell, and
      # depending on the shell, they can add their own "helpful" commentary,
      # pretty printing the name of the signal that killed the process. So we
      # ignore the stdout here, we only care about the exit code (which itself
      # is messed up because of the shell, using 128+sig encoding)
      ignore_output,
      exit_code(130)  # actually signal 2 SIGINT
     ], compile_and_run, [''])
test('topHandler03',
     [when(opsys('mingw32'), skip),
      # As above, shells, grrr.
      ignore_output,
      exit_code(143)  # actually signal 15 SIGTERM
     ], compile_and_run, [''])


test('T8766',
        [ stats_num_field('bytes allocated', (16828144, 5)),
	# with GHC-7.6.3: 83937384 (but faster execution than the next line)
	# before:         58771216 (without call-arity-analysis)
	# expected value: 16828144 (2014-01-14)
	only_ways(['normal'])],
      compile_and_run,
      ['-O'])
