# T12791 and T5835 test that GHC uses top-level instances in places where using
# a locally given solution would produce worse code.
# See Note [Solving from instances when interacting Dicts]

test('T5835',
     [collect_runtime_residency(10),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T12791',
     [collect_runtime_residency(10),
      only_ways(['normal'])
       ],
     compile_and_run,
     ['-O'])

# Tests that newArray/newArray_ is being optimised correctly

test('T10359',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T14955',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     multimod_compile_and_run,
     ['T14955', '-O'])

# fortunately the values here are mostly independent of the wordsize,
# because the test allocates an unboxed array of doubles.
test('T3586',
     [collect_runtime_residency(2),
      collect_stats('bytes allocated', 5),
      only_ways(['normal']),
      ],
     compile_and_run,
     ['-O'])

test('T4830',
     [collect_stats('bytes allocated',4),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T3245', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, ['-O'])

# Test that bytestring reading/writing isn't over-allocating.  We had
# a bug in hGetBufNonBlocking in 6.13 that triggered this.
#
test('lazy-bs-alloc',
     [extra_files(['../../numeric/should_run/arith011.stdout']),
      collect_stats('bytes allocated',5),
      collect_runtime_residency(1),
      only_ways(['normal']),
      extra_run_opts('arith011.stdout'),
      ignore_stdout,
      ],
     # use a suitably big file, without bloating the repo with a new one:
     compile_and_run,
     ['-O'])

test('T876',
     [collect_stats('bytes allocated',5),
      only_ways(['normal']),
      extra_run_opts('10000')
      ],
     compile_and_run,
     ['-O'])

# Get reproducible floating-point results on x86
if config.arch == 'i386':
   sse2_opts = '-msse2'
else:
   sse2_opts = ''

test('T4321',
     omit_ways(['ghci']),
     compile_and_run, ['-O ' + sse2_opts])

test('T3736', [], makefile_test, ['T3736'])
test('T3738',
     [extra_clean(['T3738a.hi', 'T3738a.o']),
      collect_runtime_residency(1),
      collect_stats('bytes allocated',8),
      only_ways(['normal']),
      ],
     compile_and_run,
     ['-O'])

test('MethSharing',
     [collect_runtime_residency(1),
      collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T2902', [], makefile_test, ['T2902'])
test('T149',
     [ # expect_broken(149),
       # working (2 Jul 2013, x86-64/Linux)
      extra_clean(['T149_A',    'T149_B',
                   'T149_A.hi', 'T149_B.hi',
                   'T149_A.o',  'T149_B.o'])],
     makefile_test, ['T149'])

test('T5113',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])


test('T4978',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T5205',
     [collect_stats('bytes allocated',5),
      only_ways(['normal', 'optasm'])
      ],
     compile_and_run,
     [''])

test('T5549',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4474a',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474b',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474c',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T5237',
     [collect_stats('bytes allocated',5),
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O ' + sse2_opts])

test('T5536',
     [collect_stats('bytes allocated',1),
     extra_clean(['T5536.data']),
     ignore_stdout,
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O'])

test('T7257',
     [collect_stats('bytes allocated',10),
      collect_runtime_residency(5),
      only_ways(['normal'])
     ],
    compile_and_run, ['-O'])

test('Conversions',
     [collect_stats('bytes allocated',3),
      only_ways(['normal'])
     ],
    compile_and_run, ['-O -msse2'])

test('T7507', omit_ways(['ghci']), compile_and_run, ['-O'])
# For 7507, stack overflow is the bad case

test('T7436',
     [collect_runtime_residency(4),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7436b',
     [collect_runtime_residency(4),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7797',
      [collect_stats('bytes allocated',5),
      extra_clean(['T7797a.hi', 'T7797a.o']),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7954',
      [collect_stats('bytes allocated',10),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7850',
     [collect_runtime_residency(10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T5949',
     [collect_stats('bytes allocated',10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T4267',
     [collect_stats('bytes allocated',10),
      only_ways(['normal'])],
      compile_and_run,
      ['-O'])

test('T7619',
     [collect_stats('bytes allocated',10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('InlineArrayAlloc',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineByteArrayAlloc',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineCloneArrayAlloc',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9203',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9339',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2 -fspec-constr-keen'])
     # For the -fspec-constr-keen see Note [Making SpecConstr keener] in GHC.Core.Op.SpecConstr


test('T8472',
     [collect_stats('bytes allocated',80),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12996',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T13001',
     [collect_stats('bytes allocated',20),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T8763',
     [collect_stats('bytes allocated', 20),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12990',
    [collect_stats('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T13218',
    [collect_stats('bytes allocated',5),
     collect_runtime_residency(10),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNull',
    [collect_stats('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNullTermination', normal, compile_and_run, [''])

test('T13623',
    [collect_stats('bytes allocated',10),
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T14052',
     [collect_compiler_stats('bytes allocated',15)],
     ghci_script,
     ['T14052.script'])

test('T14936',
     [collect_stats('bytes allocated',10)],
     compile_and_run,
     ['-O2'])

test('T15226',
    [collect_stats('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('T15226a',
    [collect_stats('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('T15263',
    [collect_stats('bytes allocated', 4),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('T15426',
    [collect_stats('bytes allocated', 20),
    only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T15578',
    [collect_stats('bytes allocated', 5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T15185',
    [collect_stats('bytes allocated', 5), only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('T17123',
    [collect_stats('bytes allocated', 20), only_ways(['normal'])],
    compile_and_run,
    ['-O'])

# Test performance of creating Uniques.
test('UniqLoop',
     [collect_stats('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O -package ghc'])

test('T18574',
    [collect_stats('bytes allocated', 5), only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('T19347',
    [collect_stats('bytes allocated', 5), only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test ('T19474',
      [ collect_stats('bytes allocated', 5), reqlib('vector') ],
      compile_and_run,
      ['-v0 -O'])

test('IPEPerf',[collect_stats(), only_ways(['normal'])],
     compile_and_run, ['-finfo-table-map'])
