# T12791 and T5835 test that GHC uses top-level instances in places where using
# a locally given solution would produce worse code.
# See Note [Solving from instances when interacting Dicts]

test('T5835',
     [ comparison('max_bytes_used',10)
     , only_ways(['normal'])
       ],
     compile_and_run,
     ['-O'])

test('T12791',
     [ comparison('max_bytes_used',10)
     , only_ways(['normal'])
       ],
     compile_and_run,
     ['-O'])

# Tests that newArray/newArray_ is being optimised correctly

test('T10359',
     [comparison('bytes allocated',5)
     , only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

# fortunately the values here are mostly independent of the wordsize,
# because the test allocates an unboxed array of doubles.
test('T3586',
     [ comparison('peak_megabytes_allocated',17),
      comparison('bytes allocated',5),
      only_ways(['normal']),

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      # Only 64-bit as we don't have a good 32-bit test environment at the moment
      when(wordsize(64), extra_hc_opts('+RTS -G1 -RTS'))
      ],
     compile_and_run,
     ['-O'])

test('T4830',
     [ comparison('bytes allocated',4),
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
      comparison(['peak_megabytes_allocated','bytes allocated'],5),
      only_ways(['normal']),
      extra_run_opts('arith011.stdout'),
      ignore_stdout,

      # Use `+RTS -G1` for more stable residency measurements. Note [residency].
      # Only 64-bit as we don't have a good 32-bit test environment at the moment
      when(wordsize(64), extra_hc_opts('+RTS -G1 -RTS'))
      ],
     # use a suitably big file, without bloating the repo with a new one:
     compile_and_run,
     ['-O'])

test('T876',
     [ comparison('bytes allocated',5),
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

test('T3736', [], run_command, ['$MAKE -s --no-print-directory T3736'])
test('T3738',
     [extra_clean(['T3738a.hi', 'T3738a.o']),
       comparison(['peak_megabytes_allocated','bytes allocated'],5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('MethSharing',
     [comparison(['peak_megabytes_allocated','bytes allocated'],5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T2902', [], run_command, ['$MAKE -s --no-print-directory T2902'])
test('T149',
     [ # expect_broken(149),
       # working (2 Jul 2013, x86-64/Linux)
      extra_clean(['T149_A',    'T149_B',
                   'T149_A.hi', 'T149_B.hi',
                   'T149_A.o',  'T149_B.o'])],
     run_command,
     ['$MAKE -s --no-print-directory T149'])

test('T5113',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])


test('T4978',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O2'])

test('T5205',
     [ comparison('bytes allocated',5),
      only_ways(['normal', 'optasm'])
      ],
     compile_and_run,
     [''])

test('T5549',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T4474a',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474b',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
test('T4474c',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T5237',
     [ comparison('bytes allocated',5),
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O ' + sse2_opts])

test('T5536',
     [ comparison('bytes allocated',5),
     extra_clean(['T5536.data']),
     ignore_stdout,
     only_ways(['normal'])
     ],
    compile_and_run,
    ['-O'])

test('T7257',
     [ comparison('bytes allocated',10),
      comparison('peak_megabytes_allocated',10),
      only_ways(['normal'])
     ],
    compile_and_run, ['-O'])

test('Conversions',
     [ comparison('bytes allocated',5),    
      only_ways(['normal'])
     ],
    compile_and_run, ['-O -msse2'])

test('T7507', omit_ways(['ghci']), compile_and_run, ['-O'])
# For 7507, stack overflow is the bad case

test('T7436',
     [ comparison('max_bytes_used',4),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7797',
      [ comparison('bytes allocated',5),
      extra_clean(['T7797a.hi', 'T7797a.o']),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7954',
      [ comparison('bytes allocated',5),
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

test('T7850',
     [ comparison('peak_megabytes_allocated',10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T5949',
     [ comparison('bytes allocated',10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T4267',
     [ comparison('bytes allocated',10),
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('T7619',
     [ comparison('bytes allocated',10),    
      only_ways(['normal'])],
     compile_and_run,
     ['-O'])

test('InlineArrayAlloc',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineByteArrayAlloc',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('InlineCloneArrayAlloc',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9203',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T9339',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2 -fspec-constr-keen'])
     # For the -fspec-constr-keen see Note [Making SpecConstr keener] in SpecConstr


test('T8472',
     [ comparison('bytes allocated',80),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12996',
     [ comparison('bytes allocated',5),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T13001',
     [ comparison('bytes allocated',20),
      only_ways(['normal'])],
     compile_and_run,
     ['-O2'])

test('T12990',
    [ comparison('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])

test('T13218',
    [ comparison('bytes allocated',5),
     comparison('max_bytes_used',10),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNull',
    [ comparison('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O'])

test('DeriveNullTermination', normal, compile_and_run, [''])

test('T13623',
    [ comparison('bytes allocated',5),
     only_ways(['normal'])],
    compile_and_run,
    ['-O2'])
