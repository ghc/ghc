# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('arith001', normal, compile_and_run, [''])
test('arith002', normal, compile_and_run, [''])
test('arith003', normal, compile_and_run, [''])
test('arith004', normal, compile_and_run, [''])
test('arith005', if_platform('i386-apple-darwin', expect_broken_for(7043, 'ghci')), compile_and_run, [''])
test('arith006', normal, compile_and_run, [''])
test('arith007', normal, compile_and_run, [''])

# On i386, we need -msse2 to get reliable floating point results
ways = normal
opts = ''
if config.platform.startswith('i386-'):
   if config.compiler_type == 'ghc' and \
      version_ge(config.compiler_version, '6.13'):
	opts = '-msse2'
   else:
	ways = expect_fail_for(['optasm','threaded2','hpc','dyn','profasm'])

test('arith008', ways, compile_and_run, [opts])

test('arith009', normal, compile_and_run, [''])
test('arith010', normal, compile_and_run, [''])
test('arith011', normal, compile_and_run, [''])

test('arith012', ways, compile_and_run, [opts])

test('arith013', compose(normal,only_compiler_types(['ghc'])), compile_and_run, [''])
test('arith014', normal, compile_and_run, [''])
test('arith015', normal, compile_and_run, [''])
test('numrun009', normal, compile_and_run, [''])
test('numrun010', normal, compile_and_run, [''])
test('numrun011', normal, compile_and_run, [''])
test('numrun012',
        if_os('mingw32',expect_fail_for('ghci')),
        # on Windows, GHCi says that "logBase 2 (2^31)" is "31.0", but
        # other platforms (and compiled on Windows) reports 31.000000000000004
        # I have no idea where the discrepancy comes from.  --SDM
        compile_and_run, [''])
test('numrun013', normal, compile_and_run, [''])
test('numrun014', normal, compile_and_run, [''])
test('arith016', compose(normal,only_compiler_types(['ghc'])), compile_and_run, [''])
test('arith017', normal, compile_and_run, [''])
test('arith018', normal, compile_and_run, [''])
test('arith019', normal, compile_and_run, [''])
test('expfloat', normal, compile_and_run, [''])

test('T1603', skip, compile_and_run, [''])
test('T3676', expect_broken(3676), compile_and_run, [''])
test('T4381', normal, compile_and_run, [''])
test('T4383', normal, compile_and_run, [''])

test('add2', normal, compile_and_run, ['-fobject-code'])
test('mul2', normal, compile_and_run, ['-fobject-code'])
test('quotRem2', normal, compile_and_run, ['-fobject-code'])
test('T5863', normal, compile_and_run, [''])

test('T7014',
     extra_clean(['T7014.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory T7014'])

test('T7233', normal, compile_and_run, [''])
