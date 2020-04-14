
# Tests for pattern match checker (coverage and exhaustiveness)

# Just do the normal way...
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Bug reports / feature requests
test('T2006', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T2204', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3078', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T322', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T366', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927a', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927b', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T4139', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T6124', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T7669a', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T8970', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951b', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11303', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11276', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11303b', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11374', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11822', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11195', collect_compiler_stats('bytes allocated',10), compile,
     ['-package ghc -fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M2G -RTS'])
test('T11984', normal, compile,
    ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14086', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14098', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14813', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14899', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15305', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15385', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15450', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15584', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15713', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15753a', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15753b', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15753c', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15753d', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15884', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T16289', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17096', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M2G -RTS'])
test('T17112', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17207', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17207b', expect_broken(17207), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17208', expect_broken(17208), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17215', expect_broken(17215), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17216', expect_broken(17216), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17218', expect_broken(17218), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17219', expect_broken(17219), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17234', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17248', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17357', expect_broken(17357), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17376', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17465', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17646', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17703', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17783', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17977', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T17977b', collect_compiler_stats('bytes allocated',10), compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T18049', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])

# Other tests
test('pmc001', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc002', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc003', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc004', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc005', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc006', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc007', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc008', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc009', [], compile,
     ['-package ghc -fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11245', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11336b', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T12949', [],  compile, ['-fwarn-overlapping-patterns'])
test('T12957', [], compile, ['-fwarn-overlapping-patterns'])
test('T12957a', [], compile, ['-fwarn-overlapping-patterns'])
test('PmExprVars', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('CyclicSubst', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('CaseOfKnownCon', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('TooManyDeltas', [], compile,
     ['-fmax-pmcheck-models=0 -fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('LongDistanceInfo', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])

# Series (inspired) by Luke Maranget

test('PmSeriesS',
     [ collect_compiler_stats('bytes allocated',10),
       pre_cmd('$PYTHON ./genS.py 10'),
       extra_files(['genS.py']),
     ],
     multimod_compile,
     ['S', '-v0'])
test('PmSeriesT',
     [ collect_compiler_stats('bytes allocated',10),
       pre_cmd('$PYTHON ./genT.py 10'),
       extra_files(['genT.py']),
     ],
     multimod_compile,
     ['T', '-v0'])
test('PmSeriesV',
     [ collect_compiler_stats('bytes allocated',10),
       pre_cmd('$PYTHON ./genV.py 6'),
       extra_files(['genV.py']),
     ],
     multimod_compile,
     ['V', '-v0'])
test('PmSeriesG',
     [ collect_compiler_stats('bytes allocated',10),
       pre_cmd('./genG 20'),
       extra_files(['genG']),
     ],
     multimod_compile,
     ['G', '-v0'])

# EmptyCase
test('T10746', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase001', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase002', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase003', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase004', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase005', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase006', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase007', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase008', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase009', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase010', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
