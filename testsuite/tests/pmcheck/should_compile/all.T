# Tests for pattern match checker (overlap and exhaustiveness)

# Just do the normal way...
setTestOpts(only_ways(['normal']))
overlapping_incomplete = '-Woverlapping-patterns -Wincomplete-patterns '

# Bug reports / feature requests
test('T2006', [], compile, [overlapping_incomplete])
test('T2204', [], compile, [overlapping_incomplete])
test('T3078', [], compile, [overlapping_incomplete])
test('T322', [], compile, [overlapping_incomplete])
test('T366', [], compile, [overlapping_incomplete])
test('T3927a', [], compile, [overlapping_incomplete])
test('T3927b', [], compile, [overlapping_incomplete])
test('T3927', [], compile, [overlapping_incomplete])
test('T4139', [], compile, [overlapping_incomplete])
test('T6124', [], compile, [overlapping_incomplete])
test('T7669a', [], compile, [overlapping_incomplete])
test('T8970', [], compile, [overlapping_incomplete])
test('T9951b', [], compile, [overlapping_incomplete])
test('T9951', [], compile, [overlapping_incomplete])
test('T10183', [], compile, [overlapping_incomplete])
test('T11303', normal, compile, ['+RTS -M1G -RTS'])
test('T11276', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete+'+RTS -M1G -RTS'])
test('T11303b', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete+'+RTS -M1G -RTS'])
test('T11374', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete+'+RTS -M1G -RTS'])
test('T11822', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T11195', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete+'-package ghc +RTS -M2G -RTS'])
test('T11503', [], compile, [overlapping_incomplete])
test('T11984', normal, compile, [overlapping_incomplete])
test('T14086', normal, compile, [overlapping_incomplete])
test('T14098', normal, compile, [overlapping_incomplete])
test('T14141', [], compile, [overlapping_incomplete])
test('T14813', normal, compile, [overlapping_incomplete])
test('T14899', normal, compile, [overlapping_incomplete])
test('T15305', normal, compile, [overlapping_incomplete])
test('T15385', normal, compile, [overlapping_incomplete])
test('T15450', normal, compile, [overlapping_incomplete])
test('T15584', normal, compile, [overlapping_incomplete])
test('T15713', normal, compile, [overlapping_incomplete])
test('T15753a', normal, compile, [overlapping_incomplete])
test('T15753b', normal, compile, [overlapping_incomplete])
test('T15753c', normal, compile, [overlapping_incomplete])
test('T15753d', normal, compile, [overlapping_incomplete])
test('T15884', normal, compile, [overlapping_incomplete])
test('T16289', normal, compile, [overlapping_incomplete])
test('T17096', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete+'+RTS -M2G -RTS'])
test('T17112', normal, compile, [overlapping_incomplete])
test('T17207', normal, compile, [overlapping_incomplete])
test('T17207b', normal, compile, [overlapping_incomplete])
test('T17208', expect_broken(17208), compile, [overlapping_incomplete])
test('T17215', expect_broken(17215), compile, [overlapping_incomplete])
test('T17216', expect_broken(17216), compile, [overlapping_incomplete])
test('T17218', normal, compile, [overlapping_incomplete])
test('T17219', expect_broken(17219), compile, [overlapping_incomplete])
test('T17234', normal, compile, [overlapping_incomplete])
test('T17248', normal, compile, [overlapping_incomplete])
test('T17340', normal, compile, ['-Wredundant-bang-patterns'])
test('T17357', expect_broken(17357), compile, [overlapping_incomplete])
test('T17376', normal, compile, [overlapping_incomplete])
test('T17378', normal, compile, [overlapping_incomplete])
test('T17465', normal, compile, [overlapping_incomplete])
test('T17646', normal, compile, [overlapping_incomplete])
test('T17703', normal, compile, [overlapping_incomplete])
test('T17725', normal, compile, [overlapping_incomplete])
test('T17729', normal, compile, [overlapping_incomplete])
test('T17783', normal, compile, [overlapping_incomplete+'-Wincomplete-record-updates'])
test('T17836', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T17836b', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T17977', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T17977b', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T18049', normal, compile, [overlapping_incomplete])
test('T18249', normal, compile, ['-Wredundant-bang-patterns'])
test('T18273', normal, compile, [overlapping_incomplete])
test('T18341', normal, compile, [overlapping_incomplete])
test('T18371', normal, compile, [overlapping_incomplete])
test('T18371b', normal, compile, [overlapping_incomplete])
test('T18478', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T18533', normal, compile, [overlapping_incomplete])
test('T18626', normal, compile, [overlapping_incomplete])
test('T18572', normal, compile, [overlapping_incomplete+'-Wincomplete-uni-patterns'])
test('T18609', normal, compile, [overlapping_incomplete])
test('T18610', normal, compile, [overlapping_incomplete])
test('T18670', normal, compile, [overlapping_incomplete])
test('T18708', normal, compile, [overlapping_incomplete])
test('T18932', normal, compile, [overlapping_incomplete])
# Overloaded patterns and rational pattern matching being broken.
test('T19384', expect_broken(19384), compile, [overlapping_incomplete])
test('T19622', normal, compile, [overlapping_incomplete])
test('T20631', normal, compile, [overlapping_incomplete])
test('T20642', normal, compile, [overlapping_incomplete])
test('T21360', normal, compile, [overlapping_incomplete+'-Wincomplete-record-updates'])
test('T21360b', normal, compile, [overlapping_incomplete+'-Wincomplete-record-updates'])
test('T23520', normal, compile, [overlapping_incomplete+'-Wincomplete-record-updates'])
test('T25164', [extra_files(['T25164_aux.hs']), req_th], multimod_compile, ['T25164', '-v0'])

# Other tests
test('pmc001', [], compile, [overlapping_incomplete])
test('pmc002', [], compile, [overlapping_incomplete])
test('pmc003', [], compile, [overlapping_incomplete])
test('pmc004', [], compile, [overlapping_incomplete])
test('pmc005', [], compile, [overlapping_incomplete])
test('pmc006', [], compile, [overlapping_incomplete])
test('pmc007', [], compile, [overlapping_incomplete])
test('pmc008', [], compile, [overlapping_incomplete])
test('pmc009', [], compile, [overlapping_incomplete+'-package ghc'])
test('pmcOrPats', collect_compiler_stats('bytes allocated',10), compile, [overlapping_incomplete])
test('T11245', [], compile, [overlapping_incomplete])
test('T11336b', [], compile, [overlapping_incomplete])
test('T12949', [],  compile, [overlapping_incomplete])
test('T12957', [], compile, [overlapping_incomplete+'-Wincomplete-record-updates'])
test('T12957a', [], compile, [overlapping_incomplete+'-fdefer-type-errors -Wincomplete-record-updates'])
test('PmExprVars', [],  compile, [overlapping_incomplete])
test('CyclicSubst', [],  compile, [overlapping_incomplete])
test('CaseOfKnownCon', [], compile, [overlapping_incomplete])
test('TooManyDeltas', [], compile, [overlapping_incomplete+'-fmax-pmcheck-models=0'])
test('LongDistanceInfo', [], compile, [overlapping_incomplete])
test('LongDistanceDo', [], compile, [overlapping_incomplete])
test('LongDistanceGRHS', [], compile, [overlapping_incomplete])
test('T21662', [],  compile, [overlapping_incomplete])
test('T19271', [],  compile, [overlapping_incomplete])
test('T21761', [],  compile, [overlapping_incomplete])
test('T22964', [], compile, [overlapping_incomplete])
test('T23445', [], compile, [overlapping_incomplete])
test('T24234', [], compile, [overlapping_incomplete+'-Wincomplete-uni-patterns'])

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
test('T10746', [], compile, [overlapping_incomplete])
test('EmptyCase001', [], compile, [overlapping_incomplete])
test('EmptyCase002', [], compile, [overlapping_incomplete])
test('EmptyCase003', [], compile, [overlapping_incomplete])
test('EmptyCase004', [], compile, [overlapping_incomplete])
test('EmptyCase005', [], compile, [overlapping_incomplete])
test('EmptyCase006', [],  compile, [overlapping_incomplete])
test('EmptyCase007', [],  compile, [overlapping_incomplete])
test('EmptyCase008', [],  compile, [overlapping_incomplete])
test('EmptyCase009', [],  compile, [overlapping_incomplete])
test('EmptyCase010', [],  compile, [overlapping_incomplete])
test('DsIncompleteRecSel1', normal, compile, ['-Wincomplete-record-selectors'])
test('DsIncompleteRecSel2', normal, compile, ['-Wincomplete-record-selectors'])
test('DsIncompleteRecSel3', [collect_compiler_stats('bytes allocated', 10)], compile, ['-Wincomplete-record-selectors'])
test('DoubleMatch', normal, compile, [overlapping_incomplete])
test('T24817', normal, compile, [overlapping_incomplete])
test('T25257', normal, compile, [overlapping_incomplete])
test('T24845', [], compile, [overlapping_incomplete])
