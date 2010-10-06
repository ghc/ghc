
test('read001', normal, compile, [''])
test('read002', normal, compile, [''])
test('read003', normal, compile, [''])
test('read004', normal, compile, [''])
test('read005', normal, compile, [''])

# Missing:
# test('read006', normal, compile, [''])
# test('read007', normal, compile, [''])

test('read008', normal, compile, [''])
test('read009', normal, compile, [''])
test('read010', normal, compile, [''])
test('read011', normal, compile, [''])

# Missing:
# test('read012', normal, compile, [''])
# test('read013', normal, compile, [''])

test('read014', omit_ways(['optc', 'profc']), compile, ['-Wall -fno-warn-orphans'])
test('read015', normal, compile, [''])
test('read016', normal, compile, [''])
test('read017', normal, compile, [''])
test('read018', normal, compile, [''])
test('read019', normal, compile, [''])

# Missing:
# test('read020', normal, compile, [''])

test('read021', normal, compile, [''])
test('read022', normal, compile, [''])

# we don't support strict Haskell 98 with respect to qualified identifiers
# any more (read023), see #1215.  This test is in fact incorrect Haskell 98
# anyway, so expect_fail would be wrong.
test('read023', skip, compile, [''])

test('read024', normal, compile, [''])
test('read025', normal, compile, [''])
test('read026', normal, compile, [''])
test('read027', normal, compile, [''])
test('read028', normal, compile, [''])
test('read029', normal, compile, [''])
test('read030', normal, compile, [''])
test('read031', normal, compile, [''])
test('read032', normal, compile, [''])
test('read033', if_compiler_type('hugs', expect_fail), compile, [''])
test('read034', if_compiler_type('hugs', expect_fail), compile, [''])
test('read036', normal, compile, [''])
test('read037', normal, compile, [''])
test('read038', normal, compile, [''])
test('read039', normal, compile, ['-fno-code'])
test('read040', normal, compile, [''])
test('read041', literate, compile, [''])
test('read042', normal, compile, [''])
test('read043', normal, compile, [''])
test('read044', normal, compile, [''])
test('read045', normal, compile, [''])
test('read046', normal, compile, [''])
test('read047', normal, compile, [''])
test('read048', normal, compile, [''])
test('read049', normal, compile, [''])
test('read050', normal, compile, [''])
test('read051', normal, compile, [''])
test('read052', normal, compile, [''])
test('read053', normal, compile, [''])
test('read054', normal, compile, [''])
test('read055', normal, compile, [''])
test('read056', normal, compile, [''])
test('read057', normal, compile, [''])
test('read058', omit_ways(['optc', 'profc']), compile, [''])
test('read059', omit_ways(['optc', 'profc']), compile, [''])
test('read060', normal, compile, [''])
test('read061', normal, compile, [''])
test('read062', normal, compile, [''])
test('read063', normal, compile, [''])
test('read064', normal, compile, [''])
test('read065',
     [if_compiler_lt('ghc', '6.11', skip),
      omit_ways(['optc', 'profc'])],
     compile,
     [''])
test('read066', if_compiler_le('ghc', '6.10.1', expect_fail), compile, [''])
test('read067', normal, compile, [''])
test('read068', normal, compile, [''])
test('read_1821', normal, compile, [''])
test('T3303', extra_clean(['T3303A.hi', 'T3303A.o']),
     multimod_compile, ['T3303', '-v0'])
test('T3741', normal, compile, [''])
test('DoAndIfThenElse', normal, compile, [''])
