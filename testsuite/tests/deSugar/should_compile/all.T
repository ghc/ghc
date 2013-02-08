# Just do the normal way...
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('ds-wildcard', normal, compile, [''])
test('ds001', normal, compile, [''])
test('ds002', normal, compile, [''])
test('ds003', normal, compile, [''])
test('ds004', normal, compile, [''])
test('ds005', normal, compile, [''])
test('ds006', normal, compile, [''])
test('ds007', normal, compile, [''])
test('ds008', normal, compile, [''])
test('ds009', normal, compile, [''])
test('ds010', normal, compile, [''])
test('ds011', normal, compile, [''])
test('ds012', normal, compile, [''])
test('ds013', normal, compile, [''])
test('ds014', normal, compile, [''])
test('ds015', normal, compile, [''])
test('ds016', normal, compile, [''])
test('ds017', normal, compile, [''])
test('ds018', normal, compile, [''])
test('ds019', normal, compile, [''])
test('ds020', normal, compile, [''])
test('ds021', normal, compile, [''])
test('ds022', normal, compile, [''])
test('ds023', normal, compile, [''])
test('ds024', normal, compile, [''])
test('ds025', normal, compile, [''])
test('ds026', normal, compile, [''])
test('ds027', normal, compile, [''])
test('ds028', normal, compile, [''])
test('ds029', normal, compile, [''])
test('ds030', normal, compile, [''])
test('ds031', normal, compile, [''])
test('ds032', normal, compile, [''])
test('ds033', normal, compile, [''])
test('ds034', normal, compile, [''])
test('ds035', only_compiler_types(['ghc']), compile, [''])
test('ds036', normal, compile, [''])
test('ds037', normal, compile, [''])
test('ds038', normal, compile, [''])
test('ds039', normal, compile, [''])
test('ds040', normal, compile, [''])
test('ds041', normal, compile, [''])
test('ds042', normal, compile, [''])
test('ds043', normal, compile, [''])
test('ds044', normal, compile, [''])
test('ds045', normal, compile, [''])
test('ds046', normal, compile, ['-funbox-strict-fields'])
test('ds047', normal, compile, [''])
test('ds048', normal, compile, [''])
test('ds050', normal, compile, [''])
test('ds051', normal, compile, [''])
test('ds052', normal, compile, [''])
test('ds053', normal, compile, [''])
test('ds054', normal, compile, [''])
test('ds055', only_compiler_types(['ghc']), compile, [''])
test('ds056', normal, compile, ['-Wall'])
test('ds057', normal, compile, [''])
test('ds058', normal, compile, ['-W'])
test('ds059', normal, compile, ['-W'])
test('ds060', expect_broken(322), compile, [''])
test('ds061', expect_broken(851), compile, [''])
test('ds062', normal, compile, [''])
test('ds063', normal, compile, [''])

test('T2409', normal, compile, [''])
test('T3263-1', normal, compile, ['-fwarn-unused-do-bind -XRecursiveDo'])
test('T3263-2', normal, compile, ['-fwarn-wrong-do-bind -XScopedTypeVariables -XRecursiveDo'])

test('GadtOverlap', normal, compile, ['-Wall'])
test('T2395', normal, compile, [''])
test('T4371', normal, compile, [''])
test('T4439', normal, compile, [''])
test('T4488', if_compiler_lt('ghc', '7.1', expect_fail), compile, [''])
test('T4870',
     [only_ways(['optasm']),
      only_compiler_types(['ghc']),
      extra_clean(['T4870a.hi', 'T4870a.o'])],
     multimod_compile,
     ['T4870', '-v0'])
test('T5117', normal, compile, [''])
test('T5252',
     extra_clean(['T5252a.hi', 'T5252a.o']),
     run_command, 
     ['$MAKE -s --no-print-directory T5252'])
test('T5455', normal, compile, [''])
test('T5001',
     [only_ways(['optasm']),
      only_compiler_types(['ghc']),
      extra_clean(['T5001a.hi', 'T5001a.o'])],
     multimod_compile,
     ['T5001', '-v0'])

# T5252Take2 failed when compiled *wihtout* optimisation
test('T5252Take2',
     extra_clean(['T5252Take2a.hi', 'T5252Take2a.o']),
     run_command, 
     ['$MAKE -s --no-print-directory T5252Take2'])
test('T2431', normal, compile, ['-ddump-simpl -dsuppress-uniques'])
