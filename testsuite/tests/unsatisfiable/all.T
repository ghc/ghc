
test('Unsatisfiable1', normal, compile, [''])
test('Unsatisfiable2', normal, compile, [''])
test('UnsatisfiableFail1', normal, compile_fail, [''])
test('UnsatisfiableFail2', normal, compile_fail, [''])
test('UnsatisfiableFail3', normal, compile_fail, [''])
test('UnsatisfiableFail4', normal, compile_fail, [''])

test('UnsatClassMethods', normal, compile, ['-Werror=missing-methods'])
test('UnsatDefault', normal, compile_fail, [''])
test('UnsatDefer', exit_code(1), compile_and_run, ['-fdefer-type-errors'])
test('UnsatFunDeps', normal, compile, [''])
test('UnsatInstance', normal, compile_fail, [''])
test('UnsatPMWarnings', normal, compile, ['-Woverlapping-patterns -Wincomplete-patterns'])

test('T11503_Unsat', normal, compile, ['-Woverlapping-patterns -Wincomplete-patterns'])
test('T14141_Unsat', normal, compile, ['-Woverlapping-patterns -Wincomplete-patterns'])
test('T14339_Unsat', normal, compile_fail, [''])
test('T15232_Unsat', normal, compile, ['-Wredundant-constraints'])

test('T23816', exit_code(1), compile_and_run, ['-fdefer-type-errors'])
