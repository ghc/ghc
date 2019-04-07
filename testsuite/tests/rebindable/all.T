# These tests try test the rebindable-syntax feature of GHC,
# which you get when you use -XNoImplicitPrelude
#
# Written by Ashley Yakeley

# No point in doing anything except the normal way
setTestOpts(only_ways(['normal']));

test('rebindable1', normal, compile, [''])
test('rebindable2', normal, compile_and_run, [''])
test('rebindable3', normal, compile_and_run, [''])
test('rebindable4', normal, compile_and_run, [''])
test('rebindable5', normal, compile_and_run, [''])

# rebindable6 has become expected failures
# following #1537
test('rebindable6', normal, compile_fail, [''])

test('rebindable7', normal, compile_and_run, [''])
test('rebindable8', normal, compile, [''])
test('rebindable9', normal, compile, [''])
test('rebindable10', normal, compile_and_run, [''])
test('RebindableFailA', exit_code(1), compile_and_run, [''])
test('RebindableFailB', normal, compile_and_run, [''])

test('T303', normal, compile, [''])

# Tests from Oleg
test('DoRestrictedM', normal, compile, [''])
test('DoParamM', normal, compile_fail, [''])
test('T5038', normal, compile_and_run, [''])
test('T4851', normal, compile, [''])

test('T5908', normal, compile, [''])
test('T10112', normal, compile, [''])
test('T11216', normal, compile, [''])
test('T11216A', normal, compile, [''])
test('T12080', normal, compile, [''])
test('T14670', expect_broken(14670), compile, [''])
