# These tests try test the rebindable-syntax feature of GHC,
# which you get when you use -XNoImplicitPrelude
#
# Written by Ashley Yakeley

# No point in doing anything except the normal way 
setTestOpts(only_ways(['normal']));
setTestOpts(only_compiler_types(['ghc']))

test('rebindable1', normal, compile, [''])
test('rebindable2', normal, compile_and_run, [''])
test('rebindable3', normal, compile_and_run, [''])
test('rebindable4', normal, compile_and_run, [''])
test('rebindable5', normal, compile_and_run, [''])

# rebindable6 has become expected failures
# following Trac #1537
test('rebindable6', normal, compile_fail, [''])

test('rebindable7', normal, compile_and_run, [''])
test('rebindable8', normal, compile, [''])
test('rebindable9', normal, compile, [''])
test('rebindable10', normal, compile_and_run, [''])

test('T303', normal, compile, [''])

# Tests from Oleg
test('DoRestrictedM', normal, compile, [''])
test('DoParamM', reqlib('mtl'), compile_fail, [''])
test('T5038', normal, compile_and_run, [''])
test('T4851', normal, compile, [''])

test('T5821', normal, compile, [''])
