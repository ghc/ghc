# These tests try test the rebindable-syntax feature of GHC,
# which you get when you use -fno-implicit-prelude
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
test('rebindable6', normal, compile_and_run, [''])
test('rebindable7', normal, compile_and_run, [''])
