# These tests try test the rebindable-syntax feature of GHC,
# which you get when you use -fno-implicit-prelude
#

# No point in doing anything except the normal way 
setTestOpts(only_ways(['normal']));

test('mdo001', normal, compile_and_run, [''])

