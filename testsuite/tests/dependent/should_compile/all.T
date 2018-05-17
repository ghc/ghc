test('Dep1', only_ways('normal'), compile, [''])
test('Dep2', only_ways('normal'), compile, [''])
test('Dep3', only_ways('normal'), compile, [''])
test('KindEqualities', only_ways('normal'), compile, [''])
test('KindEqualities2', only_ways('normal'), compile, [''])
test('Rae31', only_ways('normal'), compile, [''])
test('RAE_T32b', only_ways('normal'), compile, [''])
test('KindLevels', normal, compile, [''])
test('RaeBlogPost', normal, compile, [''])
test('mkGADTVars', normal, compile, [''])
test('TypeLevelVec',normal,compile, [''])
test('T9632', normal, compile, [''])
# The dynamic-paper test fails in the profasm way if we don't increase
# the simplifier tick limit. If we do, we run out of stack
# space. If we increase the stack size enough with -K,
# we run out of simplifier ticks again. This is
# discussed in #11330.
test('dynamic-paper',
     expect_broken_for(11330, ['profasm']),
     compile, [''])
test('T11311', normal, compile, [''])
test('T11405', normal, compile, [''])
test('T11241', normal, compile, [''])
test('T11711', normal, compile, [''])
test('RaeJobTalk', normal, compile, [''])
test('T11635', normal, compile, [''])
test('T11719', normal, compile, [''])
test('T11966', normal, compile, [''])
test('T12442', normal, compile, [''])
test('T13538', normal, compile, [''])
test('T12176', normal, compile, [''])
test('T14038', normal, compile, [''])
test('T12742', normal, compile, [''])
# we omit profasm because it doesn't bring much to the table but
# introduces its share of complexity, as the test as it is fails with
# profasm:
# T13910.hs:6:5: fatal:
#     Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('T13910', omit_ways(['profasm']), compile, [''])
test('T13938', [extra_files(['T13938a.hs'])], run_command,
     ['$MAKE -s --no-print-directory T13938'])
test('T14556', normal, compile, [''])
test('T14720', normal, compile, [''])
test('T14066a', normal, compile, [''])
test('T14749', normal, compile, [''])
test('T14991', normal, compile, [''])
