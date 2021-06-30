test('Dep1', only_ways(['normal']), compile, [''])
test('Dep2', only_ways(['normal']), compile, [''])
test('Dep3', only_ways(['normal']), compile, [''])
test('KindEqualities', only_ways(['normal']), compile, [''])
test('KindEqualities2', only_ways(['normal']), compile, [''])
test('Rae31', only_ways(['normal']), compile, [''])
test('RAE_T32b', only_ways(['normal']), compile, [''])
test('KindLevels', normal, compile, [''])
test('RaeBlogPost', normal, compile, [''])
test('mkGADTVars', normal, compile, [''])
test('TypeLevelVec',normal,compile, [''])
test('T9632', normal, compile, [''])

# dynamic-paper used to run out of simplfier ticks because of
# infinite inlining, but the new case-depth mechanism cuts that off,
# so it now compiles fine.
#
# Historical notes: The dynamic-paper test fails in the profasm way if we don't increase
# the simplifier tick limit. If we do, we run out of stack
# space. If we increase the stack size enough with -K,
# we run out of simplifier ticks again. This is
# discussed in #11330.
test('dynamic-paper', normal, compile, [''])

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
test('T13910', [omit_ways(['profasm'])], compile, [''])
test('T13938', [req_th, extra_files(['T13938a.hs'])], makefile_test, ['T13938'])
test('T14556', normal, compile, [''])
test('T14720', normal, compile, [''])
test('T14066a', normal, compile, [''])
test('T14749', normal, compile, [''])
test('T14845_compile', normal, compile, [''])
test('T14991', normal, compile, [''])
test('DkNameRes', normal, compile, [''])
test('T15346', normal, compile, [''])
test('T15419', normal, compile, [''])
test('T14066h', normal, compile, [''])
test('T15666', normal, compile, [''])
test('T15725', normal, compile, [''])
test('T15743', normal, compile, ['-ddump-types -fprint-explicit-foralls'])
test('InferDependency', normal, compile, [''])
test('T15743e', normal, compile, ['-ddump-types -fprint-explicit-foralls'])
test('T15076c', normal, compile, [''])
test('T15829', normal, compile, [''])
test('T14729', normal, compile, ['-ddump-types -fprint-typechecker-elaboration -fprint-explicit-coercions'])
test('T14729kind', normal, ghci_script, ['T14729kind.script'])
test('T16326_Compile1', normal, compile, [''])
test('T16326_Compile2', normal, compile, [''])
test('T16391a', normal, compile, [''])
test('T16344b', normal, compile, [''])
test('T16347', normal, compile, [''])
test('T18660', normal, compile, [''])
test('T12174', normal, compile, [''])
test('LopezJuan', normal, compile, [''])
