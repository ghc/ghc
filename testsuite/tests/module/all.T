# These tests were swiped from the hugs98 source tree, in hugs98/tests/static,
# on 16/10/2002.
#
# I used something like this to partly-automate the transition:
#
# for i in mod*.hs; do
#   if ! test -f ${i%.hs}.output; then
#      echo "test('${i%.hs}', normal, compile, [''])"
#   else if grep error ${i%.hs}.output >/dev/null; then
#      echo "test('${i%.hs}', normal, compile_fail, [''])"
#   else
#      echo "test('${i%.hs}', normal, compile, [''])"
#   fi fi
# done
#
# A few of the tests use Hugs-specific extensions, and have been
# commented out.

# No point in doing anything except the normal way for renamer tests
setTestOpts(only_ways(['normal']));

test('mod1', normal, compile_fail, [''])
test('mod2', normal, compile_fail, [''])
test('mod3', normal, compile_fail, [''])
test('mod4', normal, compile_fail, [''])
test('mod5', normal, compile, [''])
test('mod6', normal, compile, [''])
test('mod7', normal, compile_fail, [''])
test('mod8', normal, compile_fail, [''])
test('mod9', normal, compile_fail, [''])
test('mod10', normal, compile_fail, [''])
test('mod11', normal, compile, [''])
test('mod12', normal, compile, [''])
test('mod13', normal, compile, [''])
test('mod14', normal, compile, [''])
test('mod15', normal, compile, [''])
test('mod16', normal, compile, [''])
test('mod17', normal, compile_fail, [''])
test('mod18', normal, compile_fail, [''])
test('mod19', normal, compile_fail, [''])
test('mod20', normal, compile_fail, [''])
test('mod21', normal, compile_fail, [''])
test('mod22', normal, compile_fail, [''])
test('mod23', normal, compile_fail, [''])
test('mod24', normal, compile_fail, [''])
test('mod25', normal, compile_fail, [''])
test('mod26', normal, compile_fail, [''])
test('mod27', normal, compile_fail, [''])
#test('mod28', normal, compile_fail, [''])
test('mod29', normal, compile_fail, [''])
test('mod30', normal, compile, [''])
test('mod31', normal, compile, [''])
test('mod32', normal, compile, [''])
test('mod33', normal, compile, [''])
test('mod34', normal, compile, [''])
test('mod35', normal, compile, [''])
test('mod36', normal, compile_fail, [''])
test('mod37', normal, compile, [''])
test('mod38', normal, compile_fail, [''])
test('mod39', normal, compile_fail, [''])
test('mod40', normal, compile_fail, [''])
test('mod41', normal, compile_fail, [''])
test('mod42', normal, compile_fail, [''])
test('mod43', normal, compile_fail, [''])
test('mod44', normal, compile_fail, [''])
test('mod45', normal, compile_fail, [''])
test('mod46', normal, compile_fail, [''])
test('mod47', normal, compile_fail, [''])
test('mod48', normal, compile_fail, [''])
test('mod49', normal, compile_fail, [''])
test('mod50', normal, compile_fail, [''])
test('mod51', normal, compile_fail, [''])
test('mod52', normal, compile_fail, [''])
test('mod53', normal, compile_fail, [''])
test('mod54', normal, compile_fail, [''])
test('mod55', normal, compile_fail, [''])
test('mod56', normal, compile_fail, [''])
#test('mod57', normal, compile_fail, [''])
test('mod58', normal, compile_fail, [''])
test('mod59', normal, compile_fail, [''])
test('mod60', normal, compile_fail, [''])
test('mod61', normal, compile_fail, [''])
test('mod62', normal, compile_fail, [''])
test('mod63', normal, compile_fail, [''])
test('mod64', normal, compile, [''])
test('mod65', normal, compile, ['-Wno-overlapping-patterns'])
test('mod66', normal, compile_fail, [''])
test('mod67', normal, compile_fail, [''])
test('mod68', normal, compile_fail, [''])
test('mod69', normal, compile_fail, [''])
test('mod70', normal, compile_fail, [''])
test('mod71', normal, compile_fail, [''])
test('mod72', normal, compile_fail, [''])

test('mod73', normal, compile_fail, [''])

test('mod74', normal, compile_fail, [''])
test('mod75', normal, compile, [''])
test('mod76', normal, compile_fail, [''])
test('mod77', normal, compile_fail, [''])
#test('mod78', normal, compile_fail, [''])
test('mod79', normal, compile_fail, [''])
test('mod80', normal, compile_fail, [''])
test('mod81', normal, compile_fail, [''])
test('mod82', normal, compile, [''])
test('mod83', normal, compile, [''])
test('mod84', normal, compile, [''])
test('mod85', normal, compile, [''])
test('mod86', normal, compile, [''])
test('mod87', normal, compile_fail, [''])
test('mod88', normal, compile_fail, [''])
test('mod89', normal, compile_fail, [''])
test('mod90', normal, compile_fail, [''])
test('mod91', normal, compile_fail, [''])
test('mod92', normal, compile, [''])
test('mod93', normal, compile, [''])
test('mod94', normal, compile, [''])
test('mod95', normal, compile, [''])
test('mod96', normal, compile, [''])
test('mod97', normal, compile_fail, [''])
test('mod98', normal, compile_fail, [''])
test('mod99', normal, compile, [''])
test('mod100', normal, compile, [''])

test('mod101', [extra_files(['Mod101_AuxA.hs', 'Mod101_AuxB.hs'])], multimod_compile_fail, ['mod101', '-v0'])

test('mod102', [extra_files(['Mod102_AuxA.hs', 'Mod102_AuxB.hs'])], multimod_compile_fail, ['mod102', '-v0'])

test('mod103', normal, compile, [''])
test('mod104', normal, compile, [''])
test('mod105', normal, compile, [''])
test('mod106', normal, compile, [''])
test('mod107', normal, compile, [''])
test('mod108', normal, compile, [''])
test('mod109', normal, compile, [''])
test('mod110', normal, compile_fail, [''])
test('mod111', normal, compile, [''])
test('mod112', normal, compile, [''])
test('mod113', normal, compile, [''])
test('mod114', [extra_files(['Mod114_Help.hs'])], multimod_compile_fail, ['mod114', '-v0'])

test('mod115', [extra_files(['Mod115_A.hs', 'Mod115_B.hs'])], multimod_compile, ['mod115', '-v0'])

test('mod116', normal, compile_fail, [''])
test('mod117', [extra_files(['Mod117_A.hs', 'Mod117_B.hs'])], multimod_compile, ['mod117', '-v0'])

test('mod118', [extra_files(['Mod118_A.hs', 'Mod118_B.hs'])], multimod_compile, ['mod118', '-v0'])

test('mod119', [extra_files(['Mod119_A.hs', 'Mod119_B.hs'])], multimod_compile, ['mod119', '-v0'])

test('mod120', [extra_files(['Mod120_A.hs'])], multimod_compile_fail, ['mod120', '-v0'])

test('mod121', [extra_files(['Mod121_A.hs'])], multimod_compile_fail, ['mod121', '-v0'])

test('mod122', [extra_files(['Mod122_A.hs'])], multimod_compile_fail, ['mod122', '-v0'])

test('mod123', [extra_files(['Mod123_A.hs'])], multimod_compile_fail, ['mod123', '-v0'])

test('mod124', [extra_files(['Mod124_A.hs'])], multimod_compile_fail, ['mod124', '-v0'])

test('mod125', [extra_files(['Mod125_A.hs'])], multimod_compile_fail, ['mod125', '-v0'])

test('mod126', [extra_files(['Mod126_A.hs'])], multimod_compile_fail, ['mod126', '-v0'])

test('mod127', [extra_files(['Mod127_A.hs'])], multimod_compile_fail, ['mod127', '-v0'])

test('mod128', [extra_files(['Mod128_A.hs'])], multimod_compile, ['mod128', '-v0'])

test('mod129', normal, compile, [''])
test('mod130', normal, compile_fail, [''])
test('mod131', [extra_files(['Mod131_A.hs', 'Mod131_B.hs'])], multimod_compile_fail, ['mod131', '-v0'])

test('mod132', [extra_files(['Mod132_A.hs', 'Mod132_B.hs'])], multimod_compile_fail, ['mod132', '-v0'])

test('mod133', normal, compile, [''])
test('mod134', normal, compile_fail, [''])
test('mod135', normal, compile_fail, [''])
test('mod136', [extra_files(['Mod136_A.hs'])], multimod_compile_fail, ['mod136', '-v0'])

test('mod137', [extra_files(['Mod137_A.hs'])], multimod_compile, ['mod137', '-v0'])

test('mod138', [extra_files(['Mod138_A.hs'])], multimod_compile_fail, ['mod138', '-v0'])

test('mod139', [extra_files(['Mod139_A.hs', 'Mod139_B.hs'])], multimod_compile, ['mod139', '-v0'])

test('mod140', [extra_files(['Mod140_A.hs'])], multimod_compile, ['mod140', '-v0'])

test('mod141', [extra_files(['Mod141_A.hs'])], multimod_compile, ['mod141', '-v0'])

test('mod142', [extra_files(['Mod142_A.hs'])], multimod_compile_fail, ['mod142', '-v0'])

test('mod143', [extra_files(['Mod143_A.hs'])], multimod_compile_fail, ['mod143', '-v0'])

test('mod144', [extra_files(['Mod144_A.hs'])], makefile_test, [])

test('mod145', [extra_files(['Mod145_A.hs'])], multimod_compile_fail, ['mod145', '-v0'])

test('mod146', [extra_files(['Mod145_A.hs'])], makefile_test, [])

test('mod147', [extra_files(['Mod147_A.hs'])], multimod_compile_fail, ['mod147', '-v0'])

test('mod148', normal, compile, [''])
test('mod149', normal, compile, [''])
test('mod150', normal, compile_fail, [''])
test('mod151', normal, compile_fail, [''])
test('mod152', normal, compile_fail, [''])
test('mod153', normal, compile_fail, [''])
test('mod154', normal, compile, [''])
test('mod155', normal, compile_fail, [''])
test('mod156', normal, compile, [''])
test('mod157', [extra_files(['Mod157_A.hs', 'Mod157_B.hs', 'Mod157_C.hs', 'Mod157_D.hs'])], multimod_compile, ['mod157', '-v0'])

test('mod158', [extra_files(['Mod157_A.hs', 'Mod157_B.hs', 'Mod157_C.hs', 'Mod157_D.hs'])], makefile_test, [])

test('mod159', [extra_files(['Mod159_A.hs', 'Mod159_B.hs', 'Mod159_C.hs', 'Mod159_D.hs'])], multimod_compile, ['mod159', '-v0'])

test('mod160', [extra_files(['Mod159_A.hs', 'Mod159_B.hs', 'Mod159_C.hs', 'Mod159_D.hs'])], makefile_test, [])

test('mod161', normal, compile_fail, [''])
test('mod162', [extra_files(['Mod162_A.hs'])], multimod_compile, ['mod162', '-v0'])

test('mod163', [extra_files(['Mod163_A.hs'])], multimod_compile, ['mod163', '-v0'])

test('mod164', [extra_files(['Mod164_A.hs', 'Mod164_B.hs'])], multimod_compile_fail, ['mod164', '-v0'])

test('mod165', [extra_files(['Mod164_A.hs', 'Mod164_B.hs'])], makefile_test, [])

test('mod166', [extra_files(['Mod164_A.hs', 'Mod164_B.hs'])],
     run_command, ['$MAKE -s --no-print-directory mod166 MAKELEVEL='])

test('mod167', [extra_files(['Mod164_A.hs', 'Mod164_B.hs'])],
     run_command, ['$MAKE -s --no-print-directory mod167 MAKELEVEL='])

test('mod168', normal, compile, [''])
test('mod169', normal, compile, [''])
test('mod170', [extra_files(['Mod170_A.hs'])], multimod_compile, ['mod170', '-v0'])


test('mod171', [extra_files(['Mod171_A.hs', 'Mod171_B.hs'])], multimod_compile, ['mod171', '-v0 -Wall'])


test('mod172', [extra_files(['Mod172_B.hs', 'Mod172_C.hs'])], multimod_compile, ['mod172', '-v0'])


test('mod173', [extra_files(['Mod173_Aux.hs'])], multimod_compile, ['mod173', '-v0'])

# mod174 has been deleted
# mod175 is a sub-directory

test('mod176', normal, compile, ['-fwarn-unused-imports'])
test('mod177', normal, compile, ['-fwarn-unused-imports'])
test('mod178', [extra_files(['Mod178_2.hs'])], multimod_compile_fail, ['mod178', '-v0'])

test('mod179', [extra_files(['Mod179_A.hs'])], multimod_compile, ['mod179', '-v0'])


test('mod180', [extra_files(['Mod180_A.hs', 'Mod180_B.hs'])], multimod_compile_fail, ['mod180', '-v0'])

# Tests for 'ImportQualifiedPost'
test('mod181', normal, compile, [''])
test('mod182', normal, compile_fail, [''])
test('mod183', normal, compile_fail, [''])
test('mod184', normal, compile, ['-Wprepositive-qualified-module'])
test('mod185', normal, compile, ['-ddump-parsed-ast'])

test('T1148', normal, compile, [''])
test('T1074', normal, compile, [''])
test('T1074a', normal, compile, [''])
test('T414', normal, compile_fail, [''])
test('T414a', normal, compile, [''])
test('T414b', normal, compile, [''])
test('T3776', normal, compile, [''])
test('T7765', normal, compile_fail, [''])
test('T9061', normal, compile, [''])
test('T9997', normal, compile, [''])
test('T10233', [], multimod_compile, ['T10233', '-v0'])
test('T11432', normal, compile_fail, [''])
test('T11432a', normal, compile_fail, [''])
test('T12026', normal, compile_fail, [''])
test('T11970', normal, compile_fail, [''])
test('T11970A', [], multimod_compile, ['T11970A','-Wunused-imports'])
test('T11970B', normal, compile_fail, [''])
test('MultiExport', normal, compile, [''])
test('T13528', normal, compile, [''])
test('T13622', normal, compile, [''])
test('T13704a', normal, compile, ['-main-is Main.program'])
test('T13704b', [], multimod_compile, ['T13704b1.hs T13704b2.hs', '-main-is T13704b2.program -v0'])
test('T20562', normal, compile, [''])

test('T21752', [extra_files(['T21752A.hs', 'T21752.hs'])], multimod_compile, ['T21752', '-v0'])

test('TupleTyConUserSyntax', [extra_files(['TupleTyConUserSyntaxA.hs', 'TupleTyConUserSyntax.hs'])], multimod_compile, ['TupleTyConUserSyntax', '-v0'])
test('T21826', normal, compile_fail, [''])
test('T20007', normal, compile_fail, [''])
