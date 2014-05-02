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
test('mod39', normal, compile, [''])
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
test('mod65', normal, compile, [''])
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

test('mod101',
     extra_clean(['Mod101_AuxA.hi', 'Mod101_AuxA.o', 'Mod101_AuxB.hi', 'Mod101_AuxB.o']),
     multimod_compile_fail, ['mod101', '-v0'])

test('mod102',
     extra_clean(['Mod102_AuxA.hi', 'Mod102_AuxA.o', 'Mod102_AuxB.hi', 'Mod102_AuxB.o']),
     multimod_compile_fail, ['mod102', '-v0'])

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
test('mod114', extra_clean(['Mod114_Help.hi', 'Mod114_Help.o']), 
     multimod_compile_fail, ['mod114', '-v0'])

test('mod115', 
     extra_clean(['Mod115_A.hi', 'Mod115_A.o', 'Mod115_B.hi', 'Mod115_B.o']), 
     multimod_compile, ['mod115', '-v0'])

test('mod116', normal, compile_fail, [''])
test('mod117', 
     extra_clean(['Mod117_A.hi', 'Mod117_A.o', 'Mod117_B.hi', 'Mod117_B.o']), 
     multimod_compile, ['mod117', '-v0'])

test('mod118', 
     extra_clean(['Mod118_A.hi', 'Mod118_A.o', 'Mod118_B.hi', 'Mod118_B.o']),
     multimod_compile, ['mod118', '-v0'])

test('mod119',
     extra_clean(['Mod119_A.hi', 'Mod119_A.o', 'Mod119_B.hi', 'Mod119_B.o']),
     multimod_compile, ['mod119', '-v0'])

test('mod120',
     extra_clean(['Mod120_A.hi', 'Mod120_A.o']),
     multimod_compile_fail, ['mod120', '-v0'])

test('mod121',
     extra_clean(['Mod121_A.hi', 'Mod121_A.o']),
     multimod_compile_fail, ['mod121', '-v0'])

test('mod122',
     extra_clean(['Mod122_A.hi', 'Mod122_A.o']),
     multimod_compile_fail, ['mod122', '-v0'])

test('mod123',
     extra_clean(['Mod123_A.hi', 'Mod123_A.o']),
     multimod_compile_fail, ['mod123', '-v0'])

test('mod124',
     extra_clean(['Mod124_A.hi', 'Mod124_A.o']),
     multimod_compile_fail, ['mod124', '-v0'])

test('mod125',
     extra_clean(['Mod125_A.hi', 'Mod125_A.o']),
     multimod_compile_fail, ['mod125', '-v0'])

test('mod126',
     extra_clean(['Mod126_A.hi', 'Mod126_A.o']),
     multimod_compile_fail, ['mod126', '-v0'])

test('mod127',
     extra_clean(['Mod127_A.hi', 'Mod127_A.o']),
     multimod_compile_fail, ['mod127', '-v0'])

test('mod128',
     extra_clean(['Mod128_A.hi', 'Mod128_A.o']) ,
     multimod_compile, ['mod128', '-v0'])

test('mod129', normal, compile, [''])
test('mod130', normal, compile_fail, [''])
test('mod131',
     extra_clean(['Mod131_A.hi', 'Mod131_A.o', 'Mod131_B.hi', 'Mod131_B.o']),
     multimod_compile_fail, ['mod131', '-v0'])

test('mod132',
     extra_clean(['Mod132_A.hi', 'Mod132_A.o', 'Mod132_B.hi', 'Mod132_B.o']),
     multimod_compile_fail, ['mod132', '-v0'])

test('mod133', reqlib('mtl'), compile, [''])
test('mod134', normal, compile_fail, [''])
test('mod135', normal, compile_fail, [''])
test('mod136', extra_clean(['Mod136_A.hi', 'Mod136_A.o']),
     multimod_compile_fail, ['mod136', '-v0'])

test('mod137', extra_clean(['Mod137_A.hi', 'Mod137_A.o']),
     multimod_compile, ['mod137', '-v0'])

test('mod138', extra_clean(['Mod138_A.hi', 'Mod138_A.o']),
     multimod_compile_fail, ['mod138', '-v0'])

test('mod139', extra_clean(['Mod139_A.hi', 'Mod139_A.o', 'Mod139_B.hi', 'Mod139_B.o']),
     multimod_compile, ['mod139', '-v0'])

test('mod140', extra_clean(['Mod140_A.hi', 'Mod140_A.o']),
     multimod_compile, ['mod140', '-v0'])

test('mod141', extra_clean(['Mod141_A.hi', 'Mod141_A.o']),
     multimod_compile, ['mod141', '-v0'])

test('mod142', extra_clean(['Mod142_A.hi', 'Mod142_A.o']),
     multimod_compile_fail, ['mod142', '-v0'])

test('mod143', extra_clean(['Mod143_A.hi', 'Mod143_A.o']),
     multimod_compile_fail, ['mod143', '-v0'])

test('mod144', extra_clean(['Mod144_A.hi', 'Mod144_A.o']),
     run_command, ['$MAKE -s --no-print-directory mod144'])

test('mod145', extra_clean(['Mod145_A.hi', 'Mod145_A.o']),
     multimod_compile_fail, ['mod145', '-v0'])

test('mod146', 
     extra_clean(['Mod145_A.mod146_hi', 'Mod145_A.mod146_o', 'mod146.mod146_o', 'mod146.mod146_hi']),
     run_command, ['$MAKE -s --no-print-directory mod146'])

test('mod147', extra_clean(['Mod147_A.hi', 'Mod147_A.o']),
     multimod_compile_fail, ['mod147', '-v0'])

test('mod148', normal, compile, [''])
test('mod149', normal, compile, [''])
test('mod150', normal, compile_fail, [''])
test('mod151', normal, compile_fail, [''])
test('mod152', normal, compile_fail, [''])
test('mod153', normal, compile_fail, [''])
test('mod154', when(compiler_type('hugs'), expect_fail), compile, [''])
test('mod155', normal, compile_fail, [''])
test('mod156', normal, compile, [''])
test('mod157', 
     extra_clean(['Mod157_A.hi', 'Mod157_A.o', 'Mod157_B.hi', 'Mod157_B.o',
       'Mod157_C.hi', 'Mod157_C.o', 'Mod157_D.hi', 'Mod157_D.o']),
     multimod_compile, ['mod157', '-v0'])

test('mod158',
     extra_clean(['Mod157_A.mod158_hi', 'Mod157_A.mod158_o', 'Mod157_B.mod158_hi', 'Mod157_B.mod158_o',
       'Mod157_C.mod158_hi', 'Mod157_C.mod158_o', 'Mod157_D.mod158_hi', 'Mod157_D.mod158_o']),
     run_command, ['$MAKE -s --no-print-directory mod158'])

test('mod159',
     extra_clean(['Mod159_A.hi', 'Mod159_A.o', 'Mod159_B.hi', 'Mod159_B.o',
       'Mod159_C.hi', 'Mod159_C.o', 'Mod159_D.hi', 'Mod159_D.o']),
     multimod_compile, ['mod159', '-v0'])

test('mod160',
     extra_clean(['Mod159_A.mod160_hi', 'Mod159_A.mod160_o', 'Mod159_B.mod160_hi', 'Mod159_B.mod160_o',
       'Mod159_C.mod160_hi', 'Mod159_C.mod160_o', 'Mod159_D.mod160_hi', 'Mod159_D.mod160_o']),
     run_command, ['$MAKE -s --no-print-directory mod160'])

test('mod161', normal, compile_fail, [''])
test('mod162', extra_clean(['Mod162_A.hi', 'Mod162_A.o']),
     multimod_compile, ['mod162', '-v0'])

test('mod163', extra_clean(['Mod163_A.hi', 'Mod163_A.o']),
     multimod_compile, ['mod163', '-v0'])

test('mod164', extra_clean(['Mod164_A.hi', 'Mod164_A.o', 'Mod164_B.hi', 'Mod164_B.o']),
     multimod_compile_fail, ['mod164', '-v0'])

test('mod165', extra_clean(['Mod164_A.mod165_hi', 'Mod164_A.mod165_o', 'Mod164_B.mod165_hi', 'Mod164_B.mod165_o']),
     run_command, ['$MAKE -s --no-print-directory mod165'])

test('mod166', extra_clean(['Mod164_A.mod166_hi', 'Mod164_A.mod166_o', 'Mod164_B.mod166_hi', 'Mod164_B.mod166_o', 'mod166.mod166_o', 'mod166.mod166_hi']),
     run_command, ['$MAKE -s --no-print-directory mod166 MAKELEVEL='])

test('mod167', extra_clean(['Mod164_A.mod167_hi', 'Mod164_A.mod167_o', 'Mod164_B.mod167_hi', 'Mod164_B.mod167_o', 'mod167.mod167_o', 'mod167.mod167_hi']),
     run_command, ['$MAKE -s --no-print-directory mod167 MAKELEVEL='])

test('mod168', normal, compile, [''])
test('mod169', normal, compile, [''])
test('mod170', extra_clean(['Mod170_A.hi', 'Mod170_A.o']),
     multimod_compile, ['mod170', '-v0'])


test('mod171', extra_clean(['Mod171_A.hi', 'Mod171_A.o', 'Mod171_B.hi', 'Mod171_B.o']),
     multimod_compile, ['mod171', '-v0 -Wall'])


test('mod172', extra_clean(['Mod172_B.hi', 'Mod172_B.o', 'Mod172_C.hi', 'Mod172_C.o']),
     multimod_compile, ['mod172', '-v0'])


test('mod173', extra_clean(['Mod173_Aux.hi', 'Mod173_Aux.o']),
     multimod_compile, ['mod173', '-v0'])

test('mod174', normal, compile_fail, [''])

# mod175 is a sub-directory

test('mod176', normal, compile, ['-fwarn-unused-imports'])
test('mod177', normal, compile, ['-fwarn-unused-imports'])
test('mod178', extra_clean(['Mod178_2.hi', 'Mod178_2.o']),
     multimod_compile_fail, ['mod178', '-v0'])

test('mod179', extra_clean(['Mod179_A.hi', 'Mod179_A.o']),
     multimod_compile, ['mod179', '-v0'])


test('mod180', extra_clean(['Mod180_A.hi', 'Mod180_A.o', 'Mod180_B.hi', 'Mod180_B.o']),
     multimod_compile_fail, ['mod180', '-v0'])

test('T1148', normal, compile, [''])
test('T1074', reqlib('mtl'), compile, [''])
test('T1074a', normal, compile, [''])
test('T2267', reqlib('utf8-string'), compile, [''])
test('T414', normal, compile_fail, [''])
test('T414a', normal, compile, [''])
test('T414b', normal, compile, [''])
test('T3776', normal, compile, [''])
test('T9061', normal, compile, [''])
