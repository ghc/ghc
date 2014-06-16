# Args to vtc are: extra compile flags

test('rn003', normal, compile, [''])
test('rn005', normal, compile, [''])
test('rn006', normal, compile, [''])
test('rn009',
     [only_compiler_types(['ghc']),
      extra_clean(['Imp10Aux.hi',      'Imp10Aux.o',
                   'Imp10Aux.hi-boot', 'Imp10Aux.o-boot'])],
     multimod_compile,
     ['rn009', '-v0'])
# rn10 tests the same things as 009,011,012
#test('rn010', only_compiler_types(['ghc']), multimod_compile, ['rn010', '-v0'])
test('rn011',
     [only_compiler_types(['ghc']),
      extra_clean(['Imp100Aux.hi',      'Imp100Aux.o',
                   'Imp100Aux.hi-boot', 'Imp100Aux.o-boot'])],
     multimod_compile,
     ['rn011', '-v0'])
test('rn012',
     [only_compiler_types(['ghc']),
      extra_clean(['Imp500Aux.hi',      'Imp500Aux.o',
                   'Imp500Aux.hi-boot', 'Imp500Aux.o-boot'])],
     multimod_compile,
     ['rn012', '-v0'])
test('rn013', normal, compile, [''])
test('rn017',
     [only_compiler_types(['ghc']),
      extra_clean(['RnAux017.hi',      'RnAux017.o',
                   'RnAux017.hi-boot', 'RnAux017.o-boot'])],
     multimod_compile,
     ['rn017', '-v0'])
test('rn019', normal, compile, [''])
test('rn020', normal, compile, [''])
test('rn022', normal, compile, [''])
test('rn023', normal, compile, [''])
test('rn024', normal, compile, [''])
test('rn025', normal, compile, [''])
test('rn026', normal, compile, [''])
test('rn027', normal, compile, [''])
test('rn028', normal, compile, [''])
test('rn029', normal, compile, [''])
test('rn031', normal, compile, [''])
test('rn032', normal, compile, [''])
test('rn033', normal, compile, [''])
test('rn034', normal, compile, [''])
test('rn035', normal, compile, [''])
test('rn036', normal, compile, [''])
test('rn037', normal, compile, [''])
	
# Missing:
# test('rn038', normal, compile, [''])

test('rn039', normal, compile, [''])
test('rn040', normal, compile, ['-fwarn-unused-binds -fwarn-unused-matches'])
test('rn041', normal, compile, [''])
test('rn042',
     extra_clean(['Rn042_A.hi', 'Rn042_A.o']),
     multimod_compile,
     ['rn042', '-v0'])
test('rn043',
     extra_clean(['Rn043_A.hi', 'Rn043_A.o', 'Rn043_B.hi', 'Rn043_B.o']),
     multimod_compile, ['rn043', '-v0'])
test('rn044',
     extra_clean(['Rn044_A.hi', 'Rn044_A.o', 'Rn044_B.hi', 'Rn044_B.o']),
     multimod_compile, ['rn044', '-v0'])
test('rn045', normal, compile, [''])
test('rn046', normal, compile, ['-W'])
test('rn047', normal, compile, ['-W'])
test('rn048', normal, compile, ['-W'])
test('rn049', normal, compile, ['-W'])

test('rn050',
     extra_clean(['Rn050_A.hi', 'Rn050_A.o']),
     multimod_compile, ['rn050', '-v0'])

test('rn051', normal, compile, [''])

test('rn052',
     extra_clean(['Rn052Aux.hi', 'Rn052Aux.o']),
     multimod_compile, ['rn052', '-v0'])

test('rn053',
     extra_clean(['Rn053_A.hi', 'Rn053_A.o', 'Rn053_B.hi', 'Rn053_B.o']),
     multimod_compile, ['rn053', '-v0'])

test('rn054', normal, compile, [''])
test('rn055', normal, compile, [''])
test('rn056', normal, compile, [''])
test('rn057', normal, compile, [''])
test('rn058', normal, compile, [''])
test('rn059',
     extra_clean(['Rn059_A.hi', 'Rn059_A.o', 'Rn059_B.hi', 'Rn059_B.o']),
     multimod_compile, ['rn059', '-v0'])
test('rn060', normal, compile, [''])
test('rn061', normal, compile, [''])
test('rn062', normal, compile, [''])
test('rn063', normal, compile, [''])
test('rn064', normal, compile, [''])
test('rn065',
     extra_clean(['Rn065A.hi', 'Rn065A.o']),
     multimod_compile,
     ['rn065', '-v0'])

test('rn066',
     extra_clean(['Rn066_A.hi', 'Rn066_A.o']),
     multimod_compile, ['rn066', '-v0'])

test('rn067',
     extra_clean(['Rn067_A.hi', 'Rn067_A.o']),
     multimod_compile, ['rn067', '-v0'])

test('T1972', normal, compile, [''])
test('T2205', normal, compile, [''])

test('T2334', normal, compile, [''])
test('T2506', normal, compile, [''])
test('T2914', normal, compile, [''])
test('T3221', normal, compile, [''])
test('T3262', normal, compile, [''])
test('T3371', normal, compile, [''])
test('T1074b', normal, compile, ['-Wall'])
test('T3640', normal, compile, [''])
test('T3901', normal, compile, [''])
test('T1954', normal, compile, [''])
test('T3943', normal, compile, [''])

test('timing001', normal, compile, [''])
test('timing002', normal, compile, [''])
test('timing003', normal, compile, [''])

test('T1792_imports', extra_clean(['T1792_imports.imports']), run_command,
     ['$MAKE -s --no-print-directory T1792_imports'])

test('T3823',
     extra_clean(['T3823A.hi-boot', 'T3823A.hi', 'T3823B.hi',
                  'T3823A.o-boot', 'T3823A.o', 'T3823B.o']),
     run_command,
     ['$MAKE -s --no-print-directory T3823'])

test('T4003',
     extra_clean(['T4003A.hi-boot', 'T4003A.hi', 'T4003B.hi',
                  'T4003A.o-boot', 'T4003A.o', 'T4003B.o']),
     run_command,
     ['$MAKE -s --no-print-directory T4003'])
test('T1789', normal, compile, [''])
test('T1789_2', normal, compile, [''])
test('T3449',
     extra_clean(['T3449.hi-boot', 'T3449A.hi',
                  'T3449.o-boot',  'T3449A.o']),
     run_command,
     ['$MAKE -s --no-print-directory T3449'])
test('T4239',
     extra_clean(['T4239A.hi', 'T4239A.o', 'T4239.imports']),
     run_command,
     ['$MAKE -s --no-print-directory T4239'])
test('T4240',
     [extra_clean(['T4240A.hi', 'T4240A.o', 'T4240B.hi', 'T4240B.o',
                   'T4240.imports'])],
     run_command,
     ['$MAKE -s --no-print-directory T4240'])

test('T4489', normal, compile, [''])
test('T4478', when(compiler_lt('ghc', '7.1'), expect_fail), compile, [''])
test('T4534', normal, compile, [''])

test('mc09', normal, compile, [''])
test('mc10', normal, compile, [''])
test('mc11', normal, compile, [''])
test('mc12', normal, compile, [''])
test('T2436',
     [ only_compiler_types(['ghc']),
       extra_clean(['T2436a.hi', 'T2436a.o']) ],
     multimod_compile,
     ['T2436', '-v0'])
test('T5331', normal, compile, [''])   # Unused tyvar warnings
test('T5334', normal, compile, [''])   # Unused tyvar warnings
test('T5306',
     [only_compiler_types(['ghc']),
      extra_clean(['T5306a.hi', 'T5306a.o',
                   'T5306b.hi', 'T5306b.o'])],
     multimod_compile,
     ['T5306', '-v0'])
test('T5592',
     [only_compiler_types(['ghc']),
      extra_clean(['T5592a.hi',      'T5592a.o'])],
     multimod_compile_and_run,
     ['T5592', '-v0'])
test('T5867',
     extra_clean(['T5867a.hi', 'T5867a.o']),
     multimod_compile, ['T5867', '-v0'])
test('T6027', normal, compile, [''])
test('T6120', normal, compile, [''])
test('T7007', normal, compile, [''])
test('T7085', normal, compile, [''])
test('T7145a', normal, compile, ['-Wall -Werror'])
test('T7145b', normal, compile, ['-Wall'])
test('T6038', normal, compile, [''])
test('dodgy',
     [ extra_clean(['DodgyA.hi', 'DodgyA.o']) ],
     multimod_compile,
     ['dodgy', '-v0'])
test('T7167', normal, compile, [''])
test('T7336', expect_broken(7336), compile, ['-Wall'])

test('T2435', normal, multimod_compile, ['T2435','-v0'])
test('T7963',
     [extra_clean(['T7963a.hi', 'T7963a.o',
                   'T7963.imports'])],
     run_command,
     ['$MAKE -s --no-print-directory T7963'])
test('T7969',
     [extra_clean(['T7969a.hi', 'T7969a.o',
                   'T7969.imports'])],
     run_command,
     ['$MAKE -s --no-print-directory T7969'])
