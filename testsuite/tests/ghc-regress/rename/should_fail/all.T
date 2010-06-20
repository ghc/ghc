
test('rnfail001', normal, compile_fail, [''])
test('rnfail002', normal, compile_fail, [''])
test('rnfail003', normal, compile_fail, [''])
test('rnfail004', normal, compile_fail, [''])
test('rnfail007', normal, compile_fail, [''])
test('rnfail008', normal, compile_fail, [''])
test('rnfail009', normal, compile_fail, [''])
test('rnfail010', normal, compile_fail, [''])
test('rnfail011', normal, compile_fail, [''])
test('rnfail012', normal, compile_fail, [''])
test('rnfail013', normal, compile_fail, [''])

test('rnfail015', normal, compile_fail, [''])
test('rnfail016', normal, compile_fail, [''])
test('rnfail017', normal, compile_fail, [''])
test('rnfail018', normal, compile_fail, [''])
test('rnfail019', normal, compile_fail, [''])
test('rnfail020', normal, compile, [''])	# Succeeds now (Jan07)
test('rnfail021', normal, compile_fail, [''])
test('rnfail022', normal, compile_fail, [''])
test('rnfail023', normal, compile_fail, [''])
test('rnfail024', normal, compile_fail, [''])
test('rnfail025', normal, compile_fail, [''])
test('rnfail026', normal, compile_fail, [''])
test('rnfail027', normal, compile_fail, [''])
test('rnfail028', normal, compile_fail, [''])
test('rnfail029', normal, compile_fail, [''])
test('rnfail030', normal, compile_fail, [''])
test('rnfail031', normal, compile_fail, [''])
test('rnfail032', normal, compile_fail, [''])
test('rnfail033', normal, compile_fail, [''])
test('rnfail034', normal, compile_fail, [''])
test('rnfail035', normal, compile_fail, [''])

# Missing:
# test('rnfail037', normal, compile_fail, [''])

test('rnfail038', normal, compile_fail, [''])
test('rnfail039', normal, compile_fail, [''])

test('rnfail040',
     extra_clean(['Rnfail040_A.hi', 'Rnfail040_A.o']),
     multimod_compile_fail, ['rnfail040', '-v0'])
test('rnfail041', normal, compile_fail, [''])
test('rnfail042', normal, compile_fail, [''])

test('rnfail043', skip_if_no_ghci, compile_fail, ['-v0'])
test('rnfail044', normal, compile_fail, [''])
test('rnfail045', normal, compile_fail, [''])
test('rnfail046', normal, compile_fail, [''])
test('rnfail047',
     extra_clean(['RnFail047_A.hi-boot', 'RnFail047_A.o-boot']),
     multimod_compile_fail,
     ['rnfail047', '-v0'])
test('rnfail048', normal, compile_fail, [''])
test('rnfail049', normal, compile_fail, [''])
test('rnfail050', normal, compile_fail, [''])
test('rnfail051', normal, compile_fail, [''])
test('rnfail052', normal, compile_fail, [''])
test('rnfail053', normal, compile_fail, [''])
test('rnfail054', normal, compile_fail, [''])
test('rnfail055',
     extra_clean(['RnFail055.hi-boot', 'RnFail055.o-boot',
                  'RnFail055_aux.hi',  'RnFail055_aux.o']),
     multimod_compile_fail,
     ['RnFail055','-v0'])
test('rnfail056', if_compiler_lt('ghc', '6.11', expect_fail), compile_fail, [''])

test('rn_dup', normal, compile_fail, [''])
test('T2310', normal, compile_fail, [''])
test('T2490', normal, compile_fail, [''])
test('T2901', normal, compile_fail, [''])
test('T2723', normal, compile, [''])  # Warnings only
test('T2993', normal, compile_fail, [''])
test('T3265', normal, compile_fail, [''])
test('T3792', normal, compile_fail, [''])
test('T4042', normal, compile_fail, [''])

