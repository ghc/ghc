test('Roles5', normal, compile_fail, [''])
test('Roles6', normal, compile_fail, [''])
test('Roles7', normal, compile_fail, [''])
test('Roles8', normal, compile_fail, [''])
test('Roles10', normal, compile_fail, [''])
test('Roles11', normal, compile_fail, [''])
test('Roles12',
     extra_clean(['Roles12.o-boot', 'Roles12.hi-boot']),
     run_command, ['$MAKE --no-print-directory -s Roles12'])
test('T8773', normal, compile_fail, [''])
test('T9204', extra_clean(['T9204.o-boot', 'T9204.hi-boot']),
              run_command, ['$MAKE --no-print-directory -s T9204'])
test('RolesIArray', normal, compile_fail, [''])
