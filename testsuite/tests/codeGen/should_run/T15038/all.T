test('T15038',
     [reqlib('containers'), reqlib('ghc-prim'), reqlib('primitive')],
     run_command,
     ['$MAKE -s --no-print-directory T15038'])
