test('multipleHomeUnits_single1', [extra_files([ 'a/', 'unitA'])], multiunit_compile, [['unitA'], '-fhide-source-paths'])
test('multipleHomeUnits_single2', [extra_files([ 'b/', 'unitB'])], multiunit_compile, [['unitB'], '-fhide-source-paths'])
test('multipleHomeUnits_single3', [extra_files([ 'c/', 'unitC'])], multiunit_compile, [['unitC'], '-fhide-source-paths'])
test('multipleHomeUnits_single4', [extra_files([ 'd/', 'unitD'])], multiunit_compile, [['unitD'], '-fhide-source-paths'])
test('multipleHomeUnits_single5', [req_th,extra_files([ 'th/', 'unitTH'])], multiunit_compile, [['unitTH'], '-fhide-source-paths'])
test('multipleHomeUnits_cpp', [extra_files([ 'cpp-includes/', 'unitCPPIncludes'])], multiunit_compile, [['unitCPPIncludes'], '-fhide-source-paths'])
test('multipleHomeUnits_cfile', [extra_files([ 'c-file/', 'unitCFile'])], multiunit_compile, [['unitCFile'], '-fhide-source-paths'])
test('multipleHomeUnits_callstack', [extra_files([ 'callstack/', 'unitCallstack'])], makefile_test, [])

test('multipleHomeUnits_warnings', [extra_files([ 'unitWarnings', 'A.hs'])], multiunit_compile_fail, [['unitWarnings'], '-fhide-source-paths'])

test('multipleHomeUnits_cpp2', [extra_files([ 'cpp-includes/', 'cpp-import/', 'unitCPPImport', 'unitCPPIncludes'])], multiunit_compile, [['unitCPPImport', 'unitCPPIncludes'], '-fhide-source-paths'])

test('multiGHCi', [extra_files(['a/', 'b/', 'unitA', 'unitB', 'multiGHCi.script'])
                  , extra_run_opts('-unit @unitA -unit @unitB')], ghci_script, ['multiGHCi.script'])

test('multipleHomeUnits001',
    [ extra_files(
        [ 'a/', 'b/'
        , 'unitA', 'unitB'])
    ], multiunit_compile, [['unitA', 'unitB'], '-fhide-source-paths'])

test('multipleHomeUnits002',
    [ extra_files(
        [ 'c/', 'd/'
        , 'unitC', 'unitD'])
    ], makefile_test, [])

test('multipleHomeUnits003',
    [ extra_files(
        [ 'a/', 'b/', 'c/', 'd/'
        , 'unitA', 'unitB', 'unitC', 'unitD'])
    ], makefile_test, [])

test('multipleHomeUnits004',
    [ extra_files(
        [ 'b/', 'e/'
        , 'unitB', 'unitE'])
    ], multiunit_compile, [['unitB', 'unitE'], '-fhide-source-paths'])

test('multipleHomeUnits004_recomp',
    [ extra_files(
        [ 'b/', 'e/'
        , 'unitB', 'unitE'])
    ], makefile_test, [])

test('multipleHomeUnitsModuleVisibility',
    [ extra_files(
        [ 'module-visibility/', 'module-visibility-import/'
        , 'unitMV', 'unitMV-import'])
    ], makefile_test, [])

test('multipleHomeUnitsPackageImports',
    [ extra_files(
        [ 'b/', 'b2/', 'package-imports/'
        , 'unitB', 'unitB2',  'unitPI'])
    ], multiunit_compile, [['unitB', 'unitB2', 'unitPI'], '-fhide-source-paths'])

test('MHU_OptionsGHC', normal, compile_fail, [''])

test('multipleHomeUnits_loop', [extra_files([ 'a/', 'unitA', 'loop', 'unitLoop'])], multiunit_compile, [['unitA', 'unitLoop'], '-fhide-source-paths'])

# This test checks that we look for things in our home unit before looking in dependencies
test('multipleHomeUnits_import_order', [extra_files([ 'unitOrder1', 'unitOrder2', 'order-matters1', 'order-matters2'])], multiunit_compile, [['unitOrder1', 'unitOrder2'], '-fhide-source-paths'])

test('multipleHomeUnits_recomp', [copy_files,extra_files([ 'Recomp.hs', 'unitRecomp', 'unitDep', 'Dep.hs'])], makefile_test, [])

test('multipleHomeUnits_recomp_th', [filter_stdout_lines(r'.*Compiling.*'), copy_files, extra_files(['thRecomp.script', 'unitRecompTH', 'unitDep', 'RecompTH.hs', 'Dep.hs', '../../ghci/shell.hs'])  , extra_run_opts('-v1 -unit @unitRecompTH -unit @unitDep')], ghci_script, ['thRecomp.script'])

test('multipleHomeUnits_shared', [extra_files([ 'A.hs', 'unitShared1', 'unitShared2'])], multiunit_compile, [['unitShared1', 'unitShared2'], '-fhide-source-paths'])

test('multipleHomeUnits_shared_ghci', [extra_files([ 'shared.script', 'A.hs', 'unitShared1', 'unitShared2']), extra_run_opts('-unit @unitShared1 -unit @unitShared2')], ghci_script, ['shared.script'])
test('t25139', [extra_files(['t25139/'])], multiunit_compile, [['t25139/u1', 't25139/u2', 't25139/u3', 't25139/u4'], '-v0'])

test('T25122',
    [ extra_files(
        [ 'T25122', 'unitSame1', 'unitSame2'])
    ], multiunit_compile, [['unitSame1', 'unitSame2'], '-v0 -fhide-source-paths -Werror -Wmissing-home-modules'])




