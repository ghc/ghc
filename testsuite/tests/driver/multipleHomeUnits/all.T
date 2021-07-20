test('multipleHomeUnits_single1', [extra_files([ 'a/', 'unitA'])], multiunit_compile, [['unitA'], '-fhide-source-paths'])
test('multipleHomeUnits_single2', [extra_files([ 'b/', 'unitB'])], multiunit_compile, [['unitB'], '-fhide-source-paths'])
test('multipleHomeUnits_single3', [extra_files([ 'c/', 'unitC'])], multiunit_compile, [['unitC'], '-fhide-source-paths'])
test('multipleHomeUnits_single4', [extra_files([ 'd/', 'unitD'])], multiunit_compile, [['unitD'], '-fhide-source-paths'])
test('multipleHomeUnits_single5', [extra_files([ 'th/', 'unitTH'])], multiunit_compile, [['unitTH'], '-fhide-source-paths'])
test('multipleHomeUnits_cpp', [extra_files([ 'cpp-includes/', 'unitCPPIncludes'])], multiunit_compile, [['unitCPPIncludes'], '-fhide-source-paths'])
test('multipleHomeUnits_cfile', [extra_files([ 'c-file/', 'unitCFile'])], multiunit_compile, [['unitCFile'], '-fhide-source-paths'])
test('multipleHomeUnits_callstack', [extra_files([ 'callstack/', 'unitCallstack'])], makefile_test, [])

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
