# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('dsrun001', normal, compile_and_run, [''])
test('dsrun002', normal, compile_and_run, [''])
test('dsrun003', normal, compile_and_run, [''])
test('dsrun004', normal, compile_and_run, [''])
test('dsrun005', exit_code(1), compile_and_run, [''])
test('dsrun006', normal, compile_and_run, [''])
test('dsrun007', exit_code(1), compile_and_run, [''])
test('dsrun008', exit_code(1), compile_and_run, [''])
test('dsrun009', normal, compile_and_run, [''])
test('dsrun010', normal, compile_and_run, [''])
test('dsrun011', when(fast(), skip), compile_and_run, [''])
test('dsrun012', when(fast(), skip), compile_and_run, [''])
test('dsrun013', normal, compile_and_run, [''])
test('dsrun014', expect_broken_for(14901,
                   ['hpc', 'dyn', 'optasm', 'optllvm', 'threaded2']
		 ), compile_and_run, ['-fobject-code'])
test('dsrun015', normal, compile_and_run, [''])
test('dsrun016', normal, compile_and_run, [''])
test('dsrun017', normal, compile_and_run, [''])
test('dsrun018', normal, compile_and_run, [''])
test('dsrun019', normal, compile_and_run, [''])
test('dsrun020', normal, compile_and_run, [''])
test('dsrun021', normal, compile_and_run, [''])
test('dsrun022', normal, compile_and_run, [''])
test('dsrun023', normal, compile_and_run, [''])
test('T246', normal, compile_and_run, [''])
test('T3126', normal, compile_and_run, [''])
test('T3382', normal, compile_and_run, [''])
test('mc01', normal, compile_and_run, [''])
test('mc02', normal, compile_and_run, [''])
test('mc03', normal, compile_and_run, [''])

# #5522: "I noticed that test mc03 runs out of memory compiled with
#         -O -fliberate-case -fspec-constr."
test('T5522', [only_ways(['optasm']), extra_files(['mc03.hs'])], multimod_compile,
     ['mc03', '-fliberate-case -fspec-constr -v0 +RTS -M1G -RTS'])

test('mc04', normal, compile_and_run, [''])
test('mc05', normal, compile_and_run, [''])
test('mc06', normal, compile_and_run, [''])
test('mc07', normal, compile_and_run, [''])
test('mc08', normal, compile_and_run, [''])
test('T5742', normal, compile_and_run, [''])
test('DsLambdaCase', [], compile_and_run, [''])
test('DsMultiWayIf', [], compile_and_run, [''])
test('DsStaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])
test('T8952', normal, compile_and_run, [''])
test('T9238', normal, compile_and_run, [''])
test('T9844', normal, compile_and_run, [''])
test('T10215', normal, compile_and_run, [''])
test('DsStrictData', normal, compile_and_run, [''])
test('DsStrict', normal, compile_and_run, [''])
test('DsStrictLet', normal, compile_and_run, ['-O'])
test('DsPostfixOperators', normal, compile_and_run, [''])
test('T11193', exit_code(1), compile_and_run, [''])
test('T11572', exit_code(1), compile_and_run, [''])
test('T11601', exit_code(1), compile_and_run, [''])
test('T11747', normal, compile_and_run, ['-dcore-lint'])
test('T12595', normal, compile_and_run, [''])
test('T13285', normal, compile_and_run, [''])
test('T18151', normal, compile_and_run, [''])
test('T18151x', normal, compile_and_run, [''])
test('T18172', [], ghci_script, ['T18172.script'])

test('DsDoExprFailMsg', exit_code(1), compile_and_run, [''])
test('DsMonadCompFailMsg', exit_code(1), compile_and_run, [''])
test('T19289', normal, compile_and_run, [''])
test('T19680', normal, compile_and_run, [''])
test('T19680A', normal, compile_and_run, [''])
test('T20024', exit_code(1), compile_and_run, [''])

test('Or5', exit_code(1), compile_and_run, [''])
