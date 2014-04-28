# args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# -----------------------------------------------------------------------------
# Pick an arbitrary few to run when 'fast' is on

test('tcrun001', normal, compile_and_run, [''])
test('tcrun002', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun003', normal, compile_and_run, [''])
test('tcrun004', normal, compile_and_run, [''])
test('tcrun005', normal, compile_and_run, [''])
test('Defer01', normal, compile_and_run, [''])

# -----------------------------------------------------------------------------
# Skip everything else if fast is on

def f(name, opts):
  if config.fast:
	opts.skip = 1
setTestOpts(f)

test('tcrun006', normal, compile_and_run, [''])
test('tcrun008', normal, compile_and_run, [''])
test('tcrun009', normal, compile_and_run, [''])
test('tcrun010', normal, compile_and_run, [''])
test('tcrun011', normal, compile_and_run, [''])
test('tcrun012', normal, compile_and_run, [''])
test('tcrun013', normal, compile_and_run, [''])
test('tcrun014', normal, compile_and_run, [''])
test('tcrun015', normal, compile_and_run, ['-O'])
test('tcrun016', normal, compile_and_run, [''])
test('tcrun017', normal, compile_and_run, [''])
test('tcrun018', normal, compile_and_run, [''])
test('tcrun019', normal, compile_and_run, [''])
test('tcrun020', normal, compile_and_run, [''])
# Doesn't work with External Core due to datatype with no constructors
test('tcrun021', expect_fail_for(['extcore','optextcore']),
     compile_and_run, ['-package containers'])
test('tcrun022', [omit_ways(['ghci']),only_compiler_types(['ghc'])],
     compile_and_run, ['-O'])
test('tcrun023', normal, compile_and_run, ['-O'])
test('tcrun024', normal, compile_and_run, ['-O'])
test('tcrun025', extra_clean(['TcRun025_B.hi', 'TcRun025_B.o']),
     multimod_compile_and_run, ['tcrun025',''])
test('tcrun026', normal, compile_and_run, [''])
test('tcrun027', normal, compile_and_run, [''])
# Doesn't work with External Core due to datatype with no constructors
test('tcrun028', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])
test('tcrun029', normal, compile_and_run, [''])
test('tcrun030', normal, compile_and_run, [''])
test('tcrun031', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun032', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun033', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun034', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun035', only_compiler_types(['ghc']), compile_fail, [''])
test('tcrun036', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun037', only_compiler_types(['ghc']), compile_and_run, [''])

test('tcrun038',
     extra_clean(['TcRun038_B.hi', 'TcRun038_B.o']),
     multimod_compile_and_run,
     ['tcrun038',''])

test('tcrun039', only_compiler_types(['ghc']), compile_and_run, [''])
test('tcrun040', normal, compile_and_run, [''])
test('tcrun041', omit_ways(['ghci']), compile_and_run, [''])
test('tcrun042', normal, compile_and_run, [''])
test('tcrun043', normal, compile_and_run, [''])
test('tcrun044', normal, compile_and_run, [''])
test('tcrun045', normal, compile_fail, [''])
test('tcrun046', normal, compile_and_run, [''])
test('tcrun047', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('tcrun048', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('tcrun049', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('tcrun050', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('tcrun051', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])

test('church', normal, compile_and_run, [''])
test('testeq2', normal, compile_and_run, [''])
test('T1624', normal, compile_and_run, [''])
test('IPRun', normal, compile_and_run, [''])

# Support files for T1735 are in directory T1735_Help/
test('T1735', normal, multimod_compile_and_run, ['T1735',''])

test('T3731', normal, compile_and_run, [''])
test('T3731-short', normal, compile_and_run, [''])
test('T3500a', normal, compile_and_run, [''])
test('T3500b', normal, compile_and_run, [''])
test('T4809', reqlib('mtl'), compile_and_run, [''])
test('T2722', normal, compile_and_run, [''])
test('mc17', normal, compile_and_run, [''])
test('T5759', normal, compile_and_run, [''])
test('T5573a', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('T5573b', [omit_ways(['ghci']), only_compiler_types(['ghc'])],
     compile_and_run, [''])
test('T7023', normal, compile_and_run, [''])
test('T7126', normal, compile_and_run, [''])
test('T6117', normal, compile_and_run, [''])
test('T5751', normal, compile_and_run, [''])
test('T5913', normal, compile_and_run, [''])
test('T7748', normal, compile_and_run, [''])
test('TcNullaryTC', when(compiler_lt('ghc', '7.7'), skip), compile_and_run, [''])
test('T7861', exit_code(1), compile_and_run, [''])
test('TcTypeNatSimpleRun', normal, compile_and_run, [''])
test('TcCoercible', when(compiler_lt('ghc', '7.7'), skip), compile_and_run, [''])
test('T8119', normal, ghci_script, ['T8119.script'])
test('T8492', normal, compile_and_run, [''])
test('T8739', normal, compile_and_run, [''])
