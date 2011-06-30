def f(opts):
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(compose(alone, if_compiler_lt('ghc', '7.1', skip)))

test('plugins01', normal, run_command, ['$MAKE -s --no-print-directory plugins01'])
test('plugins02', normal, compile_fail, ['-package-conf simple-plugin/local.package.conf -fplugin Simple.BadlyTypedPlugin -package simple-plugin'])
test('plugins03', normal, compile_fail, ['-package-conf simple-plugin/local.package.conf -fplugin Simple.NonExistantPlugin -package simple-plugin'])

test('plugins04', extra_clean(['HomePackagePlugin.hi', 'HomePackagePlugin.o']), multimod_compile_fail, ['plugins04', '-package ghc -fplugin HomePackagePlugin'])
test('plugins05', extra_clean(['HomePackagePlugin.hi', 'HomePackagePlugin.o']), multimod_compile_and_run, ['plugins05', '-package ghc'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
