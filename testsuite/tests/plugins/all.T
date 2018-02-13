setTestOpts(req_interp)

test('plugins01',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins01'])

test('plugins02',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins02 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins02/local.package.conf -fplugin Simple.BadlyTypedPlugin -package simple-plugin ' + config.plugin_way_flags])

test('plugins03',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins03 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins03/local.package.conf -fplugin Simple.NonExistentPlugin -package simple-plugin'])

test('plugins04', [extra_files(['HomePackagePlugin.hs'])], multimod_compile_fail,
     ['plugins04', '-package ghc -fplugin HomePackagePlugin'])

test('plugins05', [extra_files(['HomePackagePlugin.hs']),
                   only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins05', '-package ghc'])

test('plugins06',
     [extra_files(['LinkerTicklingPlugin.hs']),
      unless(have_dynamic(), skip), only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins06', '-package ghc'])

test('plugins07',
     [extra_files(['rule-defining-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.plugins07 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins07'])

test('plugins08',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins08 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins08'])

test('T10420',
     [extra_files(['rule-defining-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T10420 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10420'])

test('T10294',
     [extra_files(['annotation-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10294'])

test('T10294a',
     [extra_files(['annotation-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294a TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10294a'])

test('frontend01', [extra_files(['FrontendPlugin.hs']),
                    unless(have_dynamic(), expect_broken(10301))], run_command,
     ['$MAKE -s --no-print-directory frontend01'])

test('T11244',
     [extra_files(['rule-defining-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T11244 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T11244'])

test('T12567a',
     [extra_files(['T12567b.hs', 'simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T12567a TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T12567a'])

test('T14335',
     [extra_files(['simple-plugin/', 'plugins01.hs']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}'),
      expect_broken(14335)],
     run_command, ['$MAKE -s --no-print-directory T14335'])
