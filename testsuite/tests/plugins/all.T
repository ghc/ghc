setTestOpts(req_interp)

# The implementation of ghc-pkg doesn't seem to be multi-concurrent process safe
# on windows. These tests which mutate the package db need to be run
# sequentially until this is fixed.  This likely means that #13194 isn't fully
# solved.  See also #15313.

test('plugins01',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
     when(opsys('mingw32'), multi_cpu_race),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins01'])

test('plugins02',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
     when(opsys('mingw32'), multi_cpu_race),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins02 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins02/local.package.conf -fplugin Simple.BadlyTypedPlugin -package simple-plugin ' + config.plugin_way_flags])

test('plugins03',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
     when(opsys('mingw32'), multi_cpu_race),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins03 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins03/local.package.conf -fplugin Simple.NonExistentPlugin -package simple-plugin'])

test('plugins04', [extra_files(['HomePackagePlugin.hs']),
                   only_ways([config.ghc_plugin_way]),
                   when(opsys('mingw32'), multi_cpu_race)],
     multimod_compile_fail,
     ['plugins04', '-package ghc -fplugin HomePackagePlugin'])

test('plugins05', [extra_files(['HomePackagePlugin.hs']),
                   when(opsys('mingw32'), multi_cpu_race),
                   only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins05', '-package ghc'])

test('plugins06',
     [extra_files(['LinkerTicklingPlugin.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      unless(have_dynamic(), skip),
      only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins06', '-package ghc'])

test('plugins07',
     [extra_files(['rule-defining-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.plugins07 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins07'])

test('plugins08',
     [extra_files(['simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins08 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins08'])

test('plugins09',
     [extra_files(['simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins09 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins09'])

test('plugins10',
     [expect_broken(15216),
      extra_files(['simple-plugin/', 'QuasiQuotation.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins10 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins10'])

test('plugins11',
     [extra_files(['simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins11 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins11'])

test('plugins12',
     [extra_files(['simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins12 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins12'])

test('plugins13',
     [extra_files(['simple-plugin/', 'PluginFilteredExport.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins13 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins13'])

test('plugins14',
     [extra_files(['simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins14 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins14'])

test('plugins15',
     [extra_files(['simple-plugin/', 'MetaRemoveHelper.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins15 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory plugins15'])

test('T10420',
     [extra_files(['rule-defining-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T10420 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10420'])

test('T10294',
     [extra_files(['annotation-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10294'])

test('T10294a',
     [extra_files(['annotation-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294a TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T10294a'])

test('frontend01', [extra_files(['FrontendPlugin.hs']),
                    only_ways([config.ghc_plugin_way]),
                    when(opsys('mingw32'), multi_cpu_race),
                    unless(have_dynamic(), expect_broken(10301))], run_command,
     ['$MAKE -s --no-print-directory frontend01'])

test('T11244',
     [extra_files(['rule-defining-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T11244 TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T11244'])

test('T12567a',
     [extra_files(['T12567b.hs', 'simple-plugin/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T12567a TOP={top}')],
     run_command, ['$MAKE -s --no-print-directory T12567a'])

test('T14335',
     [extra_files(['simple-plugin/', 'plugins01.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin \
       -fexternal-interpreter -package simple-plugin ' + config.plugin_way_flags])

test('plugin-recomp-pure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     run_command, ['$MAKE -s --no-print-directory plugin-recomp-pure'])

test('plugin-recomp-impure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     run_command, ['$MAKE -s --no-print-directory plugin-recomp-impure'])

test('plugin-recomp-flags',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     run_command, ['$MAKE -s --no-print-directory plugin-recomp-flags'])

test('plugin-recomp-change',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     run_command, ['$MAKE -s --no-print-directory plugin-recomp-change'])

test('plugin-recomp-change-prof',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}'),
      when(not config.have_profiling,skip)
      ],
     run_command, ['$MAKE -s --no-print-directory plugin-recomp-change-prof'])
