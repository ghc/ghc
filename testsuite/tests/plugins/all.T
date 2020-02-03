setTestOpts([
    req_interp,
    # The implementation of ghc-pkg doesn't seem to be multi-concurrent process
    # safe on windows. These tests which mutate the package db need to be run
    # sequentially until this is fixed.  This likely means that #13194 isn't fully
    # solved.  See also #15313.
    when(opsys('mingw32'), multi_cpu_race),
    # Unfortunately even with the above these tests are incredibly flaky on Windows.
    # In any given testsuite run at least half a dozen fail for one reason of another
    # (typically a cmmbination of timeouts, some missing static archive errors from ld,
    # and a few plain crashes).
    when(opsys('mingw32'), fragile(16405))
])


test('plugins01',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     makefile_test, [])

test('plugins02',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins02 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins02/local.package.conf -fplugin Simple.BadlyTypedPlugin -package simple-plugin ' + config.plugin_way_flags])

test('plugins03',
     [extra_files(['simple-plugin/']), only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins03 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins03/local.package.conf -fplugin Simple.NonExistentPlugin -package simple-plugin'])

test('plugins04',
     [extra_files(['HomePackagePlugin.hs']),
      only_ways([config.ghc_plugin_way])],
     multimod_compile_fail,
     ['plugins04', '-package ghc -fplugin HomePackagePlugin'])

test('plugins05', [extra_files(['HomePackagePlugin.hs']),
                   only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins05', '-package ghc'])

test('plugins06',
     [extra_files(['LinkerTicklingPlugin.hs']),
      unless(have_dynamic(), skip),
      only_ways([config.ghc_plugin_way])],
     multimod_compile_and_run, ['plugins06', '-package ghc'])

test('plugins07',
     [extra_files(['rule-defining-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.plugins07 TOP={top}')],
     makefile_test, [])

test('plugins08',
     [extra_files(['simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins08 TOP={top}')],
     makefile_test, [])

test('plugins09',
     [extra_files(['simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins09 TOP={top}')],
     makefile_test, [])

test('plugins10',
     [extra_files(['simple-plugin/', 'QuasiQuotation.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins10 TOP={top}')],
     makefile_test, [])

test('plugins11',
     [extra_files(['simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins11 TOP={top}')],
     makefile_test, [])

test('plugins12',
     [extra_files(['simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins12 TOP={top}')],
     makefile_test, [])

test('plugins13',
     [extra_files(['simple-plugin/', 'PluginFilteredExport.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins13 TOP={top}')],
     makefile_test, [])

test('plugins14',
     [extra_files(['simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins14 TOP={top}')],
     makefile_test, [])

test('plugins15',
     [extra_files(['simple-plugin/', 'MetaRemoveHelper.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins15 TOP={top}')],
     makefile_test, [])

test('T10420',
     [extra_files(['rule-defining-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T10420 TOP={top}')],
     makefile_test, [])

test('T10294',
     [extra_files(['annotation-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294 TOP={top}')],
     makefile_test, [])

test('T10294a',
     [extra_files(['annotation-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294a TOP={top}')],
     makefile_test, [])

test('frontend01', [extra_files(['FrontendPlugin.hs']),
                    only_ways([config.ghc_plugin_way]),
                    unless(have_dynamic(), expect_broken(10301))],
     makefile_test, [])

test('T11244',
     [extra_files(['rule-defining-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T11244 TOP={top}')],
     makefile_test, [])

test('T12567a',
     [extra_files(['T12567b.hs', 'simple-plugin/']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T12567a TOP={top}')],
     makefile_test, [])

test('T14335',
     [extra_files(['simple-plugin/', 'plugins01.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin \
       -fexternal-interpreter -package simple-plugin ' + config.plugin_way_flags])

test('plugin-recomp-pure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-impure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-flags',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-change',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-change-prof',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}'),
      when(not config.have_profiling,skip)
      ],
     makefile_test, [])

test('static-plugins',
     [extra_files(['simple-plugin/']),
      unless(config.have_RTS_linker, skip),
      expect_broken_for(16803, prof_ways),
      extra_run_opts('"' + config.libdir + '"')],
     compile_and_run,
     ['-package ghc -isimple-plugin/'])

test('T15858',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
#      only_ways([config.ghc_plugin_way]),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db plugin-recomp/pkg.plugins01/local.package.conf ")
      ],
     ghci_script, ['T15858.script'])

test('T16104',
     [extra_files(['T16104-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C T16104-plugin package.T16104-plugin TOP={top}')
      ],
     makefile_test, [])


test('T16260',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T16260 TOP={top}')
      ],
     makefile_test, [])

test('test-hole-plugin',
     [extra_files(['hole-fit-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C hole-fit-plugin package.hole-fit-plugin TOP={top}'),
      extra_hc_opts('-package-db hole-fit-plugin/pkg.hole-fit-plugin/local.package.conf '+ config.plugin_way_flags),
      req_th
      ],
     compile, ['-fdefer-typed-holes'])
test('test-hooks-plugin',
     [extra_files(['hooks-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C hooks-plugin package.hooks-plugin TOP={top}'),
      # The following doesn't seem to work, even though it
      # seems identical to the previous test...?
      # extra_hc_opts('-package-db hooks-plugin/pkg.hooks-plugin/local.package.conf '+ config.plugin_way_flags),
      req_th
      ],
     compile_and_run,
     ['-package-db hooks-plugin/pkg.hooks-plugin/local.package.conf '+ config.plugin_way_flags])
