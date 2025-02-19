setTestOpts([
    req_plugins,
    # Plugins aren't supported with the JS backend so we get unexpected passes
    # for tests that expected to fail... Just skipping them for now until #14335
    # is fixed
    js_skip,
    # The implementation of ghc-pkg doesn't seem to be multi-concurrent process
    # safe on windows. These tests which mutate the package db need to be run
    # sequentially until this is fixed.  This likely means that #13194 isn't fully
    # solved.  See also #15313.
    when(opsys('mingw32'), multi_cpu_race),
])


# Check the simple-plugin tests without
# caring about the order of loading of interfaces
def normalizeIfaces(unnormalized_str):
    # Why is this not at the top-level? Probably a python bug which I didn't fancy
    # investigating deeply.
    # NameError: name 'itertools' is not defined. Did you forget to import 'itertools'
    # if placed at the top-level
    import itertools
    iface_plugin_prefix = "interfacePlugin: "
    grouped_plugins = itertools.groupby(
        unnormalized_str.split('\n'),
        lambda l: l.startswith(iface_plugin_prefix)
    )
    return "\n".join([
        line
        for k, lines in grouped_plugins
        for line in (sorted(lines) if k else lines)
    ])

test('plugins01',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     makefile_test, [])

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

test('plugins04',
     [extra_files(['HomePackagePlugin.hs'])],
     multimod_compile_fail,
     ['plugins04', '-package ghc -fplugin HomePackagePlugin'])

test('plugins05', [extra_files(['HomePackagePlugin.hs'])],
     multimod_compile_and_run, ['plugins05', '-package ghc ' + config.plugin_way_flags ])

test('plugins06',
     [extra_files(['LinkerTicklingPlugin.hs']),
      unless(have_dynamic(), skip)],
     multimod_compile_and_run, ['plugins06', '-package ghc -dppr-debug ' + config.plugin_way_flags ])

test('plugins07',
     [extra_files(['rule-defining-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.plugins07 TOP={top}')],
     makefile_test, [])

test('plugins08',
     [extra_files(['simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins08 TOP={top}')],
     makefile_test, [])

test('plugins09',
     [extra_files(['simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins09 TOP={top}')],
     makefile_test, [])

test('plugins10',
     [extra_files(['simple-plugin/', 'QuasiQuotation.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins10 TOP={top}')],
     makefile_test, [])

test('plugins11',
     [extra_files(['simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins11 TOP={top}')],
     makefile_test, [])

test('plugins12',
     [extra_files(['simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins12 TOP={top}')],
     makefile_test, [])

test('plugins13',
     [extra_files(['simple-plugin/', 'PluginFilteredExport.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins13 TOP={top}')],
     makefile_test, [])

test('plugins14',
     [extra_files(['simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins14 TOP={top}')],
     makefile_test, [])

test('plugins15',
     [extra_files(['simple-plugin/', 'MetaRemoveHelper.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins15 TOP={top}')],
     makefile_test, [])

test('T10420',
     [extra_files(['rule-defining-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T10420 TOP={top}')],
     makefile_test, [])

test('T10294',
     [extra_files(['annotation-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294 TOP={top}')],
     makefile_test, [])

test('T10294a',
     [extra_files(['annotation-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C annotation-plugin package.T10294a TOP={top}')],
     makefile_test, [])

test('frontend01', [extra_files(['FrontendPlugin.hs'])],
     makefile_test, [])

test('T11244',
     [extra_files(['rule-defining-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.T11244 TOP={top}')],
     makefile_test, [])

test('T12567a',
     [extra_files(['T12567b.hs', 'simple-plugin/']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T12567a TOP={top}')],
     makefile_test, [])

test('T14335',
     [extra_files(['simple-plugin/', 'plugins01.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01 TOP={top}')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin \
       -fexternal-interpreter -package simple-plugin ' + config.plugin_way_flags])

test('plugin-recomp-pure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-impure',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-flags',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-change',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugin-recomp-change-prof',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),

      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}'),
      when(not config.have_profiling,skip)
      ],
     makefile_test, [])

test('static-plugins',
     [extra_files(['simple-plugin/']),
      unless(config.have_RTS_linker, skip),
      expect_broken_for(16803, prof_ways),
      extra_run_opts('"' + config.libdir + '"'),
      normalise_fun(normalizeIfaces)
      ],
     compile_and_run,
     ['-package ghc -isimple-plugin/ -j1'])

test('T25217',
     [extra_files(['static-plugins-module.hs']),
      unless(config.have_RTS_linker, skip),
      expect_broken_for(16803, prof_ways),
      extra_run_opts('"' + config.libdir + '"'),
      ],
     compile_and_run,
     ['-package ghc -j1'])

test('T15858',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
#
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

test('T19926',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T19926 TOP={top}')
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

test('plugins-ghci', [extra_files(['HomePackagePlugin.hs']), only_ways(['ghci'])], compile_and_run, ['-package ghc'])

test('plugin-recomp-change-2',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs', 'PluginRecompTest.hs']),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])


test('T20417',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('T20218',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T20218 TOP={top}'),
      grep_errmsg(r'-lHSsimple-plugin'),
      ignore_stdout
      ],
     makefile_test, [])

test('T20218b',
     [extra_files(['simple-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.T20218b TOP={top}')
      ],
     makefile_test, [])

test('test-defaulting-plugin',
     [extra_files(['defaulting-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C defaulting-plugin package.test-defaulting-plugin TOP={top}')],
     makefile_test, [])

test('test-defaulting-plugin-fail',
     [extra_files(['defaulting-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C defaulting-plugin package.test-defaulting-plugin-fail TOP={top}')],
     makefile_test, [])

test('T23821',
     [extra_files(['defaulting-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C defaulting-plugin package.test-defaulting-plugin TOP={top}')],
     makefile_test, [])

test('T23832',
     [extra_files(['defaulting-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C defaulting-plugin package.test-defaulting-plugin TOP={top}')],
     makefile_test, [])

test('T23832_invalid',
     [extra_files(['defaulting-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C defaulting-plugin package.test-defaulting-plugin TOP={top}')],
     makefile_test, [])

test('plugins-order',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('plugins-order-pragma',
     [extra_files(['plugin-recomp/', 'plugin-recomp-test.hs']),
      pre_cmd('$MAKE -s --no-print-directory -C plugin-recomp package.plugins01 TOP={top}')
      ],
     makefile_test, [])

test('T20803a',
     [extra_files(['T20803-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C T20803-plugin package.T20803a TOP={top}')],
     compile,
     ['-package-db T20803-plugin/pkg.T20803a/local.package.conf -fplugin FixErrorsPlugin -package T20803-plugin ' + config.plugin_way_flags])

test('T20803b',
     [extra_files(['T20803-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C T20803-plugin package.T20803b TOP={top}')],
     compile_fail,
     ['-package-db T20803-plugin/pkg.T20803b/local.package.conf -fplugin AddErrorPlugin -package T20803-plugin ' + config.plugin_way_flags])

test('test-echo-in-turn',
     [extra_files(['echo-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C echo-plugin package.test-echo-in-turn TOP={top}')],
     makefile_test, [])

test('test-echo-in-line',
     [extra_files(['echo-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C echo-plugin package.test-echo-in-line TOP={top}')],
     makefile_test, [])

test('test-echo-in-turn-many-args',
     [extra_files(['echo-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C echo-plugin package.test-echo-in-turn-many-args TOP={top}')],
     makefile_test, [])

test('test-echo-in-line-many-args',
     [extra_files(['echo-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C echo-plugin package.test-echo-in-line-many-args TOP={top}')],
     makefile_test, [])

test('plugins-external',
     [extra_files(['shared-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C shared-plugin package.plugins01 TOP={top}'),
      when(opsys('mingw32') or (opsys('linux') and not ghc_dynamic()), expect_broken(20706))],
     makefile_test, [])

test('test-phase-hooks-plugin',
     [extra_files(['hooks-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C hooks-plugin package.test-phase-hooks-plugin TOP={top}')],
     compile,
     ['-package-db hooks-plugin/pkg.test-phase-hooks-plugin/local.package.conf -fplugin Hooks.PhasePlugin -package hooks-plugin ' + config.plugin_way_flags])

test('test-log-hooks-plugin',
     [extra_files(['hooks-plugin/']),
      pre_cmd('$MAKE -s --no-print-directory -C hooks-plugin package.test-log-hooks-plugin TOP={top}')],
     compile_fail,
     ['-package-db hooks-plugin/pkg.test-log-hooks-plugin/local.package.conf -fplugin Hooks.LogPlugin -package hooks-plugin ' + config.plugin_way_flags])

test('test-late-plugin',
     [extra_files(['late-plugin/LatePlugin.hs']), ignore_stdout],
     makefile_test,
     [])
