# A test where we depend on the bytecode artefacts from a dependency.
# This test is used to show that `Linkable`s are not retained
# after they have been loaded into the `LoaderState`.
# However, this property is currently not validated automatically.
test('LinkableUsage01'
    , [ collect_compiler_stats('all', 2)
      , extra_files(['genLinkables.sh', 'BCOTemplate.hs'])
      , pre_cmd('$MAKE -s --no-print-directory LinkableUsage01_Prep')
      , req_bco
      , req_th
      ]
    , multimod_compile
    , ['LinkableUsage01', '-package-db bytecode.package.conf -package-id testpkg-1.2.3.4-XXX -fprefer-byte-code ' + config.ghc_th_way_flags])

# Performance test for bytecode `Linkable`s.
test('LinkableUsage02'
    , [ collect_compiler_stats('all', 2)
      , extra_files(['genLinkables.sh', 'BCOTemplate.hs'])
      , pre_cmd('$MAKE -s --no-print-directory LinkableUsage02_Prep')
      , req_bco
      , req_th
      ]
    , multimod_compile
    , ['LinkableUsage02', '-no-link -fbyte-code -fwrite-byte-code -fprefer-byte-code'])

test('linkable-space'
    , [ extra_files(['genLinkables.sh', 'BCOTemplate.hs'])
      , pre_cmd('$MAKE -s --no-print-directory linkable-space_Prep')
      , req_bco
      , req_th
      ]
    , makefile_test
    , ['linkable-space'])
