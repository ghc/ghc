setTestOpts([
  unless(arch('wasm32'), skip),
  only_ways(['ghci', 'ghci-opt']),
  extra_ways(['ghci', 'ghci-opt'])
])

test('T26430', [
  extra_files(['T26430A.c', 'T26430B.c']),
  pre_cmd('$MAKE -s --no-print-directory T26430_setup ghciWayFlags=' + config.ghci_way_flags),
  extra_hc_opts('-L. -lT26430B')]
, compile_and_run, ['']
)
