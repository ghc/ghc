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

test('T26431', [
  extra_files(['../../../.gitlab/hello.hs']),
  extra_hc_opts('-package ghc'),
  extra_run_opts(f'"{config.libdir}"'),
  ignore_stderr]
, compile_and_run, ['']
)
