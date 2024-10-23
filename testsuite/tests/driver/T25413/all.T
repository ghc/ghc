test('T25413', [
  req_th,
  req_c,
  extra_files(['A.hs'])
], makefile_test, ['T25413 ghcThWayFlags=' + config.ghc_th_way_flags])
