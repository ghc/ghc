test('T25510', [
  req_th,
  js_skip
], multimod_compile, ['T25510B', '-fhpc -fbyte-code-and-object-code -fprefer-byte-code -v0'])

test('T25510c', [
  extra_files(['T25510A.hs', 'T25510B.hs']),
  req_th,
  js_skip
], makefile_test, ['T25510c ghcThWayFlags=' + config.ghc_th_way_flags])
