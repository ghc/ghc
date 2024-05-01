test('T24474-cmm-override-g0',
     [cmm_src,
      check_errmsg('GHC-75725'),
      # Relies on GCC, as this is a GCC-related workaround.
      unless(gcc_as_cmmp(), fragile(0))],
     compile_fail,
     ['-optCmmP-g3'])
