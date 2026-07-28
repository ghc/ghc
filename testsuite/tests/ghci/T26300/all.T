test('T26300',
     [extra_files(['a/', 'b/', 'c/', 'unitA', 'unitB', 'unitC']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      normalise_slashes,
      req_interp],
     ghci_multiunit_script, [['unitA', 'unitB', 'unitC'], 'T26300.script'])
