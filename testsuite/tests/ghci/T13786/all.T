test('T13786',
     [when(cross(), skip), # <command line>: not built for interactive use
      when(unregisterised(), fragile(17018))],
     makefile_test, [])
