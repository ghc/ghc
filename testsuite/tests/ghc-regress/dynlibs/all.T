
test('T3807',
     [extra_clean(['T3807Export_stub.c', 'T3807Export_stub.h',
                   'T3807Export_stub.o',
                   'T3807Export.o', 'T3807Export.hi',
                   'T3807-export.o', 'T3807-load.o',
                   'T3807test.so',
                   'T3807-load']),
      if_os('mingw32', skip),
      if_os('darwin', expect_broken(4264))],
     run_command,
     ['$MAKE --no-print-directory -s T3807'])

