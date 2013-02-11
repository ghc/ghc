
test('T3807',
     [req_shared_libs,
      extra_clean(['T3807Export_stub.c', 'T3807Export_stub.h',
                   'T3807Export_stub.o',
                   'T3807Export.o', 'T3807Export.hi',
                   'T3807-export.o', 'T3807-load.o',
                   'T3807test.so',
                   'T3807-load']),
      when(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE --no-print-directory -s T3807'])

test('T4464',
     [req_shared_libs,
      extra_clean(['T4464B.o', 'T4464C.o', 'T4464H.hi', 'T4464H.o',
                   'T4464H_stub.c', 'T4464H_stub.h', 'T4464H_stub.o',
                   'HS4464.dll', 'HS4464.dll.a', 't4464.exe']),
      unless(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE --no-print-directory -s T4464'])

test('T5373',
     [req_shared_libs,
      extra_clean(['T5373A.hi',  'T5373B.hi',  'T5373C.hi',  'T5373D.hi',
                   'T5373A.o',   'T5373B.o',   'T5373C.o',   'T5373D.o',
                   'T5373A',     'T5373B',     'T5373C',     'T5373D',
                   'T5373A.exe', 'T5373B.exe', 'T5373C.exe', 'T5373D.exe'])
     ],
     run_command,
     ['$MAKE --no-print-directory -s T5373'])

