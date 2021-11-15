# Ensure that RTS flags past -- get ignored 

test('T13287', [when(opsys("OSJavaScript"), skip), extra_run_opts('a1 +RTS -RTS -- a2 +RTS -RTS a3'), omit_ways(['ghci'])], compile_and_run, [''])

