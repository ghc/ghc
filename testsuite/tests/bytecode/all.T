ghci_dump_bcos = [only_ways(['ghci']), extra_run_opts('-dno-typeable-binds -dsuppress-uniques -ddump-bcos')]

test('T23068', ghci_dump_bcos + [filter_stdout_lines(r'.*bitmap: .*')], ghci_script, ['T23068.script'])

test('T25975', extra_ways(ghci_ways), compile_and_run,
     # Some of the examples work more robustly with these flags
     ['-fno-break-points -fno-full-laziness'])

test('T26565', extra_files(["T26565.hs"]), ghci_script, ['T26565.script'])
test('T23973', extra_files(["T23973.hs"]), ghci_script, ['T23973.script'])

# Nullary data constructors
test('T26216', extra_files(["T26216_aux.hs"]), ghci_script, ['T26216.script'])
