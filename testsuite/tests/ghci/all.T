test('ghci-mem-primops', [  extra_ways(['ghci-opt']), only_ways(['ghci', 'ghci-opt']),
                            extra_hc_opts('-fno-unoptimized-core-for-interpreter -O')], ghci_script, ['ghci-mem-primops.script'])
