setTestOpts([skip, extra_run_opts('-ignore-dot-ghci'),
             when(config.have_ext_interp, extra_ways(['ghci-ext'])), # test with -fexternal-interpreter
             normalise_slashes])


test('print001', [skip, normal], ghci_script, ['print001.script'])
test('print002', [skip, extra_files(['../Test.hs'])], ghci_script, ['print002.script'])
test('print003', [skip, extra_files(['../Test.hs'])], ghci_script, ['print003.script'])
test('print004', [skip, normal], ghci_script, ['print004.script'])
test('print005', [skip, extra_files(['../QSort.hs'])], ghci_script, ['print005.script'])
test('print006', [skip, extra_files(['../Test.hs'])], ghci_script, ['print006.script'])
test('print007', [skip, extra_files(['../Test.hs'])], ghci_script, ['print007.script'])
test('print008', [skip, extra_files(['../Test.hs'])], ghci_script, ['print008.script'])
test('print009', [skip, normal], ghci_script, ['print009.script'])
test('print010', [skip, extra_files(['../Test.hs'])], ghci_script, ['print010.script'])
test('print011', [skip, extra_files(['../Test.hs'])], ghci_script, ['print011.script'])
test('print012', [skip, extra_files(['../GADT.hs', '../Test.hs'])], ghci_script, ['print012.script'])
test('print013', [skip, extra_files(['../GADT.hs'])], ghci_script, ['print013.script'])
test('print014', [skip, extra_files(['../GADT.hs'])], ghci_script, ['print014.script'])
test('print016', [skip, extra_files(['../Test.hs'])], ghci_script, ['print016.script'])
test('print017', [skip, extra_files(['../Test.hs'])], ghci_script, ['print017.script'])
test('print018', [skip, extra_files(['../Test.hs'])], ghci_script, ['print018.script'])
test('print019', [skip, extra_files(['../Test.hs'])], ghci_script, ['print019.script'])

# The ghci-ext way emits messages in a slightly different order due to
# printing from two processes, so let's just skip it.
test('print020', [skip, extra_files(['../HappyTest.hs']),
                  omit_ways(['ghci-ext'])], ghci_script, ['print020.script'])

test('print021', [skip, normal], ghci_script, ['print021.script'])
test('print022',
     [skip, when(arch('powerpc64'), expect_broken(14455))],
     ghci_script, ['print022.script'])
test('print023', [skip, extra_files(['../Test.hs'])], ghci_script, ['print023.script'])
test('print024', [skip, extra_files(['../Test.hs'])], ghci_script, ['print024.script'])
test('print025', [skip, normal], ghci_script, ['print025.script'])
test('print026', [skip, normal], ghci_script, ['print026.script'])
test('print027', [skip, normal], ghci_script, ['print027.script'])
test('print028', [skip, normal], ghci_script, ['print028.script'])
test('print029', [skip, normal], ghci_script, ['print029.script'])
test('print030', [skip, extra_files(['print029.hs'])], ghci_script, ['print030.script'])
test('print031', [skip, normal], ghci_script, ['print031.script'])
test('print032', [skip, extra_files(['print029.hs'])], ghci_script, ['print032.script'])
test('print033', [skip, normal], ghci_script, ['print033.script'])
test('print034', [skip, extra_files(['../GADT.hs', '../Test.hs'])], ghci_script, ['print034.script'])
test('print035', [skip, extra_files(['../Unboxed.hs'])], ghci_script, ['print035.script'])
test('print036', [skip, expect_broken(9046)], ghci_script, ['print036.script'])
test('print037', [skip, when(llvm_build() and integer_gmp(),
                      fragile_for(16205, ['ghci-ext']))]
               , ghci_script
               , ['print037.script'])

test('break001', [skip, extra_files(['../Test2.hs'])], ghci_script, ['break001.script'])
test('break002', [skip, extra_files(['../Test2.hs'])], ghci_script, ['break002.script'])
test('break003', [skip, extra_files(['../Test3.hs'])], ghci_script, ['break003.script'])
test('break005', [skip, extra_files(['../QSort.hs'])], ghci_script, ['break005.script'])
test('break006', [skip, extra_files(['../Test3.hs'])], ghci_script, ['break006.script'])
test('break007', [skip, extra_files(['Break007.hs'])], ghci_script, ['break007.script'])
test('break008', [skip, extra_files(['../Test3.hs'])], ghci_script, ['break008.script'])
test('break009', [skip, extra_files(['../Test6.hs']),
                  combined_output], ghci_script, ['break009.script'])
test('break010', [skip, extra_files(['../Test6.hs'])], ghci_script, ['break010.script'])
test('break011',
     [skip, extra_files(['../Test7.hs']),
      combined_output],
     ghci_script, ['break011.script'])
test('break012', [skip, normal], ghci_script, ['break012.script'])
test('break013', [skip, normal], ghci_script, ['break013.script'])
test('break014', [skip, normal], ghci_script, ['break014.script'])
test('break015', [skip, expect_broken(1532)], ghci_script, ['break015.script'])
test('break016', [skip, combined_output], ghci_script, ['break016.script'])
test('break017', [skip, extra_files(['../QSort.hs']),
                  combined_output], ghci_script, ['break017.script'])
test('break018', [skip, extra_files(['../mdo.hs'])], ghci_script, ['break018.script'])
test('break019', [skip, extra_files(['../Test2.hs'])], ghci_script, ['break019.script'])
test('break020', [skip, extra_files(['Break020b.hs'])], ghci_script, ['break020.script'])
test('break021', [skip, extra_files(['Break020b.hs', 'break020.hs'])], ghci_script, ['break021.script'])
test('break024', [skip, combined_output], ghci_script, ['break024.script'])
test('break025', [skip, normal], ghci_script, ['break025.script'])
test('break026', [skip, normal], ghci_script, ['break026.script'])
test('break027', [skip, extra_files(['../QSort.hs'])], ghci_script, ['break027.script'])
test('dynbrk001', [skip, extra_files(['../QSort.hs'])], ghci_script, ['dynbrk001.script'])
test('dynbrk002', [skip, extra_files(['../QSort.hs'])], ghci_script, ['dynbrk002.script'])
test('dynbrk003', [skip, normal], ghci_script, ['dynbrk003.script'])
test('dynbrk004', [skip, extra_files(['../mdo.hs'])], ghci_script, ['dynbrk004.script'])
test('dynbrk005', [skip, extra_files(['TupleN.hs']), expect_broken(1530)],
     ghci_script, ['dynbrk005.script'])
test('dynbrk007', [skip, normal], ghci_script, ['dynbrk007.script'])
test('dynbrk008', [skip, normal], ghci_script, ['dynbrk008.script'])
test('dynbrk009', [skip, normal], ghci_script, ['dynbrk009.script'])

test('result001', [skip, normal], ghci_script, ['result001.script'])

test('listCommand001', [skip, extra_files(['../Test3.hs']),
                        combined_output], ghci_script, ['listCommand001.script'])
test('listCommand002', [skip, normal], ghci_script, ['listCommand002.script'])

test('hist001', [skip, extra_files(['../Test3.hs']), extra_run_opts('+RTS -I0')],
     ghci_script, ['hist001.script'])
test('hist002', [skip, extra_files(['../Test3.hs']), extra_run_opts('+RTS -I0')],
     ghci_script, ['hist002.script'])

test('T1620', [skip, extra_files(['T1620/', 'T1620/T1620.hs'])],
              ghci_script, ['T1620.script'])
test('T2740', [skip, normal], ghci_script, ['T2740.script'])

test('getargs', [skip, extra_files(['../getargs.hs'])], ghci_script, ['getargs.script'])
test('T7386', [skip, normal], ghci_script, ['T7386.script'])
test('T8487', [skip, normal], ghci_script, ['T8487.script'])
test('T8557', [skip, normal], ghci_script, ['T8557.script'])
test('T12458', [skip, normal], ghci_script, ['T12458.script'])
test('T13825-debugger', [skip, when(arch('powerpc64'), expect_broken(14455))],
      ghci_script, ['T13825-debugger.script'])
test('T14690', [skip, normal], ghci_script, ['T14690.script'])
test('T16700', [skip, normal], ghci_script, ['T16700.script'])

test('break029', [skip, extra_files(['break029.hs'])], ghci_script, ['break029.script'])
test('T2215', [skip, normal], ghci_script, ['T2215.script'])
