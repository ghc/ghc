setTestOpts([extra_run_opts('-ignore-dot-ghci'),
             when(config.have_ext_interp, extra_ways(['ghci-ext'])), # test with -fexternal-interpreter
             normalise_slashes])


test('print001', normal, ghci_script, ['print001.script'])
test('print002', [extra_files(['../Test.hs']), when(arch('arm'), fragile(17557))], ghci_script, ['print002.script'])
test('print003', extra_files(['../Test.hs']), ghci_script, ['print003.script'])
test('print004', normal, ghci_script, ['print004.script'])
test('print005', extra_files(['../QSort.hs']), ghci_script, ['print005.script'])
test('print006', extra_files(['../Test.hs']), ghci_script, ['print006.script'])
test('print007', extra_files(['../Test.hs']), ghci_script, ['print007.script'])
test('print008', extra_files(['../Test.hs']), ghci_script, ['print008.script'])
test('print009', normal, ghci_script, ['print009.script'])
test('print010', extra_files(['../Test.hs']), ghci_script, ['print010.script'])
test('print011', extra_files(['../Test.hs']), ghci_script, ['print011.script'])
test('print012', extra_files(['../GADT.hs', '../Test.hs']), ghci_script, ['print012.script'])
test('print013', extra_files(['../GADT.hs']), ghci_script, ['print013.script'])
test('print014', extra_files(['../GADT.hs']), ghci_script, ['print014.script'])
test('print016', extra_files(['../Test.hs']), ghci_script, ['print016.script'])
test('print017', extra_files(['../Test.hs']), ghci_script, ['print017.script'])
test('print018', extra_files(['../Test.hs']), ghci_script, ['print018.script'])
test('print019', extra_files(['../Test.hs']), ghci_script, ['print019.script'])

# The ghci-ext way emits messages in a slightly different order due to
# printing from two processes, so let's just skip it.
test('print020', [extra_files(['../HappyTest.hs']),
                  omit_ways(['ghci-ext'])], ghci_script, ['print020.script'])

test('print021', normal, ghci_script, ['print021.script'])
test('print022', normal, ghci_script, ['print022.script'])
test('print023', extra_files(['../Test.hs']), ghci_script, ['print023.script'])
test('print024', extra_files(['../Test.hs']), ghci_script, ['print024.script'])
test('print025', normal, ghci_script, ['print025.script'])
test('print026', normal, ghci_script, ['print026.script'])
test('print027', normal, ghci_script, ['print027.script'])
test('print028', normal, ghci_script, ['print028.script'])
test('print029', normal, ghci_script, ['print029.script'])
test('print030', extra_files(['print029.hs']), ghci_script, ['print030.script'])
test('print031', normal, ghci_script, ['print031.script'])
test('print032', extra_files(['print029.hs']), ghci_script, ['print032.script'])
test('print033', normal, ghci_script, ['print033.script'])
test('print034', extra_files(['../GADT.hs', '../Test.hs']), ghci_script, ['print034.script'])
test('print035', extra_files(['../Unboxed.hs']), ghci_script, ['print035.script'])
test('print037', when(llvm_build(),
                      fragile_for(16205, ['ghci-ext']))
               , ghci_script
               , ['print037.script'])

test('break001', extra_files(['../Test2.hs']), ghci_script, ['break001.script'])
test('break002', extra_files(['../Test2.hs']), ghci_script, ['break002.script'])
test('break003', extra_files(['../Test3.hs']), ghci_script, ['break003.script'])
test('break005', extra_files(['../QSort.hs']), ghci_script, ['break005.script'])
test('break006',
     [ when(compiler_debugged(), expect_broken(17833)),
       extra_files(['../Test3.hs'])],
     ghci_script,
     ['break006.script'])
test('break007', extra_files(['Break007.hs']), ghci_script, ['break007.script'])
test('break008', extra_files(['../Test3.hs']), ghci_script, ['break008.script'])
test('break009', [extra_files(['../Test6.hs']),
                  combined_output], ghci_script, ['break009.script'])
test('break010', extra_files(['../Test6.hs']), ghci_script, ['break010.script'])
test('break011',
     [extra_files(['../Test7.hs']),
      combined_output],
     ghci_script, ['break011.script'])
test('break012', normal, ghci_script, ['break012.script'])
test('break013', normal, ghci_script, ['break013.script'])
test('break014', normal, ghci_script, ['break014.script'])
test('break015', expect_broken(1532), ghci_script, ['break015.script'])
test('break016', combined_output, ghci_script, ['break016.script'])
test('break017', [extra_files(['../QSort.hs']),
                  combined_output], ghci_script, ['break017.script'])
test('break018', extra_files(['../mdo.hs']), ghci_script, ['break018.script'])
test('break019', extra_files(['../Test2.hs']), ghci_script, ['break019.script'])
test('break020', extra_files(['Break020b.hs']), ghci_script, ['break020.script'])
test('break021', extra_files(['Break020b.hs', 'break020.hs']), ghci_script, ['break021.script'])
test('break024', combined_output, ghci_script, ['break024.script'])
test('break025', normal, ghci_script, ['break025.script'])
test('break026', normal, ghci_script, ['break026.script'])
test('break027', extra_files(['../QSort.hs']), ghci_script, ['break027.script'])
test('dynbrk001', extra_files(['../QSort.hs']), ghci_script, ['dynbrk001.script'])
test('dynbrk002', extra_files(['../QSort.hs']), ghci_script, ['dynbrk002.script'])
test('dynbrk003', normal, ghci_script, ['dynbrk003.script'])
test('dynbrk004', extra_files(['../mdo.hs']), ghci_script, ['dynbrk004.script'])
test('dynbrk005', [extra_files(['TupleN.hs']), expect_broken(1530)],
     ghci_script, ['dynbrk005.script'])
test('dynbrk007', normal, ghci_script, ['dynbrk007.script'])
test('dynbrk008', normal, ghci_script, ['dynbrk008.script'])
test('dynbrk009', normal, ghci_script, ['dynbrk009.script'])

test('result001', normal, ghci_script, ['result001.script'])

test('listCommand001', [extra_files(['../Test3.hs']),
                        combined_output], ghci_script, ['listCommand001.script'])
test('listCommand002', normal, ghci_script, ['listCommand002.script'])

test('hist001', [extra_files(['../Test3.hs']), extra_run_opts('+RTS -I0')],
     ghci_script, ['hist001.script'])
test('hist002', [extra_files(['../Test3.hs']), extra_run_opts('+RTS -I0')],
     ghci_script, ['hist002.script'])

test('T1620', extra_files(['T1620/', 'T1620/T1620.hs']),
              ghci_script, ['T1620.script'])
test('T2740', normal, ghci_script, ['T2740.script'])
test('T2950', normal, ghci_script, ['T2950.script'])
test('T3000', normal, ghci_script, ['T3000.script'])

test('getargs', extra_files(['../getargs.hs']), ghci_script, ['getargs.script'])
test('T7386', normal, ghci_script, ['T7386.script'])
test('T8487', normal, ghci_script, ['T8487.script'])
test('T8557', normal, ghci_script, ['T8557.script'])
test('T12449', normal, ghci_script, ['T12449.script'])
test('T12458', normal, ghci_script, ['T12458.script'])
test('T13825-debugger',
      [when(arch('powerpc64'), expect_broken(14455)),
       when(arch('arm'), fragile_for(17557, ['ghci-ext']))],
      ghci_script, ['T13825-debugger.script'])
test('T14628', normal, ghci_script, ['T14628.script'])
test('T14690', normal, ghci_script, ['T14690.script'])
test('T16700', normal, ghci_script, ['T16700.script'])

test('break029', extra_files(['break029.hs']), ghci_script, ['break029.script'])
test('T2215', normal, ghci_script, ['T2215.script'])
test('T17989', normal, ghci_script, ['T17989.script'])
test('T19157', normal, ghci_script, ['T19157.script'])
test('T19355', normal, ghci_script, ['T19355.script'])
test('T19394', normal, ghci_script, ['T19394.script'])
