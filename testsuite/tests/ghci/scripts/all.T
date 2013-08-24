# coding=utf8

setTestOpts(when(compiler_profiled(), skip))

test('ghci001', combined_output, ghci_script, ['ghci001.script'])
test('ghci002', combined_output, ghci_script, ['ghci002.script'])
test('ghci003', combined_output, ghci_script, ['ghci003.script'])
test('ghci004', combined_output, ghci_script, ['ghci004.script'])
test('ghci005', combined_output, ghci_script, ['ghci005.script'])
test('ghci006', combined_output, ghci_script, ['ghci006.script'])
test('ghci007', combined_output, ghci_script, ['ghci007.script'])
test('ghci008', combined_output, ghci_script, ['ghci008.script'])
test('ghci009', combined_output, ghci_script, ['ghci009.script'])

# Skip this test: deadlock cannot be detected now, because we wait for
# signals to arrive if there are signal handlers installed, and GHCi
# has a ^C handler installed.
test('ghci010', skip, ghci_script, ['ghci010.script'])

test('ghci011', normal, ghci_script, ['ghci011.script'])
test('ghci012', normal, ghci_script, ['ghci012.script'])
test('ghci013', normal, ghci_script, ['ghci013.script'])
test('ghci014', reqlib('QuickCheck'), ghci_script, ['ghci014.script'])

# Ignore output from ghci015, the output is non-deterministic
# (has occasional "thread blocked indefinitely" exceptions), and this
# is part of the test.
test('ghci015', ignore_output, ghci_script, ['ghci015.script'])

test('ghci016', expect_broken(552), ghci_script, ['ghci016.script'])
test('ghci017', extra_run_opts('-hide-package haskell98'), ghci_script, ['ghci017.script'])
test('ghci018', expect_broken(1201), ghci_script, ['ghci018.script'])
test('ghci019', normal, ghci_script, ['ghci019.script'])
test('ghci020', normal, ghci_script, ['ghci020.script'])
test('ghci021', normal, ghci_script, ['ghci021.script'])
test('ghci022', normal, ghci_script, ['ghci022.script'])

test('ghci023', normal, ghci_script, ['ghci023.script'])
test('ghci024',
     when(fast(), skip),
     run_command,
     ['$MAKE -s --no-print-directory ghci024'])
test('ghci025', normal, ghci_script, ['ghci025.script'])
test('ghci026', normal, ghci_script, ['ghci026.script'])

test('ghci027', extra_clean(['ghci027.hs']), ghci_script, ['ghci027.script'])
test('ghci028', normal, ghci_script, ['ghci028.script'])
test('ghci029', normal, ghci_script, ['ghci029.script'])
test('ghci030', normal, ghci_script, ['ghci030.script'])
test('ghci031', normal, ghci_script, ['ghci031.script'])
test('ghci032', normal, ghci_script, ['ghci032.script'])
test('ghci033', normal, ghci_script, ['ghci033.script'])
test('ghci034', normal, ghci_script, ['ghci034.script'])
test('ghci035', normal, ghci_script, ['ghci035.script'])
test('ghci036', normal, ghci_script, ['ghci036.script'])
test('ghci037', req_interp, run_command, ['$MAKE -s --no-print-directory ghci037'])
test('ghci038', normal, ghci_script, ['ghci038.script'])
test('ghci039', normal, ghci_script, ['ghci039.script'])
test('ghci040', normal, ghci_script, ['ghci040.script'])
test('ghci041', normal, ghci_script, ['ghci041.script'])
test('ghci042', normal, ghci_script, ['ghci042.script'])
test('ghci043', normal, ghci_script, ['ghci043.script'])
test('ghci044', normal, ghci_script, ['ghci044.script'])
test('ghci045', normal, ghci_script, ['ghci045.script'])
test('ghci046', normal, ghci_script, ['ghci046.script'])
test('ghci047', normal, ghci_script, ['ghci047.script'])
test('ghci048', normal, ghci_script, ['ghci048.script'])
test('ghci049', normal, ghci_script, ['ghci049.script'])
test('ghci050', normal, ghci_script, ['ghci050.script'])
test('ghci051', normal, ghci_script, ['ghci051.script'])
test('ghci052', normal, ghci_script, ['ghci052.script'])
test('ghci053', normal, ghci_script, ['ghci053.script'])
test('ghci054', normal, ghci_script, ['ghci054.script'])
test('ghci055', combined_output, ghci_script, ['ghci055.script'])

test('ghci056',
     [
       pre_cmd('$MAKE -s --no-print-directory ghci056_setup'),
       extra_run_opts('ghci056_c.o'),
       extra_clean(['ghci056_c.o'])
     ],
     ghci_script, ['ghci056.script'])

test('ghci057', normal, ghci_script, ['ghci057.script'])

test('T2452', normal, ghci_script, ['T2452.script'])
test('T2766', normal, ghci_script, ['T2766.script'])

test('T1914',
     extra_clean(['T1914A.hs', 'T1914B.hs']),
     ghci_script,
     ['T1914.script'])

test('T2976', normal, ghci_script, ['T2976.script'])
test('T2816', normal, ghci_script, ['T2816.script'])
test('T789', normal, ghci_script, ['T789.script'])
test('T3263', normal, ghci_script, ['T3263.script'])
test('T4051', normal, ghci_script, ['T4051.script'])
test('T4087', normal, ghci_script, ['T4087.script'])
test('T4015', normal, ghci_script, ['T4015.script'])
test('T4127', normal, ghci_script, ['T4127.script'])
test('T4127a', normal, ghci_script, ['T4127a.script'])
test('T4316', reqlib('mtl'), ghci_script, ['T4316.script'])
test('T4832', normal, ghci_script, ['T4832.script'])
test('T5045', normal, ghci_script, ['T5045.script'])
test('T5130', normal, ghci_script, ['T5130.script'])
test('T5417', normal, ghci_script, ['T5417.script'])
test('T5545', normal, ghci_script, ['T5545.script'])
test('T5557', combined_output, ghci_script, ['T5557.script'])
test('T5566', normal, ghci_script, ['T5566.script'])
test('GhciKinds', normal, ghci_script, ['GhciKinds.script'])
test('T5564', normal, ghci_script, ['T5564.script'])
test('Defer02', normal, ghci_script, ['Defer02.script'])
test('T5820', normal, ghci_script, ['T5820.script'])
test('T5836', normal, ghci_script, ['T5836.script'])
test('T5979', normalise_slashes, ghci_script, ['T5979.script'])
test('T5975a',
     [pre_cmd('touch föøbàr1.hs'),
      clean_cmd('rm föøbàr1.hs')],
     ghci_script, ['T5975a.script'])
test('T5975b',
     [pre_cmd('touch föøbàr2.hs'),
      clean_cmd('rm föøbàr2.hs'),
      extra_hc_opts('föøbàr2.hs')],
     ghci_script, ['T5975b.script'])
test('T6027ghci', normal, ghci_script, ['T6027ghci.script'])

test('T6007', normal, ghci_script, ['T6007.script'])
test('T6091', normal, ghci_script, ['T6091.script'])
test('T6106',
     [extra_clean(['T6106.hs',
                   'T6106_preproc.hi', 'T6106_preproc.o',
                   'T6106_preproc', 'T6106_preproc.exe']),
      pre_cmd('$MAKE -s --no-print-directory T6106_prep')],
     ghci_script,
     ['T6106.script'])
test('T6105', normal, ghci_script, ['T6105.script'])
test('T7117', normal, ghci_script, ['T7117.script'])
test('ghci058',
     [extra_clean(['Ghci058.hs', 'Ghci058.hi', 'Ghci058.o']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script,
     ['ghci058.script'])
test('T7587', normal, ghci_script, ['T7587.script'])
test('T7688', normal, ghci_script, ['T7688.script'])
test('T7627', normal, ghci_script, ['T7627.script'])
test('T7627b', normal, ghci_script, ['T7627b.script'])
test('T7586', normal, ghci_script, ['T7586.script'])
test('T4175', normal, ghci_script, ['T4175.script'])
test('T7872', normal, ghci_script, ['T7872.script'])
test('T7873', normal, ghci_script, ['T7873.script'])
test('T7939', normal, ghci_script, ['T7939.script'])
test('T7894', normal, ghci_script, ['T7894.script'])
test('T8116', normal, ghci_script, ['T8116.script'])
test('T8113', normal, ghci_script, ['T8113.script'])
