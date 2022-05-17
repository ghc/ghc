

ctlextra_files = extra_files(['src/',
                              'ActionsAndObservations.hs', 'BitConsumer.hs', 'CmmPaths.hs',
                              'ControlTestMonad.hs', 'EntropyTransducer.hs', 'LoadCmmGroup.hs',
                              'RunCmm.hs', 'RunWasm.hs',])

basenames = ['Church.hs', 
             'Closure.hs', 
             'FailingLint.hs', 
             'Irr.hs', 
             'Irr2.hs', 
             'Irr3.hs', 
             'Irr4.hs', 
             'Length.hs', 
             'Map.hs', 
             'Max.hs', 
             'PJIf.hs', 
             'dec.cmm', 
             'dloop.cmm', 
             'ex9.cmm', 
             'ex10.cmm', 
             'fig1b.cmm', 
             'hardswitch.cmm', 
             'idmerge.cmm', 
             'ifloop.cmm', 
             'irr.cmm', 
             'irrbad.cmm', 
             'loop.cmm', 
             'looptail.cmm', 
             'multiswitch.cmm', 
             'noloop.cmm', 
             'panic.cmm', 
             'panic2.cmm', 
             'self.cmm', 
             'selfloop.cmm', 
             'switch.cmm', 
             'webexample.cmm'
             ]


sources = ['src/' + basename for basename in basenames]

test('WasmControlFlow',
     [extra_run_opts(" ".join(['"' + config.libdir + '"', '-r'] + sources)), ctlextra_files],
     multimod_compile_and_run,
     ['WasmControlFlow', '-package ghc'])
