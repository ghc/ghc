test('T22376', [extra_files(['A.hs', 'B.hs'])], multimod_compile_and_run,
     ['T22376', '-O1 -fwrite-if-simplified-core -fbyte-code-and-object-code -fprefer-byte-code'])
