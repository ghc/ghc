test('simplCore.oneShot', [extra_files(['OneShot1.hs', 'OneShot2.hs']),
                           only_ways(['optasm'])], multimod_compile_and_run,
     ['OneShot2', '-v0'])
