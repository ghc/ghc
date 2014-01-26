test('simplCore.oneShot',
     [ only_ways(['optasm']),
       extra_clean(['OneShot1.hi', 'OneShot1.o',
                    'OneShot2.hi', 'OneShot2.o']),
     ],
     multimod_compile_and_run,
     ['OneShot2', '-v0'])
