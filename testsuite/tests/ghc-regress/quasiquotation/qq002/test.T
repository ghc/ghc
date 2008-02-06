test('qq002', [ if_compiler_lt( 'ghc', '6.9', skip ),
                only_compiler_types(['ghc']) ],
     compile_fail, [''])
