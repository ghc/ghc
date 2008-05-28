setTestOpts(only_compiler_types(['ghc']))

test( 'xmlish',
      compose(skip_if_fast,
              # These are pretty consistent about giving
              #     magic number mismatch: old/corrupt interface file?
              # when run after the other ways.
              if_compiler_lt('ghc', '6.9', omit_ways(['profasm', 'profc']))),
      multimod_compile_and_run,
      ['Main','']
    )
clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o', 'Main.hi', 'Main.o'])
