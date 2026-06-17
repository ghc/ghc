test(
    't24005',
    [ js_skip,
      omit_ways(llvm_ways), # -finfo-table-map does not work with -fllvm (#26435)
    ],
    compile_and_run,
    ['-finfo-table-map -fdistinct-constructor-tables']
)
