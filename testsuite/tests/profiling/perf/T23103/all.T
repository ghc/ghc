test(
    'info_table_map_perf',
    [
        collect_compiler_stats('bytes allocated',10),
        extra_files(
            [
                '../../../../../utils/check-exact/ExactPrint.hs',
                '../../../../../utils/check-exact/Types.hs',
                '../../../../../utils/check-exact/Utils.hs',
            ]
        ),
        js_skip,
        omit_ways(llvm_ways), # -finfo-table-map does not work with -fllvm (#26435)
    ],
    multimod_compile,
    ['ExactPrint.hs', '-package ghc -O -finfo-table-map']
)
