test(
    'info_table_map_perf',
    [
        collect_compiler_stats('bytes allocated',10),
        no_lint,
        extra_files(
            [
                '../../../../../utils/check-exact/ExactPrint.hs',
                '../../../../../utils/check-exact/Types.hs',
                '../../../../../utils/check-exact/Utils.hs',
            ]
        ),
        js_skip
    ],
    multimod_compile,
    ['ExactPrint.hs', '-package ghc -O -finfo-table-map']
)
