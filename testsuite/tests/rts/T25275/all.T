# Compile and run with default RTS options
test(
    'T25275_A',
    [
        extra_files(['TraceIpe.hs']),
        js_skip
    ],
    multimod_compile_and_run,
    ['TraceIpe', ''],
)

# Compile and run with -lI
test(
    'T25275_B',
    [
        extra_files(['TraceIpe.hs']),
        extra_run_opts('+RTS -lI -RTS'),
        js_skip
    ],
    multimod_compile_and_run,
    ['TraceIpe', ''],
)
