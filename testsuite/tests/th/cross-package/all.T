test(
    'CrossPackage',
    [
        pre_cmd('$MAKE -s --no-print-directory CrossPackage'),
        extra_files(['Cross.hs', 'CrossLocal.hs', 'CrossDep.hs', 'CrossNum.hs', 'prep.bash', 'dep.conf']),
        # ignore_stderr,
    ],
    # multimod_compile_and_run,
    multimod_compile,
    ['Cross', '-package-db db -fprefer-byte-code -fbyte-code-and-object-code -package dep -v0'],
)
