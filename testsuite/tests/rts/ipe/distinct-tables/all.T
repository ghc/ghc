test(
    'distinct_tables',
    [
        extra_files([
            # Source files
            'Main.hs',
            'X.hs',

            # Expected output files
            'ACon.out',
            'BCon.out',
            'CCon.out',
            'AConBCon.out',
            'NoACon.out',
            'NoBCon.out',
            'NoCCon.out',
            'NoBConCCon.out'
        ]),
        ignore_stdout
        ]
    , makefile_test, [])
