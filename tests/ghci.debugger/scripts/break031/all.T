setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break031a',
    [ extra_files(['a/', 'b/', 'unitA', 'unitB'])
    , normalise_slashes
    ]
    , ghci_multiunit_script, [['unitA', 'unitB'], 'break031a.script']
    )
test('break031b',
    [ extra_files(['a/', 'b/', 'unitA', 'unitB'])
    , normalise_slashes
    ]
    , ghci_multiunit_script, [['unitA', 'unitB'], 'break031b.script']
    )
