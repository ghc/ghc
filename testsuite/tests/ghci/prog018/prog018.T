# testcase for warning and error messages from :load
test('prog018', [combined_output,
    skip, # ghcjs
    extra_files(['A.hs', 'B.hs', 'C.hs'])],
    ghci_script, ['prog018.script'])
