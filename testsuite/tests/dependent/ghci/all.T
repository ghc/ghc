test('T11549',
     [ expect_broken( 11787 ),
       expect_broken( 11786 ) ],
     ghci_script, ['T11549.script'])
test('T14238', normal, ghci_script, ['T14238.script'])
