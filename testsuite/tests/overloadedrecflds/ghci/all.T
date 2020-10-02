test('duplicaterecfldsghci01', combined_output, ghci_script, ['duplicaterecfldsghci01.script'])
test('overloadedlabelsghci01', combined_output, ghci_script, ['overloadedlabelsghci01.script'])
test('T13438', [expect_broken(13438), combined_output], ghci_script, ['T13438.script'])
