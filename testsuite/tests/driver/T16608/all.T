test('T16608_1', [when(opsys('darwin'), fragile(16855)), extra_files(['MyInteger.hs'])], makefile_test, [])
test('T16608_2', [when(opsys('darwin'), fragile(16855)), extra_files(['MyInteger.hs'])], makefile_test, [])
