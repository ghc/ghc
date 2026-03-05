test('T13786',
     [when(unregisterised() or platform('x86_64-apple-darwin'), fragile(17018)), req_c],
     makefile_test, [])

