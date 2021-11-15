test('prog012', [extra_files(['../shell.hs', 'Bar1.hs', 'Bar2.hs', 'Foo.hs', 'FooBar.hs']),
	 skip # ghcjs
	 ], ghci_script, ['prog012.script'])
