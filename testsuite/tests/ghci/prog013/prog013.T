test('prog013', [extra_files(['Bad.hs', 'Good.hs']),
	 skip # ghcjs
	 ], ghci_script, ['prog013.script'])

