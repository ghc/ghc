test('T24081', [extra_files(['TH.hs', 'Main.hs']), req_th],
	      multimod_compile, ['TH Main', '-v0 -ddump-splices -dsuppress-uniques'])
