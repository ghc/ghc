# This shouldn't crash the disassembler
test('T25374', [extra_hc_opts('+RTS -Di -RTS'), ignore_stderr, unless(debug_rts(), skip)], ghci_script, [''])

