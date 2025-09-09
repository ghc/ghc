# This shouldn't crash the disassembler
test('T25374', [extra_hc_opts('+RTS -Di -RTS'), ignore_stderr, req_target_debug_rts], ghci_script, [''])

