# -*-: mode: python -*-

test('unsigned_reloc_macho_64',
     [
         extra_files(['runner.c', 'unsigned_reloc_macho_64.s']),
         unless(opsys('darwin'), skip),
         unless(arch('x86_64'), skip)
     ],
     run_command, ['$MAKE -s --no-print-directory unsigned_reloc_macho_64'])
