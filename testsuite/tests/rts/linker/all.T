# -*-: mode: python -*-

test('unsigned_reloc_macho_x64',
     [
         extra_files(['runner.c', 'unsigned_reloc_macho_x64.s']),
         unless(opsys('darwin'), skip),
         unless(arch('x86_64'), skip)
     ],
     run_command, ['$MAKE -s --no-print-directory unsigned_reloc_macho_x64'])

test('section_alignment',
     [
         extra_files(['runner.c', 'section_alignment.c']),
         unless(opsys('darwin') and arch('x86_64'), expect_broken(13624))
     ],
     run_command, ['$MAKE -s --no-print-directory section_alignment'])
