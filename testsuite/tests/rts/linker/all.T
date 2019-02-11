# -*-: mode: python -*-

test('unsigned_reloc_macho_x64',
     [
         extra_files(['runner.c', 'unsigned_reloc_macho_x64.s']),
         unless(opsys('darwin'), skip),
         unless(arch('x86_64'), skip)
     ],
     run_command, ['$MAKE -s --no-print-directory unsigned_reloc_macho_x64'])

test('aligned_mem_access',
     [
         extra_files(['runner.c', 'aligned_mem_access.c']),
         unless(opsys('darwin') and arch('x86_64'), expect_broken(13624))
     ],
     run_command, ['$MAKE -s --no-print-directory aligned_mem_access'])
