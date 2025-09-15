# Test for the ELF parser: more than 0xff00 sections (use different ELF fields)

test('recomp015',
     [extra_files(['Generate.hs']),
       # See ticket:11022#comment:7
       unless(opsys('linux') or opsys('solaris2') or opsys('openbsd'), skip),
       when(arch('arm'), skip),
       js_skip, # JS backend doesn't support .s assembly files

       # the linker sometimes throws warnings since we don't
       # generate a .note.GNU-stack section
       ignore_stderr,
     ],
     makefile_test, [])

