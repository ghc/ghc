# Test for the ELF parser: more than 0xff00 sections (use different ELF fields)

test('recomp015',
     [ clean_cmd('$MAKE -s clean'),
       unless(opsys('linux') or opsys('solaris2') or opsys('openbsd'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory recomp015'])

