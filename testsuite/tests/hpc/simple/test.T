setTestOpts([omit_ghci, when(fast(), skip), js_skip])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('hpc001', [extra_files(['../hpcrun.pl']), cmd_prefix(hpc_prefix),
     when(arch('wasm32'), fragile(23243)),
     ignore_extension
     ],
     compile_and_run, ['-fhpc'])
