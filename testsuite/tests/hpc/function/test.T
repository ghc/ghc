setTestOpts([omit_ghci, when(fast(), skip), js_skip])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('tough',
     [extra_files(['../hpcrun.pl']),
      cmd_prefix(hpc_prefix),
      ignore_extension,
      when(arch('wasm32'), fragile(23243))],
     compile_and_run, ['-fhpc'])
