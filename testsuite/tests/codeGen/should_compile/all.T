test('cg001', normal, compile, [''])
test('cg002', normal, compile, [''])
test('cg003', only_ways(['optasm']), compile, [''])
test('cg004', normal, compile, [''])
test('cg005', only_ways(['optasm']), compile, [''])
test('cg006', normal, compile, [''])
test('cg007', normal, compile, [''])
test('cg008', normal, compile, [''])
# 009/010 have their own all.T file
test('cg011', normal, compile, [''])

test('T1916', normal, compile, [''])
test('T2388', normal, compile, [''])
test('T3132', normal, compile, ['-dcmm-lint'])
test('T3286', [], multimod_compile, ['T3286', '-v0'])
test('T3579', normal, compile, [''])
test('T2578', normal, makefile_test, [])
# skip llvm on i386 as we don't support fPIC
test('jmp_tbl', when(arch('i386'), omit_ways(llvm_ways)), compile, ['-fPIC -O'])
test('massive_array',
     [ when(arch('i386'), omit_ways(llvm_ways)) ],
     compile, ['-fPIC'])
test('T7237', normal, compile, [''])
test('T7574', [cmm_src, omit_ways(llvm_ways)], compile, ['-no-hs-main'])
test('T8205', normal, compile, ['-O0'])
test('T9155', normal, compile, ['-O2'])
test('T9303', normal, compile, ['-O2'])
test('T9329', [when(unregisterised(), expect_broken(15467)), cmm_src], compile, ['-no-hs-main'])

test('debug',
  [ normal,
    req_cmm
  ],
  makefile_test, [])

test('T9964', normal, compile, ['-O'])
test('T10518', [cmm_src], compile, ['-no-hs-main'])
test('T10667', normal, compile, ['-g'])
test('T12115', normal, compile, [''])
test('T12355', normal, compile, [''])
test('T13233_orig', [unless(have_profiling(), skip)], makefile_test, [])
test('T14999',
     [when(unregisterised(), skip),
      unless(opsys('linux') and arch('x86_64') and have_gdb() and
        have_readelf(), skip)],
     makefile_test, [])

# Verify that we optimize away redundant jumps for unordered comparisons.
test('T15196',
  [ unless(arch('x86_64'), skip),
    only_ways(['normal']),
  ], makefile_test, [])

test('T15723',
  [ unless(have_profiling(), skip),
    unless(have_dynamic(), skip),
  ], makefile_test, [])

test('T15155',
  [ unless(have_ncg(), skip)
  ], makefile_test, [])

test('T15155l',
     [when(unregisterised(), skip), unless(have_llvm(), skip), only_ways(llvm_ways)],
     makefile_test, [])

test('T16449_1', normal, compile, [''])

# Verify that we keep the CFG in sync on x86
test('T17334', [ unless(have_ncg() and (arch('x86_64') or arch('i386')), skip)
               , only_ways(['normal'])
               ], compile, ['-O'])

test('T14373',
      [ req_cmm
      ],
     multimod_compile_filter, ['T14373', '-fasm -O2 -c -ddump-cmm-from-stg',
     r'grep -e "const T14373\.._closure+.;"'])

switch_skeleton_only = r'grep -e "switch \[" -e "case " -e "default: " | sed -e "s|\] .*|\]|g" -e "s|goto .*|goto |g"'

test('T14373a',
      [ req_cmm
      ],
     multimod_compile_filter, ['T14373a', '-fasm -O2 -c -ddump-cmm-from-stg',
     switch_skeleton_only])
test('T14373b',
      [ req_cmm
      ],
     multimod_compile_filter, ['T14373b', '-fasm -O2 -c -ddump-cmm-from-stg',
     switch_skeleton_only])
test('T14373c',
      [ req_cmm
      ],
     multimod_compile_filter, ['T14373c', '-fasm -O2 -c -ddump-cmm-from-stg',
     switch_skeleton_only])

switch_skeleton_and_entries_only = (r'grep -e "switch \[" -e "case " -e "default: " -e "Default_entry(" -e "R1 = .*_closure+2;"'
                                    r'| sed -e "s|\] .*|\]|g" -e "s|goto .*|goto |g" -e "s|R1 = .*_closure+2;.*|R1 = XYZ_closure+2;|g" -e "s|//.*|//|g"')

test('T14373d',
      [ req_cmm
      ],
     multimod_compile_filter, ['T14373d', '-fasm -O2 -c -ddump-cmm-from-stg',
     switch_skeleton_and_entries_only])

test('T17648', js_broken(22370), makefile_test, [])
test('T17904', normal, compile, ['-O'])
test('T18227A', normal, compile, [''])
test('T18227B', normal, compile, [''])

# runRW#-related
test('T18291', normal, compile, ['-O0'])
test('T18397', normal, compile, ['-O0'])

test('T15570',
   when(unregisterised(), skip),
   compile, ['-Wno-overflowed-literals'])
   # skipped with CmmToC because it generates a warning:
   #   warning: integer constant is so large that it is unsigned

test('T18614', normal, compile, [''])
test('mk-big-obj',
     [unless(opsys('mingw32'), skip), pre_cmd('$PYTHON mk-big-obj.py > mk-big-obj.c')],
     multimod_compile, ['mk-big-obj.c', '-c -v0 -no-hs-main'])
test('T21710a', [ unless(tables_next_to_code(), skip) , when(wordsize(32), skip)
                , only_ways(['optasm'])
                , grep_errmsg('(call)',[1]) ]
                , compile, ['-ddump-cmm -dno-typeable-binds'])
test('T23002', normal, compile, ['-fregs-graph'])
test('T24264', [req_cmm, grep_errmsg(r'(.*\().*(\) returns to)', [1,2])],
     compile, ['-O -ddump-cmm-from-stg -dno-typeable-binds'])

test('callee-no-local', [
    req_cmm, unless(arch('wasm32') or unregisterised(), skip),
    grep_errmsg('ccall')
  ],
  compile,
  ['-ddump-cmm-raw']
)
