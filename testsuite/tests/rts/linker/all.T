# -*-: mode: python -*-

test('unsigned_reloc_macho_x64',
     [
         extra_files(['runner.c', 'unsigned_reloc_macho_x64.s']),
         unless(opsys('darwin'), skip),
         unless(arch('x86_64'), skip)
     ],
     makefile_test, [])

######################################
test('section_alignment',
     [
         extra_files(['runner.c', 'section_alignment.c']),
         unless(opsys('darwin'), expect_broken(13624))
     ],
     makefile_test, [])

######################################
test('T23066',
     [ unless(arch('x86_64'), skip)
     , unless(opsys('linux'), skip)
     , extra_files(['runner.c', 'T23066_c.c'])
     ],
     makefile_test, [])

######################################
# Test to see if linker scripts link properly to real ELF files
test('T2615',
     [extra_files(['libfoo_T2615.c', 'libfoo_script_T2615.so']),
       req_dynamic_lib_support,
       # JavaScript linker doesn't support linker scripts
       js_skip,
       # Windows linker doesn't seem to support linker scripts
       when(opsys('mingw32'), skip),
       # OS X doesn't seem to support linker scripts
       when(opsys('darwin'), skip),
       # Solaris' linker does not support GNUish linker scripts
       when(opsys('solaris2'), skip),
       # OpenBSD linker does not support GNUish linker scripts
       when(opsys('openbsd'), expect_broken(20869)),
       # LD_LIBRARY_PATH etc doesn't work on wasm
       when(arch('wasm32'), skip),
       pre_cmd('$MAKE -s --no-print-directory T2615-prep'),
       # Add current directory to dlopen search path
       cmd_prefix('LD_LIBRARY_PATH=$LD_LIBRARY_PATH:. ')],
     compile_and_run,
     ['-package ghc'])

######################################
# Workaround bug #8458: old dlopen opens sections in the wrong order,
# so we just accept both orders.
def checkDynAsm(actual_file, normaliser):
    actual_raw = read_no_crs(actual_file)
    actual_str = normaliser(actual_raw)
    actual = actual_str.split()
    if actual == ['initArray1', 'initArray2', 'success']:
        return True
    elif opsys('darwin') and actual == ['modInitFunc1', 'modInitFunc2', 'success']:
        return True
    elif opsys('mingw32') and actual == ['ctors1', 'ctors2', 'success']:
        return True
    else:
        if_verbose(1, 'T5435_dyn_asm failed with %s, see all.T for details' % actual)
        return False

# T5435_v_asm got split into two tests because depending
# on the linker, .init_array and .ctors sections are loaded
# in a different order (but all entries within a section
# do get loaded in a deterministic order). So we test each
# separately now.
test('T5435_v_asm_a',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      req_rts_linker,
      fragile(22970),
      when(arch('arm'), expect_broken(17559)),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, ['T5435_v_asm_a'])
# this one just needs to run on linux, as darwin/mingw32 are covered
# by the _a test already.
test('T5435_v_asm_b',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      req_rts_linker,
      fragile(22970),
      when(arch('arm'), expect_broken(17559)),
      when(opsys('darwin') or opsys('mingw32'), skip),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, ['T5435_v_asm_b'])
test('T5435_v_gcc',
     [extra_files(['T5435.hs', 'T5435_gcc.c']),
      req_rts_linker,
      fragile(22970),
      when(arch('arm'), expect_broken(17559)),
      when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, ['T5435_v_gcc'])
test('T5435_dyn_asm',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      fragile(22970),
      req_c,
      check_stdout(checkDynAsm)],
     makefile_test, ['T5435_dyn_asm'])
test('T5435_dyn_gcc',
     [extra_files(['T5435.hs', 'T5435_gcc.c']),
      fragile(22970),
      req_c],
     makefile_test, ['T5435_dyn_gcc'])

######################################
test('linker_unload',
     [extra_files(['LinkerUnload.hs', 'Test.hs']),
      req_rts_linker],
     makefile_test, ['linker_unload'])

test('linker_unload_native',
     [extra_files(['LinkerUnload.hs', 'Test.hs']),
      req_rts_linker,
      unless(have_dynamic(), skip),
      fragile(23993),
      when(opsys('darwin') or opsys('mingw32'), skip)],
     makefile_test, ['linker_unload_native'])

######################################
test('linker_error1', [extra_files(['linker_error.c']),
                       js_skip, # dynamic linking not supported by the JS backend
                       ignore_stderr], makefile_test, ['linker_error1'])

test('linker_error2', [extra_files(['linker_error.c']),
                       js_skip, # dynamic linking not supported by the JS backend
                       ignore_stderr], makefile_test, ['linker_error2'])

test('linker_error3', [extra_files(['linker_error.c']),
                       js_skip, # dynamic linking not supported by the JS backend
                       ignore_stderr], makefile_test, ['linker_error3'])

######################################
test('rdynamic', [ unless(opsys('linux') or opsys('mingw32'), skip)
                 , unless(have_dynamic(), skip)
                 # this needs runtime infrastructure to do in ghci:
                 #  '-rdynamic' ghc, load modules only via dlopen(RTLD_BLOBAL) and more.
                 , omit_ghci
                 , js_broken(22374)
                 # RTS linker doesn't work on non-dyn ways on wasm32
                 , when(arch('wasm32'), skip)
                 ],
     compile_and_run, ['-rdynamic -package ghc'])


test('T7072',
	[extra_files(['load-object.c', 'T7072.c']),
		unless(opsys('linux'), skip),
		req_rts_linker],
	makefile_test, ['T7072'])

test('T20494', [req_rts_linker, when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, ['T20494'])

test('T20918',
     [extra_files(['T20918_v.cc']),
      unless(opsys('mingw32'), skip),
      when(opsys('mingw32'), expect_broken(2)),
      req_rts_linker],
     makefile_test, ['T20918'])

test('T21618',
     [unless(opsys('mingw32'), skip), req_rts_linker],
     makefile_test, ['T21618'])

# test R_AARCH64_NONE relocation support
test('reloc-none',
	[extra_files(['load-object.c', 'reloc-none.c']),
		unless(arch('aarch64'), skip),
                unless(opsys('linux'), skip),
		req_rts_linker],
	makefile_test, ['reloc-none'])

test('T25191',
     [req_rts_linker,
      extra_files(['T25191_foo1.c','T25191_foo2.c']),
      when(opsys('darwin'), expect_broken(25191)), # not supported in the MachO linker yet
      when(opsys('mingw32'), expect_broken(25191)) # not supported in the PE linker yet
     ],
     makefile_test, ['T25191'])
