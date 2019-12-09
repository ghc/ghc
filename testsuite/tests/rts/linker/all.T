# -*-: mode: python -*-

test('unsigned_reloc_macho_x64',
     [
         extra_files(['runner.c', 'unsigned_reloc_macho_x64.s']),
         unless(opsys('darwin'), skip),
         unless(arch('x86_64'), skip)
     ],
     run_command, ['$MAKE -s --no-print-directory unsigned_reloc_macho_x64'])

######################################
test('section_alignment',
     [
         extra_files(['runner.c', 'section_alignment.c']),
         unless(opsys('darwin') and arch('x86_64'), expect_broken(13624))
     ],
     run_command, ['$MAKE -s --no-print-directory section_alignment'])

######################################
# Test to see if linker scripts link properly to real ELF files
test('T2615',
     [extra_files(['libfoo_T2615.c', 'libfoo_script_T2615.so']),
       when(opsys('mingw32'), skip),
       # OS X doesn't seem to support linker scripts
       when(opsys('darwin'), skip),
       # Solaris' linker does not support GNUish linker scripts
       when(opsys('solaris2'), skip),
       pre_cmd('$MAKE -s --no-print-directory T2615-prep'),
       # Add current directory to dlopen search path
       cmd_prefix('LD_LIBRARY_PATH=$LD_LIBRARY_PATH:. '),
       extra_clean(['libfoo_T2615.so', 'libfoo_T2615.o'])],
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
# These should have extra_clean() arguments, but I need
# to somehow extract out the name of DLLs to do that
test('T5435_v_asm_a',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      req_rts_linker,
      when(arch('arm'), expect_broken(17559))],
     makefile_test, ['T5435_v_asm_a'])
# this one just needs to run on linux, as darwin/mingw32 are covered
# by the _a test already.
test('T5435_v_asm_b',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      req_rts_linker,
      when(arch('arm'), expect_broken(17559)),
      when(opsys('darwin') or opsys('mingw32'), skip)],
     makefile_test, ['T5435_v_asm_b'])
test('T5435_v_gcc',
     [extra_files(['T5435.hs', 'T5435_gcc.c']),
      req_rts_linker,
      when(arch('arm'), expect_broken(17559))],
     makefile_test, ['T5435_v_gcc'])
test('T5435_dyn_asm',
     [extra_files(['T5435.hs', 'T5435_asm.c']),
      check_stdout(checkDynAsm)],
     makefile_test, ['T5435_dyn_asm'])
test('T5435_dyn_gcc', extra_files(['T5435.hs', 'T5435_gcc.c']) , makefile_test, ['T5435_dyn_gcc'])

######################################
test('linker_unload',
     [extra_files(['LinkerUnload.hs', 'Test.hs']), req_rts_linker],
     makefile_test, ['linker_unload'])

######################################
test('linker_error1', [extra_files(['linker_error.c']),
                       ignore_stderr], makefile_test, ['linker_error1'])

test('linker_error2', [extra_files(['linker_error.c']),
                       ignore_stderr], makefile_test, ['linker_error2'])

test('linker_error3', [extra_files(['linker_error.c']),
                       ignore_stderr], makefile_test, ['linker_error3'])

######################################
test('rdynamic', [ unless(opsys('linux') or opsys('mingw32'), skip)
                 # this needs runtime infrastructure to do in ghci:
                 #  '-rdynamic' ghc, load modules only via dlopen(RTLD_BLOBAL) and more.
                 , omit_ways(['ghci'])
                 ],
     compile_and_run, ['-rdynamic -package ghc'])
