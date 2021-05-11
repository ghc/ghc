# Tests for various scenarios in multi module compilation when code generation
# options (-fasm, -fllvm, -fobject-code, -fbyte-code) are used via OPTIONS_GHC
# pragma
# (Only one of the llvm way is sufficient to test these)

# Basic working of overriding 'backend' via OPTIONS_GHC
test('options_ghc_fasm_fllvm',
     [ when(fast(), skip),
          only_ways(['llvm', 'ghci']),
          extra_files(['Mod_fasm.hs', 'Mod_fllvm.hs', 'Mod_fasm_fllvm.hs'])],
     multimod_compile,
     ['Mod_fasm_fllvm', ''])

# Test that llvm is indeed getting used
test('options_ghc_fasm_fllvm_2',
     [ when(fast(), skip),
          only_ways(['llvm']),
          extra_files(['Mod_fasm.hs', 'Mod_fllvm.hs', 'Mod_fasm_fllvm.hs'])],
     multimod_compile_fail,
     ['Mod_fasm_fllvm', '-optlc -SOMETHING_INVALID'])

# This should compile, as llvm should not be used with -fasm override
test('options_ghc_fasm',
     [ when(fast(), skip),
          only_ways(['llvm']),
          extra_files(['Mod_fasm.hs'])],
     multimod_compile,
     ['Mod_fasm', '-optlc -SOMETHING_INVALID'])

# Should cause object-code creation
test('options_ghc_fobject-code',
     [ when(fast(), skip),
          only_ways(['ghci']),
          extra_files(['Mod_fasm.hs', 'Mod_fllvm.hs', 'Mod_fbyte_code.hs', 'Mod_fobject_code.hs', 'Mod_top.hs', 'Load_Mod_top.script'])],
     ghci_script,
     ['Load_Mod_top.script'])

# Should override the -fbyte-code in normal compile
test('options_ghc_fbyte-code',
     [ when(fast(), skip),
          only_ways(['llvm']),
          extra_files(['Mod_fasm.hs', 'Mod_fllvm.hs', 'Mod_fbyte_code.hs', 'Mod_fobject_code.hs', 'Mod_top.hs'])],
     multimod_compile,
     ['Mod_top', ''])