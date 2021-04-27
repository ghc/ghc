# Note [_ffi_call_unix64]
# ~~~~~~~~~~~~~~~~~~~~~~~
#
# Please refer to https://gitlab.haskell.org/ghc/ghc/issues/5019
# for the subsections_via_symbols.stderr
def ignore_llvm_and_vortex( msg ):
     return re.sub(r"You are using an unsupported version of LLVM!.*\n",r"",
               re.sub(r"Currently only [^ ]* is supported. System LLVM version: .*\n", r"",
                    re.sub(r"We will try though.*\n",r"",
                         re.sub(r".* is not a recognized processor for this target.*\n",r"",msg))))

test('subsections_via_symbols',
     [when(not opsys('darwin'), skip),
      only_ways(['optllvm', 'llvm', 'debugllvm']),
      extra_files(['SubsectionsViaSymbols.hs']),
      normalise_errmsg_fun(ignore_llvm_and_vortex)],
     makefile_test, [])
