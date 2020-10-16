# Tests for LLVM code generator

def f( name, opts ):
  opts.only_ways = ['optllvm', 'llvm', 'debugllvm']

setTestOpts(f)

def ignore_llvm_and_vortex( msg ):
     return re.sub(r"You are using an unsupported version of LLVM!.*\n",r"",
               re.sub(r"Currently only [^ ]* is supported. System LLVM version: .*\n", r"",
                    re.sub(r"We will try though.*\n",r"",
                         re.sub(r".* is not a recognized processor for this target.*\n",r"",msg))))

# test('T5486', normal, compile, [''])
test('T5681', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T6158', [reqlib('vector'), reqlib('primitive')], compile, ['-package vector -package primitive'])
test('T7571', [cmm_src, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, ['-no-hs-main'])
test('T7575', unless(wordsize(32), skip), compile, [''])
test('T8131b', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T11649', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T17920fail', cmm_src, compile_fail, ['-no-hs-main'])
