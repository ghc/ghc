# Tests for LLVM code generator

def f( name, opts ):
  opts.only_ways = ['optllvm', 'llvm', 'debugllvm']

setTestOpts(f)

# Apples LLVM Toolchain knows about a `vortex` cpu (and possibly others), that
# the stock LLVM toolchain doesn't know about and will warn about. Let's not
# have test fail just because of processor name differences due to different
# LLVM Toolchains.  GHC tries to pass what apple expects (on darwin), but can
# be used with the stock LLVM toolchain as well.
def ignore_llvm_and_vortex( msg ):
     return re.sub(r".* is not a recognized processor for this target.*\n",r"",msg)

# test('T5486', normal, compile, [''])
test('T5681', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T7571', [cmm_src, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, ['-no-hs-main'])
test('T7575', unless(wordsize(32), skip), compile, [''])
test('T8131b', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T11649', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile, [''])
test('T17920fail', cmm_src, compile_fail, ['-no-hs-main'])
test('T25019', unless((arch('x86_64') or arch('i386')) and have_cpu_feature('sse4_2'),skip), compile, ['-msse4.2'])
