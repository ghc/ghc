
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

test('T22487', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, [''])
test('T22033', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, [''])
