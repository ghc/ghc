
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

test('T25770', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, [''])
test('T22487', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, [''])
test('T22033', [normal, unless(wordsize(64), skip), normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, [''])
test('T25730', [req_c, unless(arch('x86_64'), skip), normalise_errmsg_fun(ignore_llvm_and_vortex)], compile_and_run, ['T25730C.c'])
  # T25730C.c contains Intel instrinsics, so only run this test on x86
test('T20645', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex), when(have_llvm(), extra_ways(["optllvm"]))], compile_and_run, [''])
# T26065.c tests LLVM linking of Intel instrinsics, so only run this test on x86
test('T26065', [normal, normalise_errmsg_fun(ignore_llvm_and_vortex), when(have_llvm(), extra_ways(["optllvm"])),
                unless((arch('x86_64') or arch('i386')) and have_cpu_feature('bmi2'),skip)],
                compile_and_run, ['-mbmi2'])

