# We can compile Network on its own (pkg02_a), but as soon as we
# compile a module that depends on something in the network package,
# we get a conflict (pkg02_b).

test('pkg02_a', only_ways(['normal']), multimod_compile, ['Network','-v0'])
test('pkg02_b', normal, multimod_compile_fail, ['A','-v0'])
clean_o_hi()
