# We can compile Network on its own (pkg02_a)

test('pkg02_a', only_ways(['normal']), multimod_compile, ['Network','-v0'])
test('pkg02_b', normal, multimod_compile, ['A','-v0'])
clean_o_hi()
