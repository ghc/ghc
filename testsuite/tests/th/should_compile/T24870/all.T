# The interpreter must uphold tagging invariants, and failed to do so in #24870
# We test this here by having the interpreter calls a strict worker function
# with a reference to a value it constructed.
# See also Note [CBV Functions and the interpreter]
test('T24870', [extra_files(['Def.hs', 'Use.hs']), req_th],
	      multimod_compile, ['Def Use', '-dtag-inference-checks -v0'])
