# This tests #21390, where data constructors constructed by the interpreter
# would contain untagged values in strict fields.
#
# The test is structured as follows:
#
#  * `Lib.hs` defines a type defining `Ty`, a type with a strict Maybe field;
#    this should be compiled to object code
#  * `GetTy.hs` defines a function, `getTy`, which scrutinizes a `Ty` value and
#    its field. This too should be compiled to object code.
#  * `Test.hs` defines `mkTy` which constructs a `Ty`. This should be
#    interpreted.

test('T21390', extra_files(['Lib.hs', 'Test.hs', 'GetTy.hs']), ghci_script, ['T21390.script'])

