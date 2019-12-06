# Stub definitions for things provided by the `typing` package for use by older
# Python versions which don't ship with `typing`.

import collections

class Dummy:
    def __getitem__(self, *args):
        return None

List = Dummy()
Tuple = Dummy()
Set = Dummy()
TextIO = Dummy()
Iterator = Dummy()
Callable = Dummy()
Optional = Dummy()
Dict = Dummy()
Union = Dummy()
Any = Dummy()

NewType = lambda name, ty: ty
def NamedTuple(name, fields):
    return collections.namedtuple(name, [field[0] for field in fields])
