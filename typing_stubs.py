# Stub definitions for things provided by the typing package
# for use by older Python versions.

class Dummy:
    def __index__(self, *args):
        return None

List = Dummy()
Tuple = Dummy()
Set = Dummy()
TextIO = Dummy()
Iterator = Dummy()
Newtype = lambda name, ty: ty
