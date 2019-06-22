try:
    from typing import *

    WayName = NewType("WayName", str)
    TestName = NewType("TestName", str)
    OutputNormalizer = Callable[[str], str]
except:
    pass
    #WayName = str
    #TestName = str

    #class Dummy:
    #    def __index__(self, ty):
    #        return None

    #List = Dummy() # type: ignore
    #Tuple = Dummy() # type: ignore

