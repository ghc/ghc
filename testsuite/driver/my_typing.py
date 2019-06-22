try:
    from typing import *
except:
    from typing_stubs import * # type: ignore

WayName = NewType("WayName", str)
TestName = NewType("TestName", str)
OutputNormalizer = Callable[[str], str]
