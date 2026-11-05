import sys

from filecheck.colors import WARN, FMT
from filecheck.ops import CheckOp
from filecheck.options import Options


def warn(
    msg: str,
    *,
    op: CheckOp | None = None,
    input_loc: str | None = None,
    opts: Options,
):
    print(f"{WARN}Warning: {msg}{FMT.RESET}", end="", file=sys.stderr)
    if input_loc:
        print(f" at {input_loc}", end="", file=sys.stderr)
    print("", file=sys.stderr)
    if op:
        print(op.source_repr(opts), file=sys.stderr)
