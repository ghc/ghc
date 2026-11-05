from collections.abc import Sequence
from dataclasses import dataclass, field

from filecheck.error import CheckError
from filecheck.finput import FInput, InputRange
from filecheck.ops import CheckOp
from filecheck.options import Options
from filecheck.compiler import compile_uops


@dataclass
class Preprocessor:
    opts: Options
    input: FInput
    checks: Sequence[CheckOp]
    range: InputRange = field(
        default=None,  # pyright: ignore[reportAssignmentType]
        init=False,
    )

    def __post_init__(self):
        self.range = self.input.range

    def run(self):
        for op in self.checks:
            if op.name == "LABEL":
                self.preprocess_label(op)

    def preprocess_label(self, op: CheckOp):
        pattern, _ = compile_uops(op, dict(), self.opts)
        match = self.input.find_between(pattern, self.range)
        if not match:
            raise CheckError(
                f"{op.check_name}: Could not find label '{op.arg}' in input", op
            )

        self.range = self.range.split_at(match)
        self.input.ranges.append(self.range)
