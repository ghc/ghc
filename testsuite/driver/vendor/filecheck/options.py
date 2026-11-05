from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Iterable
import os


class Extension(Enum):
    MLIR_REGEX_CLS = "MLIR_REGEX_CLS"


class DumpInputKind(Enum):
    HELP = auto()
    ALWAYS = auto()
    NEVER = auto()
    FAIL = auto()


@dataclass
class Options:
    match_filename: str
    input_file: str = "-"
    check_prefixes: list[str] = "CHECK"  # type: ignore[reportAssignmentType]
    comment_prefixes: list[str] = "COM,RUN"  # type: ignore[reportAssignmentType]
    strict_whitespace: bool = False
    enable_var_scope: bool = False
    match_full_lines: bool = False
    allow_empty: bool = False
    reject_empty_vars: bool = False
    dump_input: DumpInputKind = DumpInputKind.FAIL
    variables: dict[str, str | int] = field(default_factory=dict[str, str | int])

    extensions: set[Extension] = field(default_factory=set[Extension])

    def __post_init__(self):
        # make sure we split the comment prefixes
        if isinstance(self.comment_prefixes, str):
            self.comment_prefixes = self.comment_prefixes.split(",")
        if isinstance(self.check_prefixes, str):
            self.check_prefixes = self.check_prefixes.split(",")
        extensions: set[Extension] = set()
        for ext in self.extensions:
            if isinstance(ext, str):
                if ext in set(e.value for e in Extension):
                    extensions.add(
                        Extension[ext]  # pyright: ignore[reportArgumentType]
                    )
                else:
                    print(
                        f"Unknown filecheck extension: {ext}, supported are {list(e.name for e in Extension)}"
                    )
            else:
                extensions.add(ext)
        self.extensions = extensions
        if isinstance(self.dump_input, str):
            name: str = self.dump_input.upper()
            if name in set(d.name for d in DumpInputKind):
                self.dump_input = DumpInputKind[name]
            else:
                raise RuntimeError(
                    f'Unknown value supplied for dump-input flag: "{name}"'
                )

    def readable_input_file(self):
        if self.input_file == "-":
            return "<stdin>"
        return self.input_file


def parse_argv_options(argv: list[str]) -> Options:
    # pop the name off of argv
    _ = argv.pop(0)

    # create a set of valid fields
    valid_fields = set(Options.__dataclass_fields__)
    # add check-prefix as another valid field (merged with check-prefixes)
    valid_fields.add("check_prefix")

    # final options to return
    opts: dict[str, str | bool] = {}
    variables: dict[str, str | int] = {}
    # args that were consumed
    remove: set[int] = set()
    argv = list(normalise_args(argv))
    # grab extensions from env:
    extensions = set(os.getenv("FILECHECK_FEATURE_ENABLE", "").split(","))
    # remove empty extension in case it made it in there
    if "" in extensions:
        extensions.remove("")

    for i, arg in enumerate(argv):
        # skip consumed args
        if i in remove:
            continue
        if arg.startswith("-D"):
            if i == len(argv) - 1:
                raise RuntimeError("Out of range arguments")
            key, val = arg[2:], argv[i + 1]
            variables[key] = val
            remove.update((i, i + 1))
        elif arg.startswith("--"):
            arg = arg[2:].replace("-", "_")
        elif arg.startswith("-"):
            arg = arg[1:].replace("-", "_")
        else:
            continue
        if arg not in valid_fields:
            continue

        remove.add(i)
        if (f := Options.__dataclass_fields__.get(arg, None)) and f.type == bool:
            opts[arg] = True
        else:
            if i == len(argv) - 1:
                raise RuntimeError("Out of range arguments")
            # make sure to append check and comment prefixes if flag is used multiple times
            if arg in ("check_prefix", "comment_prefixes") and arg in opts:
                existing_opts = opts[arg]
                assert isinstance(existing_opts, str)
                opts[arg] = existing_opts + "," + argv[i + 1]
            else:
                opts[arg] = argv[i + 1]
            remove.add(i + 1)

    if "check_prefix" in opts:
        prefixes = opts.pop("check_prefix")
        assert isinstance(prefixes, str)
        if "check_prefixes" in opts:
            more_prefixes = opts.pop("check_prefixes")
            assert isinstance(more_prefixes, str)
            prefixes += "," + more_prefixes
        opts["check_prefixes"] = prefixes

    for idx in sorted(remove, reverse=True):
        argv.pop(idx)

    if len(argv) > 1:
        raise RuntimeError(
            f"Unconsumed arguments: {argv}, expected one remaining arg, the match-filename."
        )
    if len(argv) != 1:
        raise RuntimeError(f"Missing argument: check-file")

    opts["match_filename"] = argv[0]

    return Options(
        **opts,  # pyright: ignore[reportArgumentType]
        variables=variables,
        extensions=extensions,  # pyright: ignore[reportArgumentType]
    )


def normalise_args(argv: list[str]) -> Iterable[str]:
    """
    Normalize arguments by splitting "--key=value" pairs into separate "--key" and
    "value" entries.
    """
    for arg in argv:
        if arg.startswith("-") and "=" in arg:
            yield from arg.split("=", 1)
        else:
            yield arg
