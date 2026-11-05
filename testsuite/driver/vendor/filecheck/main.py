import io
import sys
import importlib.metadata

from filecheck.help import HELP_TEXT
from filecheck.matcher import Matcher
from filecheck.options import parse_argv_options


def main(argv: list[str] | None = None):
    for stream in (sys.stdout, sys.stderr):
        if isinstance(stream, io.TextIOWrapper):
            stream.reconfigure(encoding="utf-8")  # pyright: ignore[reportUnknownMemberType]

    if argv is None:
        argv = sys.argv

    if "--help" in argv or len(argv) < 2:
        print(HELP_TEXT)
        return

    if "--version" in argv or "-version" in argv:
        print(f"filecheck version {importlib.metadata.version('filecheck')}")
        return

    opts = parse_argv_options(argv)
    m = Matcher.from_opts(opts)
    sys.exit(m.run())
