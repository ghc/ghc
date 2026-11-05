import sys
from enum import Flag, auto

COLOR_SUPPORT = hasattr(sys.stdout, "isatty") and sys.stdout.isatty()


class FMT(Flag):
    RED = auto()
    BLUE = auto()
    YELLOW = auto()
    GREEN = auto()
    ORANGE = auto()
    BOLD = auto()
    GRAY = auto()
    UNDERLINE = auto()
    RESET = auto()

    def __str__(self) -> str:
        if not COLOR_SUPPORT:
            return ""
        fmt_str: list[str] = []

        if FMT.RED in self:
            fmt_str.append("\033[31m")
        if FMT.ORANGE in self:
            fmt_str.append("\033[33m")
        if FMT.GRAY in self:
            fmt_str.append("\033[37m")
        if FMT.GREEN in self:
            fmt_str.append("\033[32m")
        if FMT.BLUE in self:
            fmt_str.append("\033[34m")
        if FMT.YELLOW in self:
            fmt_str.append("\033[93m")
        if FMT.BOLD in self:
            fmt_str.append("\033[1m")
        if FMT.RESET in self:
            fmt_str.append("\033[0m")
        if FMT.UNDERLINE in self:
            fmt_str.append("\033[4m")

        return "".join(fmt_str)


WARN = FMT.ORANGE | FMT.UNDERLINE
ERR = FMT.RED | FMT.BOLD
