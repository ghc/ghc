from enum import Enum

# Whether to emit color escapes; set in runtests.py.
enable_color = True

class Color(Enum):
    BLACK   = 30
    RED     = 31
    GREEN   = 32
    YELLOW  = 33
    BLUE    = 34
    MAGENTA = 35
    CYAN    = 36
    WHITE   = 37

def colored(color: Color, s: str) -> str:
    if enable_color:
        return '\033[1m\033[{}m{}\033[0m'.format(color.value, s)
    else:
        return s

# For renderers that serve several sinks: `enabled` says whether *this* sink
# takes color (the summary is written both to stdout and to a plain-text file).
def colored_if(enabled: bool, color: Color, s: str) -> str:
    return colored(color, s) if enabled else s
