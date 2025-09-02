import re

def drop_callstack(s):
     # Remove dieProgress call stack blocks (Cabal commit eab5a10d0b...).
     # Also remove the preceding blank line (if any) to avoid leaving an extra blank.
     s = re.sub(r'(?ms)\n?\s*CallStack \(from HasCallStack\):\n(?:\s+.*\n)*', '\n', s)
     # Collapse any runs of >1 blank line to a single newline.
     return re.sub(r'\n{3,}', '\n\n', s)

test('cabal03',
     [extra_files(['Setup.lhs', 'p/', 'q/', 'r/']),
      js_broken(22349),
      normalise_errmsg_fun(drop_callstack)],
     makefile_test,
     [])
