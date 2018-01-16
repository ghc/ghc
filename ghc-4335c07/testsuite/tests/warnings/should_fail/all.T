import re

def normalise_whitespace_carefully(s):
    # Merge contiguous whitespace characters into a single space
    # except on caret diagnostic lines
    return '\n'.join(line
                     if re.match(r'\s*\d*\s*\|', line)
                     else ' '.join(w for w in line.split())
                     for line in s.split('\n'))

test('WerrorFail', normal, compile_fail, [''])
test('WerrorFail2', normal, compile_fail, [''])
test('CaretDiagnostics1',
     [normalise_whitespace_fun(normalise_whitespace_carefully)],
     compile_fail,
     ['-fdiagnostics-show-caret -ferror-spans'])
test('CaretDiagnostics2', normal, compile_fail, ['-fdiagnostics-show-caret'])
