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
test('WarningGroups', normal, compile_fail, [''])
test('CaretDiagnostics1',
     [normalise_whitespace_fun(normalise_whitespace_carefully)],
     compile_fail,
     ['-fdiagnostics-show-caret -ferror-spans'])
test('CaretDiagnostics2', normal, compile_fail, ['-fdiagnostics-show-caret'])
test('Colour', normal, compile_fail, ['-fdiagnostics-color=always'])
test('T20263', normal, compile_fail, ['-Wunicode-bidirectional-format-characters -Werror'])
test('WarningCategory1', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -Werror'])
test('WarningCategory2', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -Wall -Werror -Wno-x-harmless -Wwarn=x-mostly-harmless -Werror=x-dangerous'])
test('WarningCategory3', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile, ['WarningCategory1', '-v0 -Wall -Werror -Wno-extended-warnings'])
test('WarningCategory4', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -w -Wx-mostly-harmless -Wwarn=extended-warnings -Werror=x-dangerous -Wx-dangerous'])
test('WarningCategory5', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -Wwarnings-deprecations -Wno-deprecations -Werror=x-dangerous'])
test('WarningCategory6', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -Wno-extended-warnings -Wdeprecations -Werror=warnings-deprecations'])
test('WarningCategory7', [extra_files(['WarningCategory1.hs', 'WarningCategory1_B.hs', 'WarningCategoryModule.hs'])], multimod_compile_fail, ['WarningCategory1', '-v0 -Werror -w -Wall'])
test('WarningCategoryInvalid', normal, compile_fail, [''])
test('T24396c', normal, compile_fail, [''])
test('SpecEMultipleTys', normal, compile_fail, [''])
