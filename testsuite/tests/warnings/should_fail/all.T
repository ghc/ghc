test('WerrorFail', normal, compile_fail, [''])
test('CaretDiagnostics1', normal, compile_fail, ['-fdiagnostics-show-caret -ferror-spans'])
test('CaretDiagnostics2', normal, compile_fail, ['-fdiagnostics-show-caret'])
