test('T1791',
     [ exit_code(0)
     , js_skip # The JS backend doesn't detect heap overflows
     ],
     run_command,
     ['''"$MAKE" -s --no-print-directory T1791 >/dev/null && ./T1791 +RTS -M8M'''])
