test('T1791',
     [ exit_code(0) ],
     run_command,
     ['''"$MAKE" -s --no-print-directory T1791 >/dev/null && ./T1791 +RTS -M8M'''])
