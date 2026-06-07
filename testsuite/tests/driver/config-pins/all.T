import sys

# Python run_command rather than compiled Haskell: wasm32/WASI targets cannot
# call runInteractiveProcess from compiled binaries (no process creation in
# WASI).  Python scripts run on the host regardless of the target under test.

test('GhcInfoPins',
     [extra_files(['ConfigPinsExpected.py', 'GhcInfoPins.py']),
      ignore_stdout],
     run_command,
     [sys.executable + ' GhcInfoPins.py "' + config.compiler + '"'])

test('TestsuiteConfigPins',
     [extra_files(['ConfigPinsExpected.py', 'TestsuiteConfigPins.py']),
      ignore_stdout],
     run_command,
     [sys.executable + ' TestsuiteConfigPins.py ' + ' '.join([
         config.platform,
         config.arch,
         config.wordsize,
         str(int(config.have_ncg)),
         str(int(config.have_interp)),
         str(int(config.unregisterised)),
         str(int(config.tables_next_to_code)),
         str(int(config.leading_underscore)),
         str(int(config.target_has_smp)),
         str(int(config.have_RTS_linker)),
         str(int(config.interp_force_dyn)),
         str(int(config.cross)),
     ])])
