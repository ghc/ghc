class MeaningfulMainReturn:
  setTestOpts(extra_files(['MeaningfulMainReturn.hs', 'Imported.hs']))

  def compile_ok(name, main, enabled, extra=''):
      extension = '-XMeaningfulMainReturn' if enabled else '-XNoMeaningfulMainReturn'
      opts = f'-v0 -main-is MeaningfulMainReturn.{main} {extension} {extra}'
      test(name, normal, multimod_compile, ['MeaningfulMainReturn', opts])

  def compile_fail(name, main, enabled, extra=''):
      extension = '-XMeaningfulMainReturn' if enabled else '-XNoMeaningfulMainReturn'
      opts = f'-v0 -main-is MeaningfulMainReturn.{main} {extension} {extra}'
      test(name, normal, multimod_compile_fail, ['MeaningfulMainReturn', opts])

  def run(name, main, enabled, setup=normal, extra=''):
      extension = '-XMeaningfulMainReturn' if enabled else '-XNoMeaningfulMainReturn'
      opts = f'-v0 -main-is MeaningfulMainReturn.{main} {extension} {extra}'
      setups = [only_ways(['normal'])]
      if setup != normal:
          setups.append(setup)
      test(name, setups, multimod_compile_and_run, ['MeaningfulMainReturn', opts])

  compile_ok('MainReturn_unit_disabled', 'mainUnit', False)
  compile_ok('MainReturn_unit_enabled', 'mainUnit', True)
  run('MainReturn_unit_disabled_run', 'mainUnit', False)
  run('MainReturn_unit_enabled_run', 'mainUnit', True)

  compile_ok('MainReturn_void_disabled', 'mainVoid', False)
  compile_ok('MainReturn_void_enabled', 'mainVoid', True)
  run('MainReturn_void_disabled_run', 'mainVoid', False)
  run('MainReturn_void_enabled_run', 'mainVoid', True)

  compile_ok('MainReturn_int_disabled', 'mainInt', False)
  compile_ok('MainReturn_int_disabled_no_warning', 'mainInt', False, '-Wno-ambiguous-main-return')
  compile_fail('MainReturn_int_enabled', 'mainInt', True)
  run('MainReturn_int_disabled_run', 'mainInt', False)

  compile_ok('MainReturn_exit_success_disabled', 'mainExitSuccess', False)
  compile_ok('MainReturn_exit_success_enabled', 'mainExitSuccess', True)
  run('MainReturn_exit_success_disabled_run', 'mainExitSuccess', False)
  run('MainReturn_exit_success_enabled_run', 'mainExitSuccess', True)

  compile_ok('MainReturn_exit_code_disabled', 'mainExitFailure', False)
  compile_ok('MainReturn_exit_code_enabled', 'mainExitFailure', True)
  run('MainReturn_exit_failure_disabled_run', 'mainExitFailure', False)
  run('MainReturn_exit_failure_enabled_run', 'mainExitFailure', True, exit_code(7))

  compile_ok('MainReturn_recursive_disabled', 'mainRecursive', False)
  compile_ok('MainReturn_recursive_enabled', 'mainRecursive', True)

  compile_ok('MainReturn_polymorphic_disabled', 'mainPolymorphic', False)
  compile_ok('MainReturn_polymorphic_enabled', 'mainPolymorphic', True)
  run('MainReturn_polymorphic_disabled_run', 'mainPolymorphic', False)
  run('MainReturn_polymorphic_enabled_run', 'mainPolymorphic', True)

  compile_ok('MainReturn_exit_code_bottom_disabled', 'mainExitCodeBottom', False)
  compile_ok('MainReturn_exit_code_bottom_enabled', 'mainExitCodeBottom', True)
  run('MainReturn_exit_code_bottom_disabled_run', 'mainExitCodeBottom', False)
  run('MainReturn_exit_code_bottom_enabled_run', 'mainExitCodeBottom', True, exit_code(1))

  compile_ok('MainReturn_exit_code_alias_disabled', 'mainExitCodeAlias', False)
  compile_ok('MainReturn_exit_code_alias_enabled', 'mainExitCodeAlias', True)
  run('MainReturn_exit_code_alias_disabled_run', 'mainExitCodeAlias', False)
  run('MainReturn_exit_code_alias_enabled_run', 'mainExitCodeAlias', True, exit_code(8))

  compile_ok('MainReturn_newtype_disabled', 'mainExitCodeNewtype', False)
  compile_fail('MainReturn_newtype_enabled', 'mainExitCodeNewtype', True)
  run('MainReturn_newtype_disabled_run', 'mainExitCodeNewtype', False)

  compile_ok('MainReturn_family_exit_code_enabled', 'mainFamilyExitCode', True)
  run('MainReturn_family_exit_code_enabled_run', 'mainFamilyExitCode', True, exit_code(10))

  compile_ok('MainReturn_family_unit_disabled', 'mainFamilyUnit', False)
  compile_ok('MainReturn_family_unit_enabled', 'mainFamilyUnit', True)

  compile_ok('MainReturn_family_invalid_disabled', 'mainFamilyInvalid', False)
  compile_fail('MainReturn_family_invalid_enabled', 'mainFamilyInvalid', True)

  compile_ok('MainReturn_family_always_exit_disabled', 'mainFamilyAlwaysExit', False)
  compile_ok('MainReturn_family_always_exit_enabled', 'mainFamilyAlwaysExit', True)
  run('MainReturn_family_always_exit_disabled_run', 'mainFamilyAlwaysExit', False)
  run('MainReturn_family_always_exit_enabled_run', 'mainFamilyAlwaysExit', True, exit_code(12))

  compile_ok('MainReturn_family_stuck_disabled', 'mainFamilyUnconstrained', False)
  compile_fail('MainReturn_family_stuck_enabled', 'mainFamilyUnconstrained', True)

  compile_ok('MainReturn_constrained_disabled', 'mainConstrained', False)
  compile_ok('MainReturn_constrained_enabled', 'mainConstrained', True)
  run('MainReturn_constrained_disabled_run', 'mainConstrained', False)
  run('MainReturn_constrained_enabled_run', 'mainConstrained', True, exit_code(13))

  compile_ok('MainReturn_imported_disabled', 'main', False)
  compile_ok('MainReturn_imported_enabled', 'main', True)
  run('MainReturn_imported_disabled_run', 'main', False)
  run('MainReturn_imported_enabled_run', 'main', True, exit_code(14))

  compile_ok('MainReturn_imported_invalid_disabled', 'mainInvalid', False)
  compile_fail('MainReturn_imported_invalid_enabled', 'mainInvalid', True)
  run('MainReturn_imported_invalid_disabled_run', 'mainInvalid', False)

  compile_ok('MainReturn_imported_wrapped_disabled', 'mainImportedWrapped', False)
  compile_ok('MainReturn_imported_wrapped_enabled', 'mainImportedWrapped', True)
  run('MainReturn_imported_disabled_wrapped_run', 'main', False)
  run('MainReturn_imported_enabled_wrapped_run', 'main', True, exit_code(14))

  compile_ok('MainReturn_typeapp_disabled', 'mainTypeApp', False)
  compile_ok('MainReturn_typeapp_enabled', 'mainTypeApp', True)
  run('MainReturn_typeapp_disabled_run', 'mainTypeApp', False)
  run('MainReturn_typeapp_enabled_run', 'mainTypeApp', True, exit_code(15))
