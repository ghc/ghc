setTestOpts(js_broken(22261))
if have_profiling():
  setTestOpts(extra_ways(['prof']))

test('cont_simple_shift', [extra_files(['ContIO.hs'])], multimod_compile_and_run, ['cont_simple_shift', ''])
test('cont_exn_masking', [extra_files(['ContIO.hs'])], multimod_compile_and_run, ['cont_exn_masking', ''])
test('cont_missing_prompt_err', [extra_files(['ContIO.hs']), exit_code(1)], multimod_compile_and_run, ['cont_missing_prompt_err', ''])
test('cont_nondet_handler', [extra_files(['ContIO.hs'])], multimod_compile_and_run, ['cont_nondet_handler', ''])
test('cont_stack_overflow', [extra_files(['ContIO.hs'])], multimod_compile_and_run, ['cont_stack_overflow', '-with-rtsopts "-ki1k -kc2k -kb256"'])

test('T23513', [extra_files(['ContIO.hs'])], multimod_compile_and_run, ['T23513', ''])
