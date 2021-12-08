setTestOpts(no_lint)

test('amazonka-ec2',
     [ collect_compiler_stats('all',3),
       pre_cmd('$MAKE -s --no-print-directory amazonka-ec2-prep'),
       extra_files(['Setup.hs', 'core/', 'gen/']),
       compile_timeout_multiplier(5)
     ],
     multimod_compile,
     ['Network.AWS.EC2.Types.Product', '-igen -package-db tmp.d -v0'])
