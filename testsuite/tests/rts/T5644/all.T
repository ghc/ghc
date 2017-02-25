test('T5644', [extra_files(['Conf.hs', 'ManyQueue.hs', 'Util.hs', 'heap-overflow.hs']),
               
                only_ways(['optasm','threaded1','threaded2']),
                extra_run_opts('+RTS -M20m -RTS'),
                exit_code(251) # RTS exit code for "out of memory"
              ],
              multimod_compile_and_run,
              ['heap-overflow.hs','-O'])
