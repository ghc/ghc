test('T9579_stackoverflow_rtsnone',
     [exit_code(2),
      extra_clean([ 'tmp_T9579_stackoverflow_rtsnone/Main.hi',
                    'tmp_T9579_stackoverflow_rtsnone/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtsnone \
       && ./T9579_stackoverflow_rtsnone'])

test('T9579_stackoverflow_rtssome',
     [exit_code(2),
      extra_clean([ 'tmp_T9579_stackoverflow_rtssome/Main.hi',
                    'tmp_T9579_stackoverflow_rtssome/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtssome \
       && ./T9579_stackoverflow_rtssome'])

test('T9579_stackoverflow_rtsall',
     [exit_code(2),
      extra_clean([ 'tmp_T9579_stackoverflow_rtsall/Main.hi',
                    'tmp_T9579_stackoverflow_rtsall/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtsall \
       && ./T9579_stackoverflow_rtsall'])

test('T9579_stackoverflow_rtsall_no_suggestions',
     [exit_code(2),
      extra_clean([ 'tmp_T9579_stackoverflow_rtsall_no_suggestions/Main.hi',
                    'tmp_T9579_stackoverflow_rtsall_no_suggestions/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtsall_no_suggestions \
       && ./T9579_stackoverflow_rtsall_no_suggestions'])

test('T9579_outofheap_rtsnone',
     [exit_code(251),
      extra_clean([ 'tmp_T9579_outofheap_rtsnone/Main.hi',
                    'tmp_T9579_outofheap_rtsnone/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtsnone \
      && ./T9579_outofheap_rtsnone'])

test('T9579_outofheap_rtssome',
     [exit_code(251),
      extra_clean([ 'tmp_T9579_outofheap_rtssome/Main.hi',
                    'tmp_T9579_outofheap_rtssome/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtssome \
       && ./T9579_outofheap_rtssome'])

test('T9579_outofheap_rtsall',
     [exit_code(251),
      extra_clean([ 'tmp_T9579_outofheap_rtsall/Main.hi',
                    'tmp_T9579_outofheap_rtsall/Main.o' ])],

       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtsall \
       && ./T9579_outofheap_rtsall'])

test('T9579_outofheap_rtsall_no_suggestions',
     [exit_code(251),
      extra_clean([ 'tmp_T9579_outofheap_rtsall_no_suggestions/Main.hi',
                    'tmp_T9579_outofheap_rtsall_no_suggestions/Main.o' ])],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtsall_no_suggestions \
       && ./T9579_outofheap_rtsall_no_suggestions'])
