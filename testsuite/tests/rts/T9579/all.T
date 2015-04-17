test('T9579_stackoverflow_rtsnone',
     [exit_code(2)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtsnone && ./T9579_stackoverflow_rtsnone'])

test('T9579_stackoverflow_rtssome',
     [exit_code(2)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtssome && ./T9579_stackoverflow_rtssome'])

test('T9579_stackoverflow_rtsall',
     [exit_code(2)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_stackoverflow_rtsall && ./T9579_stackoverflow_rtsall'])

test('T9579_outofheap_rtsnone',
     [exit_code(251)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtsnone && ./T9579_outofheap_rtsnone'])

test('T9579_outofheap_rtssome',
     [exit_code(251)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtssome && ./T9579_outofheap_rtssome'])

test('T9579_outofheap_rtsall',
     [exit_code(251)],
       run_command,
     ['$MAKE -s --no-print-directory T9579_outofheap_rtsall && ./T9579_outofheap_rtsall'])
