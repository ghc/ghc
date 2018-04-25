# some numbers like "(1 MB)" would still remain.
# but let's just assume the actual difference in bytes
# is too small to have an effect on the rounded megabyte value.

def T9579_run_test(binName, expExitCode):
    fmt = lambda s: s.format(binName=binName)
    # 1. compile the binary using MAKE
    # 2. redirect stderr to stdout
    # 3. capture exitcode using echo
    # 4. replace actual number with NUM
    testCommandTemplate = """
          $MAKE -s --no-print-directory T9579_{binName} \
        && ( ( ./T9579_{binName} 2>&1; echo $?) \
           | sed -e 's/[0-9]* bytes/NUM bytes/g' ) \
    """

    test(fmt('T9579_{binName}'),
         [extra_files(['OutOfHeap.hs', 'StackOverflow.hs'])],
          run_command,
          [fmt(testCommandTemplate)])

T9579_run_test('stackoverflow_rtsnone',2)
T9579_run_test('stackoverflow_rtssome',2)
T9579_run_test('stackoverflow_rtsall',2)
T9579_run_test('stackoverflow_rtsall_no_suggestions',2)

T9579_run_test('outofheap_rtsnone',251)
T9579_run_test('outofheap_rtssome',251)
T9579_run_test('outofheap_rtsall',251)
T9579_run_test('outofheap_rtsall_no_suggestions',251)
