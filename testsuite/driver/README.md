GHC Driver Readme
=================

Greetings and well met. 
If you are reading this, I can only assume that you are likely interested in working on the testsuite in some capacity.
For more detailed documentation, please see [here][1].

## ToC

1. Entry points of the testsuite
2. Quick overview of program parts
3. Important Types
4. Quick answers for "how do I do X"?


## Entry Points of the Testsuite

The testsuite has two main entry points depending on which perspective you approach it.
From the perspective of the /user/, the entry point is the collect_stats function.
This function is declared in perf_notes.py along with its associated infrastructure.
The purpose of the function is to tell the test driver what metrics to compare when processing the test.
From the perspective of the test-suite, its entry point is the runtests.py file.
In that file contains the main logic for running the individual tests, collecting information, handling failure, and outputting the final results.

## Quick overview of program parts

The relevant bits of the directory tree are as such:

```
├── driver                   -- Testsuite driver directory
    ├── junit.py             -- Contains code implementing JUnit features.
    ├── kill_extra_files.py  -- Some of the uglier implementation details.
    ├── perf_notes.py        -- Code for a comparison tool and performance tests.
    ├── runtests.py          -- Main entrypoint for program; runs tests.
    ├── testglobals.py       -- Declares global data structures and objects.
    ├── testlib.py           -- Bulk of implementation is in here.
    └── testutil.py          -- Misc helper functions.
├── mk
    └── test.mk              -- Master makefile for running tests.
├── tests                    -- Main tests directory.
```

## Important Types

* metric
    - Internal to perf_notes.py, this has the same type as parse_git_notes
* parse_git_notes
    - Exists in testutil.py for legacy reasons but spiritually belongs in perf_notes.py
    - Returns a list of tests; each test is a dict of the form
      ```
      { 'test_env' : 'val',
        'test'    : 'val',
        'way'     : 'val',
        'metric'  : 'val',
        'value'   : 'val',
        'commit'  : 'val', }
        ```
* stats_range_fields
    - Exists in testglobals.py.
    - Is a list of tuples of the form `('metric', value, allowed deviation)`
        Note that this value is one that we collect from git notes and does not
        represent the value outputted from running the test.
* accumulate_metrics
    - Exists in testglobals.py
    - Is a list of strings in the form `'\t'.join('test_env','test','way','metric','value')`
        This is what is written to the git notes for the HEAD commit.
        As such, this 'value' /does/ represent the actual performance measured during the test-suite's run.

## Quick Answers for "How do I do X?"

* Q: How do I add a flag to the make test to extend the functionality of the testsuite?
    1. Add the flag in the appropriate global object in testglobals.py
    2. Add an argument to the parser in runtests.py that sets the flag
    3. Go to the `testsuite/mk/test.mk` file and add a new ifeq (or ifneq) block.
        I suggest adding the block around line 200.
* Q: How do I modify how performance tests work?
    * That functionality resides in perf_notes.py which has pretty good in-comment documentation.
    * Additionally, one will want to look at `compile_and_run`, `simple_run`, and `simple_build` in testutil.py

  [1]: http://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests
