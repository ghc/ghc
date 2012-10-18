This is a testsuite for Haddock that uses the concept of "golden files". That
is, it compares output files against a set of reference files.

To add a new test: 

 1. Create a module in the `html-test/src` directory.

 2. Run `cabal test`. You should now have `html-test/out/<modulename>.html`.
    The test passes since there is no reference file to compare with.

 3. To make a reference file from the output file, run

        html-test/accept.lhs <modulename>

Tips and tricks:

To "accept" all output files (copy them to reference files), run

    runhaskell accept.lhs

You can run all tests despite failing tests, like so

    cabal test --test-option=all

You can pass extra options to haddock like so

    cabal test --test-options='all --title="All Tests"'
