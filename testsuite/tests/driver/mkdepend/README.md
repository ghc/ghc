# `ghc -M` tests

`ghc -M` can output Makefile and `jsonl` files.

The output can be tricky to compare platform independently, especially when the flags
`-include-pkg-deps` and `-include-cpp-deps` are involved.

We have a couple of test output normalizers in testlib.py that make the test output stable by:

* Replacing package unit-ids with placeholders.
* Removing cpp deps that use absolute paths.
* Replacing absolute paths in the Makefile with `<absolute>` placeholder.

Further, to make the output comparable, we format the `jsonl` lines nicely and sort the keys
to avoid false test failures, and make it easier to compare if the output changes.

The normalizers are invoked when the tests are run, but the output of the normalizers is not written to the
test files.

We add two shell scripts here that apply the same steps as the test output normalizers, but allowing us to have
readable golden files.
These files are:

* `./format_json.sh FILE...`
* `./format_makefile.sh FILE...`

They update the given files in-place and need to be invoked manually whenever the test output changes.
