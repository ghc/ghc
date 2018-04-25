Integration Tests
=================

Each test is a shell script.  Tests that share files (e.g., `.cabal` files) are
grouped under a common sub-directory of [IntegrationTests].  The framework
copies the whole group's directory before running each test, which allows tests
to reuse files, yet run independently.  A group's tests are further divided into
`should_run` and `should_fail` directories, based on the expected exit status.
For example, the test
`IntegrationTests/exec/should_fail/exit_with_failure_without_args.sh` has access
to all files under `exec` and is expected to fail.

Tests can specify their expected output.  For a test named `x.sh`, `x.out`
specifies `stdout` and `x.err` specifies `stderr`.  Both files are optional.
The framework expects an exact match between lines in the file and output,
except for lines beginning with "RE:", which are interpreted as regular
expressions.

[IntegrationTests.hs] defines several environment variables:

* `CABAL` - The path to the executable being tested.
* `GHC_PKG` - The path to ghc-pkg.
* `CABAL_ARGS` - A common set of arguments for running cabal.
* `CABAL_ARGS_NO_CONFIG_FILE` - `CABAL_ARGS` without `--config-file`.

[IntegrationTests]: IntegrationTests
[IntegrationTests.hs]: IntegrationTests.hs
