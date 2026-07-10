# Developing Shake

I welcome contributions. Generally:

* Don't spend too much time working on something before raising an issue for it. There might be suggestions I can give, or reasons the work might not be appropriate.
* Pull requests are the best way to submit stuff.

### Development Workflow

You can load Shake up in `ghci`, starting from the root directory, which has a `.ghci` file to set things up. Develop, hit `:reload` to reload, then `:test` to run the test suite.

Individual test can be run interactively:

    $ :main help
    $ :main TEST_NAME test

Replace `TEST_NAME` with one of the tests listed.

I usually use [`ghcid`](https://github.com/ndmitchell/ghcid#readme) when developing Shake.

### Continuous Integration

Shake is currently tested by on Linux by [Travis](https://travis-ci.org/ndmitchell/shake), and on Windows by [Appveyor](https://ci.appveyor.com/project/ndmitchell/shake). The tests are designed to be aggressively comprehensive (testing things like performance and that documentation all type checks), so while they usually pass, do not be overly concerned if they are failing or if your patch breaks a test -- serious issues will be spotted very quickly.

### Sandboxes

The tests do things like recompilation, which isn't particularly Cabal sandbox friendly. You can run some tests in the sandbox by doing:

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal repl shake-test
    $ :main TEST_NAME test --no-report
