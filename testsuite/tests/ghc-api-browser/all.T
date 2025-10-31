# makefile_test/run_command is skipped when config.target_wrapper is
# not None, see test_common_work in testsuite/driver/testlib.py. for
# now just use this workaround to run custom test script here; ideally
# we'd fix test failures elsewhere and enable
# makefile_test/run_command for cross targets some day.
async def stub_run_command(name, way, cmd):
  return await run_command(name, way, cmd)


# config.target_wrapper is prepended when running any command when
# testing a cross target, see simple_run in
# testsuite/driver/testlib.py. this is problematic when running a host
# test script. for now do this override; ideally we'd have clear
# host/target distinction for command invocations in the testsuite
# driver instead of just a command string.
def override_target_wrapper(name, opts):
  opts.target_wrapper = ""


setTestOpts(
  [
    unless(arch("wasm32"), skip),
    override_target_wrapper,
    high_memory_usage,
    ignore_stderr,
    only_ways(["dyn"]),
    extra_ways(["dyn"]),
  ]
)


test(
  "playground001",
  [
    # pretty heavyweight, just test one browser for now.
    unless("FIREFOX_LAUNCH_OPTS" in ghc_env, skip),
    extra_files(
      [
        "../../../.gitlab/hello.hs",
        "../../../utils/jsffi/dyld.mjs",
        "../../../utils/jsffi/post-link.mjs",
        "../../../utils/jsffi/prelude.mjs",
        "index.html",
        "playground001.hs",
        "playground001.js",
        "playground001.sh",
      ]
    ),
  ],
  stub_run_command,
  ['./playground001.sh "$FIREFOX_LAUNCH_OPTS"'],
)
