# Note [Testing wasm ghci browser mode]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Wasm ghci browser mode supports connecting with a user-controlled
# browser tab or an automated headless browser tab. The wasm dyld
# script supports using puppeteer/playwright to launch the headless
# browser, and this can be used either interactively by the user, or
# fully automatically when running the GHC testsuite.
#
# The first issue is deciding whether to run these tests at all, even
# when we're indeed testing the wasm32 target! The user may be using
# nodejs without the optional npm dependencies needed to make browser
# mode works, and it's non-trivial to probe browser installations and
# infer the right puppeteer/playwright launch options.
#
# We must ensure the testsuite is still green when browser mode isn't
# supported, therefore we don't do anything fancy here. These browser
# tests are opt-in, the user must explicitly set environment variables
# like FIREFOX_LAUNCH_OPTS to test a specific browser. These variables
# are *not* handled by the wasm dyld script to avoid confusion, the
# test runner is in charge of setting the right flags to invoke ghci.
#
# The next issue is #25415. The testsuite driver has more than a dozen
# different functions to "run something", some of which inserts a
# target wrapper, some of which doesn't, all without knowing if the
# thing to be run is a host or target thing! For instance, attempting
# to define these tests as ghci_script would result in the driver
# attempting to launch host ghci using the wasm interpreter. The same
# issue exists in many places other than ghci_script, and while the
# situation ought to be fixed, that is undoubtedly scope creep that
# belongs to a standalone chunk of work.
#
# To ensure wasm ghci browser mode is tested in CI without further
# complicating the testsuite driver, I choose to avoid any
# modifications to the driver and global config. So it's best to just
# define our own ghci_browser test function solely in this file, it
# does its job well for our rather specific use case.

setTestOpts(
  [
    unless(arch("wasm32"), skip),
    high_memory_usage,
    only_ways(["ghci", "ghci-opt"]),
    extra_ways(["ghci", "ghci-opt"]),
  ]
)


# you can pass more arguments here; the additional argument list is
# the last argument of test() calls. see do_test function in the
# driver and how it invokes the func argument for details.
async def ghci_browser(name_with_browser, way):
  nb = name_with_browser.rsplit("-", 1)
  name = nb[0]
  browser = nb[1]

  opts = getTestOpts()

  way_flags = " ".join(config.way_flags[way])
  flags = " ".join(get_compiler_flags())

  if browser == "firefox":
    o = ghc_env["FIREFOX_LAUNCH_OPTS"]
    browser_flags = f"-fghci-browser -fghci-browser-puppeteer-launch-opts='{o}'"
  elif browser == "chrome":
    o = ghc_env["CHROME_LAUNCH_OPTS"]
    browser_flags = f"-fghci-browser -fghci-browser-puppeteer-launch-opts='{o}'"
  else:
    assert browser == "webkit"
    o = ghc_env["WEBKIT_LAUNCH_OPTS"]
    browser_flags = f"-fghci-browser -fghci-browser-playwright-browser-type=webkit -fghci-browser-playwright-launch-opts='{o}'"

  # ghci_script formats the command then calls simple_run. simple_run
  # formats the command then calls runCmdPerf. runCmdPerf formats the
  # command and calls runCmd. runCmd formats the command then calls
  # the timeout program that calls /bin/sh which does another layer of
  # formatting. good luck making your command containing a json
  # payload to survive how many layers of pure insanity.
  cmd = (
    f"cd '{opts.testdir}' && "
    + f" {{compiler}} {way_flags} {flags} {opts.extra_run_opts}"
    + browser_flags.replace("{", "{{").replace("}", "}}")
  )

  exit_code = await runCmd(
    cmd,
    stdin=in_testdir(name, "script"),
    stdout=in_testdir(name, "run.stdout"),
    stderr=in_testdir(name, "run.stderr"),
    timeout_multiplier=opts.run_timeout_multiplier,
  )

  if exit_code != opts.exit_code:
    if config.verbose >= 1:
      print(
        f"Wrong exit code for {name_with_browser}({way})(expected {opts.exit_code}, actual {exit_code})",
      )
      dump_stdout(name)
      dump_stderr(name)
    message = format_bad_exit_code_message(exit_code)
    return failBecause(message)

  # don't handle stderr for now, i don't wanna write a filter for the
  # js stack traces
  if not (await stdout_ok(name, way)):
    return failBecause(
      "bad stdout", stderr=read_stderr(name), stdout=read_stdout(name)
    )

  return passed()


# for browser001 we generate tests like browser001-firefox, but we
# don't want to have duplicate script/stdout files in tree since the
# behavior should be coherent across all browsers. hence this layer of
# indirection: the rest of the test driver recognizes
# browser001-firefox as test name, though we explicitly add
# browser001.script/browser001.stdout as extra_files to ensure they're
# copied, and ghci_browser would properly make use of them.
def browser_test(name, setup, func, args):
  for browser in ["firefox", "chrome", "webkit"]:
    test(
      f"{name}-{browser}",
      [
        unless(f"{browser.upper()}_LAUNCH_OPTS" in ghc_env, skip),
        extra_files([f"{name}.script", f"{name}.stdout"]),
      ]
      + setup,
      func,
      args,
    )


browser_test("browser001", [], ghci_browser, [])
