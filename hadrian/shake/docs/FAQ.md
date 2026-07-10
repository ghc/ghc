# FAQ

This page includes the more commonly asked questions. For less frequently asked questions, or to ask your own question, see the `shake-build-system` tag on [StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system).

#### Q: What else is on this website?

* [User manual](Manual.md) -- the place to start, which also serves as a tutorial.
* [Why](Why.md) -- details on why you should use Shake, for those who are easily influenced.
* [Includes](Includes.md) -- how to deal with `#include` files, import statements and other dependencies between files.
* [Debugging](Debugging.md) -- how to determine what a Shake build system is doing.
* [Profiling and optimisation](Profiling.md) -- how to speed up an existing Shake build system.
* [Command line flags](CommandLine.md) -- the flags and settings supported by Shake, a better version of `--help`.
* [Developing Shake](Developing.md) -- notes for people who want to contribute to Shake itself.
* [Ninja](Ninja.md) -- features of Shake for those people who use Ninja.
* [Lint](Lint.md) -- how to use the linting features.
* [Controlling rebuilds](Rebuilds.md) -- how to use the linting features.

<!--
Shake is suitable for all sizes of build systems, from a simple C project to a huge cross-platform multi-language project. However, at different scales, different techniques tend to be applicable.
* [Small/simple build systems](Small.md#readme) -- some simpler build systems can be written as _forward_ build systems, without the need to explicitly think about dependencies or targets. Useful for getting started, relies on a tool to automatically track your dependencies.
* [Large frequently changing build systems](Large.md#readme) -- for large build systems, it is useful to split the build system interpreter and metadata apart, making changes to the Haskell build system comparatively rare.
-->

#### Q: Any more documentation?

There is a complete list of [every function in Shake](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html) which [can be searched](https://hoogle.haskell.org/?package=shake). Each function comes with documentation and examples.

Much of the theory behind Shake is covered in [a conference paper](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf) which was accompanied by [this video](https://www.youtube.com/watch?v=xYCPpXVlqFM) ([slides](https://ndmitchell.com/downloads/slides-shake_before_building-10_sep_2012.pdf)). Since then I've given videoed talks on [small worked examples](https://www.infoq.com/presentations/shake) ([slides](https://ndmitchell.com/downloads/slides-building_stuff_with_shake-20_nov_2014.pdf)) and [how to structure large Shake systems](https://skillsmatter.com/skillscasts/6548-defining-your-own-build-system-with-shake) ([slides](https://ndmitchell.com/downloads/slides-defining_your_own_build_system_with_shake-09_oct_2015.pdf)).

I sometimes write about ongoing development work or other Shake-related things on [my blog](https://neilmitchell.blogspot.co.uk/search/label/shake).

If you have any further questions:

* [Ask on StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system), using the tag `shake-build-system`.
* [Email us](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.

#### Q: Is Shake limited to building Haskell?

Not at all -- Shake can build any project in any combination of languages. In fact, Shake isn't typically necessary for vanilla Haskell projects, as you can use [`cabal`](https://haskell.org/cabal) or [`stack`](https://haskellstack.org/). Shake is often used for building C/C++, Docker containers and JavaScript/HTML/CSS projects.

#### Q: Where are functions for string manipulation?

Shake is a Haskell package focused on providing build-system functionality. Since Shake scripts are written in Haskell, they can easily access other Haskell packages. Most general needs are met by the standard [`base` library](https://hackage.haskell.org/package/base), but a few other useful general functions can be found in [the `extra` library](https://hackage.haskell.org/package/extra) (e.g. [`trim`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:trim) and [`replace`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:replace)). For more specific functionality (e.g. parsing, databases, JSON) find a [suitable Haskell library](https://hackage.haskell.org/packages) and use that.

#### Q: Why is there a `shake` executable?

Most users will write their own Haskell file and compile it to produce an executable that is their build tool. The `shake` executable is there to [run the demo](Demo.md), run [Ninja build files](Ninja.md) and will also run a `Shakefile.hs` if present.

#### Q: Can file patterns overlap?

No. If two patterns overlap for a file being built it will result in a runtime error -- you cannot have a pattern for `*.txt`, and another for `foo.*`, and then build a file named `foo.txt`. For objects that typically share the same extension (e.g. C and Haskell both produce `.o` objects), either disambiguate with a different extension (e.g. `.c.o` and `.hs.o`), or different directory (e.g. `obj/c/**/.o` and `obj/hs/**/.o`). For more information, including ways to enable overlap and set priorities, see `%>`.

#### Q: Do multiple calls to `need` run sequentially? Are `Applicative` actions run in parallel?

In Shake, `need xs >> need ys` will build `xs` and `ys` in parallel, since version 0.17.10, in a similar manner to [Haxl](https://hackage.haskell.org/package/haxl). As a consequence, enabling the [`ApplicativeDo` extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ApplicativeDo) may cause more of the build run in parallel. However, this implicit parallelisation of adjacent dependencies is easy to loose, for example by defining `need` in terms of `>>=`. Users are encouraged to merge adjacent `need` operations (e.g. `need (xs++ys)`), and where that is not possible use `parallel` explicitly.

#### Q: Should file names be relative or absolute?

We recommend using only relative file names in Shake.

Shake can use file names that are either absolute (`C:\file.txt`, `/file.txt`) or relative (`file.txt`). However, Shake compares filenames by value, so if you `need` both `file.txt` and `/file.txt` it will attempt to build both, likely resulting in failure - within a single Shake project you must stick to either relative or absolute file names.

#### Q: How can I depend on directories?

Think of directories as containers for files. They exist or don't pretty randomly, but if they have files, they must exist. In particular, you can't depend on a directory with `need` or write a rule to create a directory. Directories are created as needed -- the rule for `bar/baz.exe` will create the `bar` directory if necessary. As some examples of "depending" on directories:

* If you want to depend on a `git clone` having being performed, depend on a particular checked-out file instead (e.g. `README.md`), with the action in the rule to create it being `git clone`.
* If you want to depend on a `node_modules` having been updated after changing `package.json`, create a stamp file by copying `package.json` after running `npm update` (see [this detailed answer](https://stackoverflow.com/questions/35938956/haskell-shake-special-rule-for-building-directories)).

There is a tracked function `doesDirectoryExist`, to depend on the presence or absence of a directory, but you should not call it on directories which might be created by the build system.

#### Q: Does Shake work with Continuous Integration?

Shake works well when run by Continuous Integration (CI) systems. There are a few tweaks that sometimes make Shake work even better for CI:

* Setting `shakeProgress` to `progressDisplay 5 putStrLn` will print the progress information to stdout every 5 seconds. In many CI systems, you can tag this information to display it through the UI - e.g. [TeamCity](https://confluence.jetbrains.com/display/TCD9/Build+Script+Interaction+with+TeamCity#BuildScriptInteractionwithTeamCity-ReportingBuildProgress) spots messages of the form `##teamcity[progressMessage 'message goes here']`.
* Setting `shakeCommandOptions` to `EchoStdout True` will ensure that the standard output of all failing commands is captured in the final error messages.
* Setting `shakeStaunch` to `True` will cause the CI to find as many errors as it can - taking longer to fail, but producing a great number of errors when it does.
* Using `shakeErrorsDatabase` you can collect the exceptions for many build rules, allowing them to be presented in a CI dashbaord.

#### Q: What GHC bugs has Shake found?

For some reason, Shake tends to find a reasonable number of serious bugs in GHC, given how few bugs there are generally in GHC. I suspect the reason is a combination of thorough testing including with GHC pre-releases. Some of the best bugs found by Shake are:

* [GHC bug 7646](https://gitlab.haskell.org/ghc/ghc/issues/7646), a race condition when closing file handles that had been in several releases.
* [GHC bug 10830](https://gitlab.haskell.org/ghc/ghc/issues/10830), `maximumBy` had a space leak in a released version.
* [GHC bug 11555](https://gitlab.haskell.org/ghc/ghc/issues/11555), `catch` wouldn't catch an `undefined` argument passed to it in a pre-release.
* [GHC bug 11830](https://gitlab.haskell.org/ghc/ghc/issues/11830), livelock when disabling idle GC in a pre-release.
* [GHC bug 11458](https://gitlab.haskell.org/ghc/ghc/issues/11458) (originally from [GHC bug 11379](https://gitlab.haskell.org/ghc/ghc/issues/11379)), serious issue with type applications producing dodgy programs in a pre-release.
* [GHC bug 11978](https://gitlab.haskell.org/ghc/ghc/issues/11978), segfaults when running certain profiling modes that weren't multithread safe.
* [GHC bug 10553](https://gitlab.haskell.org/ghc/ghc/issues/10553), `getEnvironment` was blank when run on PowerPC in `ghci`.
* [GHC bug 10549](https://gitlab.haskell.org/ghc/ghc/issues/10549), inconsistent optimisation flags leading to fatal errors in a pre-release.
* [GHC bug 10176](https://gitlab.haskell.org/ghc/ghc/issues/10176), invalid optimisations caused by a part of GHC that had been formally proved to be correct, in a pre-release.
* [GHC bug 10793](https://gitlab.haskell.org/ghc/ghc/issues/10793), `BlockedIndefinitelyOnMVar` can be raised even if the thread isn't indefinitely blocked.
* [GHC bug 15595](https://gitlab.haskell.org/ghc/ghc/issues/15595), `withArgs` on a limited stack loops forever consuming memory.
* [GHC bug 17575](https://gitlab.haskell.org/ghc/ghc/issues/17575), `runhaskell -package=Cabal` causes an error.
* [GHC bug 18221](https://gitlab.haskell.org/ghc/ghc/issues/18221), `forkOn` has complexity _O(n^2)_.
* [GHC bug 19413](https://gitlab.haskell.org/ghc/ghc/-/issues/19413), `unsafePerformIO` optimised wrongly.

#### Q: What's the history of Shake?

I ([Neil Mitchell](https://ndmitchell.com)) was one of the people behind the [Yhc project](https://www.haskell.org/haskellwiki/Yhc), a Haskell compiler that died in a large part because of its build system. To quote from [the final blog post](https://yhc06.blogspot.co.uk/2011/04/yhc-is-dead.html):

> The biggest challenge for Yhc was the build system -- we ended up with 10,000 lines of Python Scons scripts. Without a robust build system nothing else matters. When our sole Python hacker left the team that was the beginning of the end.

A Haskell compiler is a big undertaking, but the build system for a simple Haskell compiler shouldn't be that complicated.

When writing my thesis I needed a build system, and decided to try writing a simple Haskell DSL, which is still online [here](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs). I defined a single operator [`<==`](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs#L71) which let me express a relationship between an output and its dependencies -- very simple, but it worked.

Later I moved to [Standard Chartered](https://www.sc.com/), where the build system was a mass of Makefiles, and it quickly became apparent that the project had outgrown the current approach. Without really surveying the alternatives, I decided that a Haskell DSL would be easiest to fit in with the existing infrastructure, so started writing some code. The first version of the build library took under a week, followed by a month of reimplementing the existing system. It wasn't until many months later I realised that the reason everything was suddenly so much easier was because we had monadic dependencies.

While people at Standard Chartered wanted to open source Shake, that turned out not to be possible. A number of people in the Haskell community implemented their own versions of Shake, but none were as polished or as strong as our internal one. Eventually, I reimplemented Shake, from scratch, in my spare time. Writing Shake from scratch, without the original source code or documentation, it naturally turned out better than the first attempt. A while later Standard Chartered migrated to the open-source version.

I still maintain Shake, but am fortunate to have [other contributors](https://github.com/ndmitchell/shake/graphs) extending and improving Shake. If you want to join in, see [notes for developers](Developing.md).
