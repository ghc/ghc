# Shake is...

* **A build system** -- an alternative to make, Scons, Ant etc.
* **Reliable and robust** -- having been relied on commercially for over five years.
* **Powerful** -- letting you express the problem precisely and directly.
* **Fast to run** -- both to build from scratch and to rebuild.

Large build systems written using Shake tend to be significantly simpler, while also running faster. If your project can use a canned build system (e.g. Visual Studio, cabal) do that; if your project is very simple use a Makefile; otherwise use Shake.

The original motivation behind the creation of Shake was to allow rules to discover additional dependencies after running previous rules, allowing the build system to generate files and then examine them to determine their dependencies -- something that cannot be expressed directly in most build systems. However, now Shake is a suitable build tool even if you do not require that feature.

[Click to read about why you should use Shake.](Why.md)

## Using Shake

To try Shake you need to install the [Haskell Stack](https://haskellstack.org/), then type `stack install shake`, then run `stack exec -- shake --demo`. The final step will create a sample Shake build system and run it (you should see [this output](Demo.md)).

To write your own Shake build system, read the [user manual](Manual.md) and refer to the [API documentation](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html). Further documentation on specific topics, including more examples, is available from [the FAQ](FAQ.md). Shake build systems are [Haskell](https://haskell.org/) programs, but can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means.

[Click to read the user manual.](Manual.md)

## Asking questions

Stuck? Confused? Frustrated? Please get in contact! If in doubt, just email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

You can ask questions on [StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`. You can email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) with anything about Shake. If you find a bug you can report it at the [GitHub issue tracker](https://github.com/ndmitchell/shake/issues). If your question needs to remain confidential you can [email me](https://ndmitchell.com/), although any insights from your question won't be public, so the other approaches are preferred.

## What else?

Shake can execute [Ninja files](Ninja.md), allowing integration with [CMake](https://www.cmake.org/) and [Meson](http://mesonbuild.com/). Shake can [predict completion time](Manual.md#progress), [profile build systems](Profiling.md) and [sanity check builds](Manual.md#lint). Shake is based on a robust underlying theory from [this academic paper](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf). Shake is an open source project under the [BSD license](https://github.com/ndmitchell/shake/blob/master/LICENSE) hosted on [GitHub](https://github.com/ndmitchell/shake/) with a [range of contributors](https://github.com/ndmitchell/shake/graphs).

## Who uses Shake?

Shake is used by lots of companies, but only a few have declared so publicly:

* [Standard Chartered](https://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/) use Shake to compile their [Checkpad MED](https://www.checkpad.de/) application, as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](https://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872), producing several [open-source projects](https://github.com/samplecount) for working with Shake.
* [CovenantEyes](http://www.covenanteyes.com/) use Shake to build their Windows client, as mentioned in their [tweet](https://twitter.com/eacameron88/status/543219899599163392).
* [Keystone Tower Systems](http://keystonetowersystems.com/) has a robotic welder with a Shake build system plus Haskell code running in the control system, as mentioned in their [tweet](https://twitter.com/eric_oconnor/status/581576757062434816).
* [FP Complete](https://www.fpcomplete.com/) use Shake to [create Docker images](https://www.fpcomplete.com/blog/2015/08/stack-docker#images).
* [codebender](https://codebender.cc/) use Shake to manage JavaScript packages that implement Arduino protocols and handle communication between the browser and Arduino devices.
* [Swift Navigation](https://www.swiftnav.com/) use Shake for their release process, as described by [their presentation](https://github.com/swift-nav/shake-before-make/blob/master/Shake.pdf).
* [NoRedInk](https://www.noredink.com/) use Shake [to build stuff](https://twitter.com/rtfeldman/status/1113968101060689920).
* [Digital Asset](https://www.digitalasset.com/) use Shake as part of their [DAML SDK IDE](https://daml.com/), as can be seen [in the central module](https://github.com/digital-asset/daml/blob/eed24b01ae4b0ca1bf024612354e88c15c9d30c0/compiler/haskell-ide-core/src/Development/IDE/State/Shake.hs).

There are several libraries providing pre-made rules and extending Shake:

* [shake-plus](https://hackage.haskell.org/package/shake-plus) wraps all of Shake to use the [Path library](https://hackage.haskell.org/package/path), [`ReaderT` wrappers](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Reader.html#t:ReaderT) and the [Text library](https://hackage.haskell.org/package/text).
* [shake-language-c](https://hackage.haskell.org/package/shake-language-c) allows cross-compiling C, C++ and Objective-C code to various target platforms.
* [shake-cpp](https://github.com/jfeltz/shake-cpp) is an abstraction layer for Shake, providing simple C++ rules.
* [kansas-lava-shake](https://hackage.haskell.org/package/kansas-lava-shake) provides a set of rules to work with [Kansas Lava](https://hackage.haskell.org/package/kansas-lava) and compile down to [Xilinx chips](https://www.xilinx.com/).
* [avr-shake](https://hackage.haskell.org/package/avr-shake) provides rules for building things with [AVR Crosspack](https://www.obdev.at/products/crosspack/index.html).
* [shake-minify](https://hackage.haskell.org/package/shake-minify) uses native Haskell code (no external `$PATH` dependencies) to minify CSS and JS files.
* [shake-pack](https://hackage.haskell.org/package/shake-pack) uses bz2 lib on the system to tar and bzip compress given files.

Some libraries push Shake into specific domains:

* [bioshake](https://github.com/PapenfussLab/bioshake) lets users define bioinformatics pipelines in a DSL that is executed in conjunction with Shake, also including support for cluster execution. There is also [a paper](https://www.biorxiv.org/content/biorxiv/early/2019/01/24/529479.full.pdf) describing the system.
* [neuron](https://neuron.zettel.page/) is a note-taking tool based on the Zettelkasten method, that uses Shake to monitor and rebuild as the Markdown note files change.
* [Slick](https://hackage.haskell.org/package/slick) is a static site generator written and configured using Haskell. Slick provides a small set of tools and combinators for building static websites on top of the Shake build system.
* [Rib](https://github.com/srid/rib#readme) is a library for writing your own static site generator, prioritizing the use of existing tools over reinventing them and thereby enabling the user to compose them together.

Several open-source projects make key use of Shake:

* [GHC](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake) uses a Shake-based build system named [Hadrian](https://github.com/snowleopard/hadrian).
* [Shakebook](https://shakebook.site/) uses Shake and [Pandoc](https://hackage.haskell.org/package/pandoc) to provide a website generator.
* [ToolCabal](https://github.com/TiborIntelSoft/ToolCabal) is a rewrite of [Cabal](https://www.haskell.org/cabal/) using Shake as the dependency engine.
* [ghc-make](https://github.com/ndmitchell/ghc-make) uses Shake to build programs with GHC, speeding up checking if the build is clean.
* [Ghcide](https://hackage.haskell.org/package/ghcide) is a Haskell IDE which uses Shake as its core.
* [shake-install](https://github.com/alphaHeavy/shake-install) helps build a set of cabal packages in the correct order.
* [OpenSUSE Haskell packaging](https://github.com/opensuse-haskell) makes use of Shake to [convert a Stack project to OBS](https://github.com/opensuse-haskell/cabal2obs).
* [Pier](https://github.com/judah/pier) is an alternative to [Stack](https://haskellstack.org/) using Shake as the dependency engine.

Here are a few tutorials and blog posts:

* [Many articles from the author of Shake](https://neilmitchell.blogspot.co.uk/search/label/shake), covering ongoing development.
* [Writing a simple Blog with Shake and Pandoc](http://declaredvolatile.org/blog/2014-09-14-writing-a-simple-blog-with-shake-and-pandoc/).

Finally, the ideas behind Shake have helped inspire other systems:

* [swift-llbuild](https://github.com/apple/swift-llbuild) used Shake as the inspiration for the mechanism `llbuild` uses to allow additional work to be discovered on the fly.

Do you have a link that should be included above? Let me know with a [tweet to `@ndm_haskell`](https://twitter.com/ndm_haskell).
