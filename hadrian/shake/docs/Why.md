# Why choose Shake?

_See also: [Shake links](https://github.com/ndmitchell/shake#readme); [Shake manual](Manual.md#readme)_

Shake is a library for writing build systems. Most large projects have a custom-written build system, and developers working on the project are likely to run the build system many times a day, spending a noticeable amount of time [waiting for the build system](https://xkcd.com/303/). This document explains why you might pick Shake over alternative tools for writing build systems (e.g. make, Ant, Scons). Shake primarily benefits two groups of people:

* **Developers running the build system** -- Shake based build systems run quickly, require little manual intervention and report estimated completion time as they go.
* **Developers writing the build system** -- Shake provides a powerful language for writing build systems, has excellent support for writing large robust build systems, can express many types of build rules and provides profiling information to help speed up builds.

In the rest of this document we explain and justify the above claims. Shake combines [cutting edge research](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf) with a [robust industrial-quality implementation](https://hackage.haskell.org/package/shake/). Shake is in constant use at many large organisations, including [a large investment bank](https://sc.com/), where it was originally developed and has been in use since 2009.

#### Expresses many types of build rule

Build systems run user supplied commands in an order satisfying dependencies. Many of the advantages of Shake are due to being able to express more powerful dependencies than other build tools. These dependency features ensure you can express the build system you want directly, without having to shoehorn your ideas into whatever dependencies your build tool provides. In particular, Shake can express both more dependencies (so things rebuild when they should) and more fine-grained dependencies (so things don't rebuild because something nearby changed).

* Shake build systems can discover additional dependencies after running previous rules, allowing the build system to generate files and then examine them to determine their dependencies, rather than predict the dependencies in advance. Such capabilities are essential when working with generated source files, but often allow build systems to be structured more logically.
* Most build systems only allow dependencies between files, but Shake provides user definable dependencies. By default Shake includes support for dependencies on files, the existence of files, environment variables, directory contents and several others, and adding more to your project is easy. In particular you can include dependencies on things like compiler versions or information stored on a remote machine.

#### Build systems run quickly

Developers are likely to spend a long time waiting for their build system, and consequently Shake is designed to be fast.

* The Shake implementation itself is highly optimised, in common with many build tools. In particular, Shake is designed for especially fast execution when nothing has changed -- a common case when developing.
* Shake benefits from its powerful dependencies, which can be more accurate and fine-grained, thus ensuring it only builds what is really necessary.
* Shake has excellent support for parallelism, fully utilising multicore machines. Shake also supports resource constraints, allowing builds to run with a higher level of parallelism than would otherwise be possible. As an example, you can limit disk-intensive operations (e.g. linking) without restricting CPU-intensive operations (e.g. compiling).
* Shake avoids rebuilding results where the dependencies are rebuilt but do not change, which is particularly useful for generated source files. The impact can reduce certain common patterns from build times of hours to build times of seconds.

#### Build systems run reliably

A build system is responsible for producing the files that developers are working with, so it is crucial that developers trust the result so they can properly investigate issues without considering build system involvement.

* The powerful dependency system ensures that all dependencies can be expressed, ensuring the build never leaves stale files.
* The Shake implementation itself has an extensive test suite, combining several examples projects and over 100 small unit tests (140 at the last count). In addition, a random build system generator allows extensive testing of key properties, including sufficient rebuilding and correctness in the presence of errors.
* Shake builds can be run in a special "lint" mode to check global invariants, detecting and reporting problems such as dependency violations before they cause problems.

#### Requires little manual intervention

Most build systems occasionally require manual intervention, typically wiping the existing build and starting again, when the build system developers change something fundamental. Shake eliminates the need for any manual intervention, reducing time wasted by users of the build system.

* The powerful dependencies ensure things that would normally require manual intervention can be tracked. For example, if the C compiler version is tracked as a dependency, then upgrading the C compiler will automatically rebuild all C files.
* Shake includes a version with each script, which can be changed to automatically force a complete rebuild.

#### Reports estimated completion time

Shake can report estimated completion time, allowing developers to plan their time better.

* Shake provides both predicted completion time (in minutes and seconds) and the percentage completed. All predictions are based on previously recorded execution times for rules and dynamic predictions of machine load, providing reasonable estimates.
* Shake provides methods to display this information in the title bar on Windows, Linux and Mac, and on Windows 7/8 can also display this information as a progress bar in the taskbar.
* The progress information can be easily integrated into continuous integration systems, such as Team City. 

#### Powerful language

Shake is implemented as a Haskell library, and Shake build systems are structured as Haskell programs which make heavy use of the Shake library functions. Shake is a delicate balance, providing access to the full power of Haskell (so build systems are not limited), yet also not requiring Haskell knowledge (suitable for any programmer).

* By building on top of Haskell, Shake build systems benefit from a powerful standardised language. Having a full language available ensures that anything that would be unsuitable to express in a build system can be implemented in Haskell and used seamlessly.
* While Shake build systems are Haskell programs, they can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means.

#### Supports large robust systems

Shake build systems can scale to tens of thousands of lines without becoming unwieldy.

* Shake uses Haskell to provide facilities for properly structuring large projects. In particular, Shake build systems can use functions to reuse common functionality, modules to group functions into separate files and packages to allow reusing and sharing modules.
* The types and utility functions provided by Shake eliminate certain classes of common error, making it harder to express invalid build systems.
* The lint mode performs sanity checks of the build system, allowing errors to be caught sooner.

#### Provides profiling information

Shake can generate profiling information allowing developers to both understand the current system, and identify opportunities for improvement.

* The Shake profiling reports are standalone web pages containing plots, tables and graphs of useful information.
* The report can be used to speed up by the build by identifying which commands are most expensive, which files cause most rebuilding and any bottlenecks in parallelism.
* The report can examine details of the last run, providing information about what built and why.
* Profiles are always recorded, allowing profile reports to be generated after a run completes, without requesting any "profiling mode" in advance. Shake ensures profiling information is recorded with no measurable performance impact.
* Graphs can be generated showing dependencies, usually grouped by either file type or location, making it easy to see the overall structure of the build.
* Reports can be mined using a powerful querying language to determine custom information about your build.

#### Why not?

This document sets out the reasons you _should_ use Shake, but naturally there are some disadvantages:

* Shake build systems are written in Haskell. While that provides many benefits, it does mean the Shake syntax follows that of Haskell, and some Shake errors are reported by the compiler as Haskell type errors. Despite being written in Haskell, the [user manual](Manual.md#readme) introduces Shake assuming no Haskell knowledge, so Haskell knowledge is not a requirement and hopefully should not be a barrier to using Shake.
* Shake is not likely to be installed by default, while Make almost always is.
* Shake does not provide default build rules, requiring everything to be expressed in your build system. In practice it seems that the default rules included with make are unsuitable for most large scale projects, but smaller projects may find themselves writing a few additional rules. It is hoped that additional experience with Shake will lead to a library of build rules being developed.
