Contributing to the Glasgow Haskell Compiler
============================================

So you've decided to hack on GHC, congratulations! We hope you have a
rewarding experience. This file will point you in the direction of
information to help you get started right away.

The GHC Developer's Wiki
========================

The home for GHC hackers is our Trac instance, located here:

<http://ghc.haskell.org/trac/ghc>

From here, you can file bugs (or look them up,) use the wiki, view the
`git` history, among other things. Of particular note is the building
page, which has the high level overview of the build process and how
to get the source:

<http://ghc.haskell.org/trac/ghc/wiki/Building>

Contributing patches to GHC in a hurry
======================================

Make sure your system has the necessary tools to compile GHC. You can
find an overview here:

<http://ghc.haskell.org/trac/ghc/wiki/Building/Preparation>

Next, clone the repository and all the associated libraries:

```
$ git clone --recursive git://git.haskell.org/ghc.git
```

On Windows, you need an extra repository containing some build tools.
These can be downloaded for you by configure. This only needs to be done once by running:

```
$ ./configure --enable-tarballs-autodownload
```

First copy `mk/build.mk.sample` to `mk/build.mk` and ensure it has
your preferred build settings. (You probably want to at least set
`BuildFlavour` to `quick`):

```
$ cp mk/build.mk.sample mk/build.mk
$ ... double-check mk/build.mk ...
```

Now build. The convenient `validate` script will build the tree in a way which
is both quick to build and consistent with our testsuite,

```
$ ./validate --build-only
```

You can use the `./inplace/bin/ghc-stage2` binary to play with the
newly built compiler.

Now, hack on your copy and rebuild (with `make`) as necessary.

Then start by making your commits however you want. When you're done, you can submit
 a pull request on Github for small changes. For larger changes the patch needs to be
 submitted to [Phabricator](https://phabricator.haskell.org/) for code review.
 The GHC Trac Wiki has a good summary for the [overall process](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/FixingBugs)
 as well as a guide on 
 [how to use Phabricator/arcanist](https://ghc.haskell.org/trac/ghc/wiki/Phabricator).


Useful links:
=============

An overview of things like using git, the release process, filing bugs
and more can be located here:

<http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions>

You can find our coding conventions for the compiler and RTS here:

<http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle>
<http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Conventions>

A high level overview of the bug tracker:

<http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/BugTracker>

If you're going to contribute regularly, **learning how to use the
build system is important** and will save you lots of time. You should
read over this page carefully:

<http://ghc.haskell.org/trac/ghc/wiki/Building/Using>

How to communicate with us
==========================

GHC is a big project, so you'll surely need help. Luckily, we can
provide plenty through a variety of means!

## IRC

If you're an IRC user, be sure to drop by the official `#ghc` channel
on [freenode](http://freenode.org). Many (but not all) of the
developers and committers are actively there during a variety of
hours.

## Mailing lists

In the event IRC does not work or if you'd like a bigger audience, GHC
has several mailing lists for this purpose. The most important one is
[ghc-devs](http://www.haskell.org/pipermail/ghc-devs/), which is where
the developers actively hang out and discuss incoming changes and
problems.

There is no strict standard about where you post patches - either in
`ghc-devs` or in the bug tracker. Ideally, please put it in the bug
tracker with test cases or relevant information in a ticket, and set
the ticket status to `patch`. By doing this, we'll see the patch
quickly and be able to review. This will also ensure it doesn't get
lost. But if the change is small and self contained, feel free to
attach it to your email, and send it to `ghc-devs`.

Furthermore, if you're a developer (or want to become one!) you're
undoubtedly also interested in the other mailing lists:

 * [glasgow-haskell-users](http://www.haskell.org/mailman/listinfo/glasgow-haskell-users)
   is where developers/users meet.
 * [ghc-tickets](http://www.haskell.org/mailman/listinfo/ghc-tickets)
   for email from Trac.
 * [ghc-builds](http://www.haskell.org/mailman/listinfo/ghc-builds)
   for nightly build emails.
 * [ghc-commits](http://www.haskell.org/mailman/listinfo/ghc-commits)
   for commit messages when someone pushes to the repository.

El fin
======

Happy Hacking!  -- The GHC Team
