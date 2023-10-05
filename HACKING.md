Contributing to the Glasgow Haskell Compiler
============================================

So you've decided to hack on GHC, congratulations! We hope you have a
rewarding experience. This file will point you in the direction of
information to help you get started right away.

The GHC Developer's Wiki
========================

The home for GHC hackers is our GitLab instance, located here:

<https://gitlab.haskell.org/ghc/ghc>

From here, you can file bugs (or look them up), use the wiki, view the
`git` history, among other things. Of particular note is the building
page, which has the high level overview of the build process and how
to get the source:

<https://gitlab.haskell.org/ghc/ghc/wikis/building>

Contributing patches to GHC in a hurry
======================================

Make sure your system has the necessary tools to compile GHC. You can
find an overview of how to prepare your system for compiling GHC here:

<https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation>

After you have prepared your system, you can build GHC following the instructions described here:

<https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian>

Then start by making your commits however you want. When you're done, you can submit a merge request to [GitLab](https://gitlab.haskell.org/ghc/ghc/merge_requests) for code review.
Changes to the `base` library require a proposal to the [core libraries committee](https://github.com/haskell/core-libraries-committee/issues).
The GHC Wiki has a good summary for the [overall process](https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/fixing-bugs). One or several reviewers will review your PR, and when they are ok with your changes, they will assign the PR to [Marge Bot](https://gitlab.haskell.org/marge-bot) which will automatically rebase, batch and then merge your PR (assuming the build passes).


Useful links:
=============

An overview of things like using Git, the release process, filing bugs
and more can be located here:

<https://gitlab.haskell.org/ghc/ghc/wikis/contributing>

You can find our coding conventions for the compiler and RTS here:

<https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style>
<https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/conventions>

If you're going to contribute regularly, **learning how to use the
build system is important** and will save you lots of time. You should
read over this page carefully:

<https://gitlab.haskell.org/ghc/ghc/wikis/building/using>

A web based code explorer for the GHC source code with semantic analysis
and type information of the GHC sources is available at:

<https://haskell-code-explorer.mfix.io/>

Look for `GHC` in `Package-name`. For example, here is the link to
[GHC-8.6.5](https://haskell-code-explorer.mfix.io/package/ghc-8.6.5).

If you want to watch issues and code review activities, the following page is a good start:

<https://gitlab.haskell.org/ghc/ghc/activity>


How to communicate with us
==========================

GHC is a big project, so you'll surely need help. Luckily, we can
provide plenty through a variety of means!

## IRC

If you're an IRC user, be sure to drop by the official `#ghc` channel
on [Libera.Chat](https://libera.chat). Many (but not all) of the
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
 * [ghc-commits](http://www.haskell.org/mailman/listinfo/ghc-commits)
   for commit messages when someone pushes to the repository.

El fin
======

Happy Hacking!  -- The GHC Team
