Thank you for your contribution to GHC!

**Please read the checklist below to make sure your contribution fulfills these
expectations. Also please answer the following question in your MR description:**

**Where is the key part of this patch? That is, what should reviewers look at first?**

Please take a few moments to address the following points:

 * [ ] if your MR touches `base` (or touches parts of `ghc-internal` used
   or re-exported by `base`) more substantially than just amending comments
   or documentation, you likely need to raise a
   [CLC proposal](https://github.com/haskell/core-libraries-committee#base-package)
   before merging it.
 * [ ] if your MR may break existing programs (e.g. causes the
   compiler to reject programs), please describe the expected breakage and add
   the ~"user-facing" label. This will run ghc/head.hackage> to characterise
   the effect of your change on Hackage.
 * [ ] ensure that your commits are either individually buildable or squashed
 * [ ] ensure that your commit messages describe *what they do*
   (referring to tickets using `#NNNN` syntax when appropriate)
 * [ ] have added source comments describing your change. For larger changes you
   likely should add a [Note][notes] and cross-reference it from the relevant
   places.
 * [ ] add a [testcase to the testsuite][adding test].
 * [ ] updates the users guide if applicable
 * [ ] mentions new features in the release notes for the next release

If you have any questions don't hesitate to open your merge request and inquire
in a comment. If your patch isn't quite done yet please do add prefix your MR
title with `WIP:`.

By default a minimal validation pipeline is run on each merge request, the ~full-ci
label can be applied to perform additional validation checks if your MR affects a more
unusual configuration.

Once your change is ready please remove the `WIP:` tag and wait for review. If
no one has offered a review in a few days then please leave a comment mentioning
@triagers and apply the ~"Blocked on Review" label.

[notes]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style#comments-in-the-source-code
[adding test]: https://gitlab.haskell.org/ghc/ghc/wikis/building/running-tests/adding
