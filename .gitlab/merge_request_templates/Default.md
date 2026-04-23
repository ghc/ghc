
<!--
Thank you for your contribution to GHC!

Please read the checklist below to make sure your contribution fulfills these
expectations.

If you have any questions don't hesitate to open your merge request and inquire
in a comment. If your patch isn't quite done yet please do add prefix your MR
title with Draft:

To make your contribution experience as smooth as possible, also check out
https://gitlab.haskell.org/ghc/ghc/-/wikis/Contributing-a-Patch
--> 

## Changes contained in this patch
<!-- Where is the key part of this patch? That is, what should reviewers look at first? -->


## MR Checklist
<!-- Please take a few moments to address the following points: -->

- [ ] This MR solves the problem described in the following issue: <!-- issue number here (please open a new issue if there isn't one) -->
- [ ] A changelog entry was added in `changelog.d/` for user-facing changes (see [changelog guide][changelog]).
      If this MR does not need a changelog entry, the ~"no-changelog" label was applied.
- [ ] This MR does not make any significant changes to `base`, or it has an accompanying [CLC proposal](https://github.com/haskell/core-libraries-committee#base-package).
- [ ] If this MR has the potential to break user programs, the ~"user-facing" label was applied to 
      test against head.hackage.
- [ ] All commits are either individually buildable or squashed.
- [ ] Commit messages describe *what they do*, referring to tickets using `#NNNNN` syntax.
- [ ] Source comments describing the change were added. For larger changes [notes][notes] and 
      cross-references from the relevant places were added (as applicable).
- [ ] [Testcases to the testsuite][adding test] were added (as applicable).
- [ ] The users guide was updated (as applicable).

<!--
By default a minimal validation pipeline is run on each merge request, the ~full-ci
label can be applied to perform additional validation checks if your MR affects a more
unusual configuration.

Once your change is ready please remove the `Draft:` tag and wait for review. If
no one has offered a review in a few days then please leave a comment mentioning
@triagers and apply the ~"Blocked on Review" label.
-->

[notes]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style#comments-in-the-source-code
[adding test]: https://gitlab.haskell.org/ghc/ghc/wikis/building/running-tests/adding
[changelog]: https://gitlab.haskell.org/ghc/ghc/-/wikis/contributing/changelog
