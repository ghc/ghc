Thank you for your contribution to GHC!

**Please read the checklist below to make sure your contribution fulfills these
expectations. Also please answer the following question in your MR description:**

**Where is the key part of this patch? That is, what should reviewers look at first?**

Please take a few moments to verify that your commits fulfill the following:

 * [ ] are either individually buildable or squashed
 * [ ] have commit messages which describe *what they do*
   (referring to [Notes][notes] and tickets using `#NNNN` syntax when
   appropriate)
 * [ ] have added source comments describing your change. For larger changes you
   likely should add a [Note][notes] and cross-reference it from the relevant
   places.
 * [ ] add a [testcase to the testsuite](https://gitlab.haskell.org/ghc/ghc/wikis/building/running-tests/adding).
 * [ ] if your MR affects library interfaces (e.g. changes `base`) or affects whether GHC will accept user-written code, please add
   the ~"user facing" label.
 * [ ] updates the users guide if applicable
 * [ ] mentions new features in the release notes for the next release

If you have any questions don't hesitate to open your merge request and inquire
in a comment. If your patch isn't quite done yet please do add prefix your MR
title with `WIP:`.

Once your change is ready please remove the `WIP:` tag and wait for review. If 
no one has offerred review in a few days then please leave a comment mentioning
@triagers.

[notes]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style#comments-in-the-source-code
