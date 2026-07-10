# Cloud Shake

The idea of Cloud Shake is to have a cache of previously computed rules which can be used in a build, avoiding rebuilding the rules. Currently, Cloud Shake is very much in development, with open questions and prototype implementations, but it does exist.

## The Model

Shake adds a feature called "History". An entry in the history describes a key, it's dependencies, and it's outputs. The plan was to provide two ways to satisfy the history:

* Via a shared file area, which records history entries in files.
* Via a web server, with a custom protocol.

That was the plan, but then Simon Peyton Jones suggested that actually the shared drive approach is sufficient. You can map a drive over the internet, use NFS, rsync, you can control permissions, you can explore it with the file system. The only purpose of a web server was to provide a more efficient communication pipe, but is it actually more efficient? And it is worth the cost? These are open questions.

At the moment the shared file area approach is implemented, and works. It hasn't been tested on a remote shared drive, and isn't particularly well designed to support that, although it might work, and could certainly be adapted. The web server approach hasn't been finished, so doesn't work at all. We're left with the question - push forward with the shared file area and delete the web server, or push forward with the web server. Proof of someone using the file area successfully would be enough to kill the web server.

## Running Cloud Shake

To run Cloud Shake pass `--share=LOCATION` with where you want the cache to go. That's it. However, it probably won't work unless your build system obeys a number of invariants.

## Modifying your build system

To make your build system work with a shared cache, it is important that any files which are produced by a rule, but are not official outputs of the rule, are still recorded - otherwise they won't end up in the cache. The easiest way to test if your build system works already is to:

* Build using `--share`.
* Delete all build outputs, but not the shared directory.
* Build using `--share`.

In likelihood, the build will fail because it can't find a file. Figure out which rule produced that file and either:

* Adjust to using `&%>` which declares that a rule produces multiple files.
* Add a call to `produces` to declare that the file was produced.

If you can't figure out file produces the rule, use `-j1 --lint-watch=FILE` which will raise an error as soon as a rule terminates and the file has changed (including being creased).

After fixing the recording of items in the cache, you can clean and try again. If that process is slow a combination of `--share-list` (see what's in the cache) and `--share-remove=RULE` (delete any keys which match that substring) might help iterate more quickly.

If a rule shouldn't be stored in the cache (e.g. because it modifies files that already exist) use `historyDisable`.

The [GHC](https://www.haskell.org/ghc/) build system [Hadrian](https://ghc.haskell.org/trac/ghc/wiki/Building/Hadrian/QuickStart) is now able to build with `--share` following [this patch](https://gitlab.haskell.org/ghc/ghc/merge_requests/317) (which also does unrelated refactorings). However, the benefits weren't as big as they were hoped because of the dependency graph of GHC - see [this blog post](https://www.well-typed.com/blog/2019/08/exploring-cloud-builds-in-hadrian/) for details.

## Symlinks and Read only output files

When run in `--share` mode, by default Shake copies the files to and from the shared location. However, that causes a lot of expensive IO - you may instead wish to use `--share-symlink` which uses hard links to link the files together. Unfortunately, when the files are linked together, changing one file changes both - which can corrupt the cache. To avoid that severe problem, when using `--share-symlink` Shake marks both files as read-only. As a consequence, if a rule tries to modify the output file it may fail - it may need to _delete_ the output file and recreate it.

One day hopefully file systems will expose real copy-on-write files, until then, every option involves some kind of trade off.
