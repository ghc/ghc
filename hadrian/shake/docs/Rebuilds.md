# Controlling rebuilds

By default, Shake rebuilds everything that has changed. However, sometimes you want to rebuild things that haven't changed, or not rebuild things that have changed. To support this workflow Shake provides three modes:

* `RebuildNormal` is the default setting, rebuild a rule if its dependencies have changed.
* `RebuildNow` forces a rule to rebuild even if its dependencies haven't changed. If the rule changes, then that will in turn cause anything depending on that rule to rebuild too.
* `RebuildLater` causes a rule not to rebuild this run even if its dependencies have changed. Note that in future runs, if the `RebuildLater` is not set, the rule may rebuild.

To set these flags in the `ShakeOptions` structure there is a field `shakeRebuild` of type `[(Rebuild, FilePattern)]`.


By default all files are set to `RebuildNormal`. To set all `.o` files to rebuild regardless you can set `shakeRebuild` to `[(RebuildNow, "**/*.o")]`. Where a file patches multiple patterns, the last one is applied.

To set these flags on the command line use `--rebuild=PAT` (for `RebuildNow`), `--skip=PAT` (for `RebuildLater`) and `--no-rebuild` (for `RebuildNormal`). For example, to rebuild all `.o` files pass `--rebuild=**/*.o`. If you omit the pattern it defaults to `**`, which matches everything.

There is currently no `RebuildNever` flag to permanently mark a file as up-to-date. Such a flag could cause inconsistent builds, but more relevantly, it hasn't been implemented yet.

### Comparison to Make

The `make` tool has a number of features to force rebuilds or skip rebuilds, all fundamentally modelled on file modification times forming an order, which is quite a different model to Shake.

* `-B` / `--always-make` considers all targets out-of-date and rebuilds everything. The Shake equivalent is `--rebuild`.
* `-o FILE` / `--old-file=FILE` / `--assume-old=FILE` does not remake the file `FILE` even if it is older than its prerequisites. The Shake equivalent is `--skip=FILE`.
* `-t` / `--touch` touches files (marks them up to date without really changing them) instead of building them. The closest equivalent in Shake is `--skip`, but that only applies to this run. A hypothetical `RebuildNever` flag would more accurately model this flag.
* `-W FILE` / `--what-if=FILE` / `--new-file=FILE` / `--assume-new=FILE` pretends that the target file has just been modified. Shake doesn't really have an equivalent, as `--rebuild` applies to the rules to rebuild, whereas in Make this applies to the things that depend on it. In addition, Make often uses this flag in conjunction with dry-run, which Shake doesn't yet have.
