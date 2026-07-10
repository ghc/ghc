# Lint features

Shake features a built in "lint" features to check the build system is well formed. To run use `build --lint`. You are likely to catch more lint violations if you first `build clean`. The lint features are listed in this document. There is a performance penalty for building with `--lint`, but it is typically small.

## Changing current directory

Enabled with `--lint`. Detects changing the current directory, typically with `setCurrentDirectory`. You should never change the current directory within the build system as multiple rules running at the same time share the current directory. You can still run `cmd_` calls in different directories using the `Cwd` argument.

## Changing outputs after building

Enabled with `--lint`. Detects if any files have changed after Shake has built them. There are a couple of causes for seeing this error:

* If there is a rule producing `foo.o`, but another rule also modifies `foo.o`.
* If you are on a file system where files change modification time after a while. A standard example would be an NFS drive where the underlying network file system stores modification times to second-level resolution, but the in-memory cache keeps them precisely.
* If you modify the build sources while running a build.

A consequence of this lint triggering would be that a subsequent build would do additional work, as it spots modifications.

## Needed

Enabled with `--lint`. Calling `needed` is normally equivalent to `need`, but with `--lint` it also changes that the file does not change as a result of `need` - expressing that you have _already_ used the contents of the file.

## Manual tracking functions

Enabled with `--lint`. The tracking function `trackRead` and `trackWrite` can be called to declare reads/writes, and they can assert various invariants about what files can be written where. Examples include:

* You can only read a file that is either your dependency, or a transitive dependency.
* You can only write a file that you are declared to write, or that no one calls `need` on.

Additionally, you can _ignore_ certain missing rules with `--lint-ignore=PATTERN`. In general all files passed to `trackRead` or `trackWrite` are expected to be relative to the current directory, so `--lint-ignore` patterns should match those relative paths.

## Automatic tracking functions

Using [`fsatrace`](https://github.com/jacereda/fsatrace) you can augment command line programs (called with `cmd` or `command`) to automatically track which files they read and write, which turn into `trackRead` and `trackWrite` calls. To enable this feature pass `--lint-fsatrace=DIR` passing the directories you want to lint. Passing `--lint-fsatrace` is equivalent to `--lint-fsatrace=.` - namely only lint the current directory.

This feature requires `fsatrace` to be on the `$PATH`, as documented on [the homepage](https://github.com/jacereda/fsatrace). If you are using Windows, you can download a [binary release here](https://github.com/ndmitchell/shake/releases/tag/fsatrace-1).
