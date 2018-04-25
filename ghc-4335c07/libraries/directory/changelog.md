Changelog for the [`directory`][1] package
==========================================

## 1.3.1.5 (October 2017)

  * Rename the internal header `windows.h` to avoid GHC#14312.
    ([#77](https://github.com/haskell/directory/issues/77))

## 1.3.1.4 (September 2017)

  * Fix `Win32` version 2.6 compatibility.
    ([#75](https://github.com/haskell/directory/pull/75))

## 1.3.1.3 (September 2017)

  * Relax `Win32` version bounds to support 2.6.

## 1.3.1.2 (September 2017)

  * Relax `base` version bounds to support 4.11.
    ([#74](https://github.com/haskell/directory/pull/74))

## 1.3.1.1 (March 2017)

  * Fix a bug where `createFileLink` and `createDirectoryLink` failed to
    handle `..` in absolute paths.

  * Improve support (partially) for paths longer than 260 characters on
    Windows.  To achieve this, many functions will now automatically prepend
    `\\?\` before calling the Windows API.  As a side effect, the `\\?\`
    prefix may show up in the error messages of the affected functions.

  * `makeAbsolute` can now handle drive-relative paths on Windows such as
    `C:foobar`

## 1.3.1.0 (March 2017)

  * `findFile` (and similar functions): when an absolute path is given, the
    list of search directories is now completely ignored.  Previously, if the
    list was empty, `findFile` would always fail.
    ([#72](https://github.com/haskell/directory/issues/72))

  * For symbolic links on Windows, the following functions had previously
    interpreted paths as referring to the links themselves rather than their
    targets.  This was inconsistent with other platforms and has been fixed.
      * `getFileSize`
      * `doesPathExist`
      * `doesDirectoryExist`
      * `doesFileExist`

  * Fix incorrect location info in errors from `pathIsSymbolicLink`.

  * Add functions for symbolic link manipulation:
      * `createFileLink`
      * `createDirectoryLink`
      * `removeDirectoryLink`
      * `getSymbolicLinkTarget`

  * `canonicalizePath` can now resolve broken symbolic links too.
    ([#64](https://github.com/haskell/directory/issues/64))

## 1.3.0.2 (February 2017)

  * [optimization] Increase internal buffer size of `copyFile`
    ([#69](https://github.com/haskell/directory/pull/69))

  * Relax `time` version bounds to support 1.8.

## 1.3.0.1 (January 2017)

  * Relax `Win32` version bounds to support 2.5.
    ([#67](https://github.com/haskell/directory/pull/67))

## 1.3.0.0 (December 2016)

  * **[breaking]** Drop trailing slashes in `canonicalizePath`
    ([#63](https://github.com/haskell/directory/issues/63))

  * **[deprecation]** Rename `isSymbolicLink` to `pathIsSymbolicLink`.  The
    old name will remain available but may be removed in the next major
    release.
    ([#52](https://github.com/haskell/directory/issues/52))

  * Changed `canonicalizePath` to dereference symbolic links even if it points
    to a file and is not the last path segment

  * On Windows, `canonicalizePath` now canonicalizes the letter case too

  * On Windows, `canonicalizePath` now also dereferences symbolic links

  * When exceptions are thrown, the error location will now contain additional
    information about the internal function(s) used.

## 1.2.7.1 (November 2016)

  * Don't abort `removePathForcibly` if files or directories go missing.
    In addition, keep going even if an exception occurs.
    ([#60](https://github.com/haskell/directory/issues/60))

## 1.2.7.0 (August 2016)

  * Remove deprecated C bits.  This means `HsDirectory.h` and its functions
    are no longer available.
    ([#50](https://github.com/haskell/directory/issues/50))

  * Add `doesPathExist` and `getFileSize`
    ([#57](https://github.com/haskell/directory/issues/57))

  * Add `renamePath`
    ([#58](https://github.com/haskell/directory/issues/58))

  * Add `removePathForcibly`
    ([#59](https://github.com/haskell/directory/issues/59))

## 1.2.6.3 (May 2016)

  * Add missing import of `(<*>)` on Windows for `base` earlier than 4.8.0.0
    ([#53](https://github.com/haskell/directory/issues/53))

## 1.2.6.2 (April 2016)

  * Bundled with GHC 8.0.1

  * Fix typo in file time functions when `utimensat` is not available and
    version of `unix` package is lower than 2.7.0.0

## 1.2.6.1 (April 2016)

  * Fix mistake in file time functions when `utimensat` is not available
    ([#47](https://github.com/haskell/directory/pull/47))

## 1.2.6.0 (April 2016)

  * Make `findExecutable`, `findExecutables`, `findExecutablesInDirectories`,
    `findFile`, and `findFilesWith` lazier
    ([#43](https://github.com/haskell/directory/issues/43))

  * Add `findFileWith`

  * Add `copyFileWithMetadata`, which copies additional metadata
    ([#40](https://github.com/haskell/directory/issues/40))

  * Improve error message of `removeDirectoryRecursive` when used on a
    directory symbolic link on Windows.

  * Add `isSymbolicLink`

  * Drop support for Hugs.

## 1.2.5.1 (February 2016)

  * Improve error message of `getCurrentDirectory` when the current working
    directory no longer exists
    ([#39](https://github.com/haskell/directory/issues/39))

  * Fix the behavior of trailing path separators in `canonicalizePath` as well
    as `makeAbsolute` when applied to the current directory; they should now
    match the behavior of `canonicalizePath` prior to 1.2.3.0 (when the bug
    was introduced)
    ([#42](https://github.com/haskell/directory/issues/42))

  * Set the location in IO errors from `makeAbsolute`.

## 1.2.5.0 (December 2015)

  * Add `listDirectory`, which is similar to `getDirectoryContents`
    but omits `.` and `..`
    ([#36](https://github.com/haskell/directory/pull/36))

  * Remove support for `--with-cc=` in `configure`; use the `CC=` flag instead
    ([ghc:D1608](https://phabricator.haskell.org/D1608))

## 1.2.4.0 (September 2015)

  * Work around lack of `#const_str` when cross-compiling
    ([haskell-cafe](F7D))

  * Add `findExecutablesInDirectories`
    ([#33](https://github.com/haskell/directory/pull/33))

  * Add `exeExtension`

[F7D]: https://mail.haskell.org/pipermail/haskell-cafe/2015-August/120892.html

## 1.2.3.1 (August 2015)

  * Restore support for Safe Haskell with base < 4.8
    ([#30](https://github.com/haskell/directory/issues/30))

## 1.2.3.0 (July 2015)

  * Add support for XDG Base Directory Specification
    ([#6](https://github.com/haskell/directory/issues/6))

  * Implement `setModificationTime` counterpart to `getModificationTime`
    ([#13](https://github.com/haskell/directory/issues/13))

  * Implement `getAccessTime` and `setAccessTime`

  * Set the filename in IO errors from the file time functions

  * Fix `canonicalizePath` so that it always returns a reasonable result even
    if the path is inaccessible and will not throw exceptions unless the
    current directory cannot be obtained
    ([#23](https://github.com/haskell/directory/issues/23))

  * Corrected the trailing slash behavior of `makeAbsolute`
    so that `makeAbsolute "" == makeAbsolute "."`

  * Deprecate use of `HsDirectory.h` and `HsDirectoryConfig.h`

  * Implement `withCurrentDirectory`

## 1.2.2.1 (Apr 2015)

  * Fix dependency problem on NixOS when building with tests
    ([#24](https://github.com/haskell/directory/issues/24))

## 1.2.2.0 (Mar 2015)

  * Bundled with GHC 7.10.1

  * Make `getModificationTime` support sub-second resolution on Windows

  * Fix silent failure in `createDirectoryIfMissing`

  * Replace `throw` by better defined `throwIO`s

  * Avoid stack overflow in `getDirectoryContents`
    ([#17](https://github.com/haskell/directory/pull/17))

  * Expose `findExecutables`
    ([#14](https://github.com/haskell/directory/issues/14))

  * `removeDirectoryRecursive` no longer follows symlinks under any
    circumstances
    ([#15](https://github.com/haskell/directory/issues/15))

  * Allow trailing path separators in `getPermissions` on Windows
    ([#9](https://github.com/haskell/directory/issues/9))

  * `renameFile` now always throws the correct error type
    (`InappropriateType`) when the destination is a directory, as long as the
    filesystem is not being modified concurrently
    ([#8](https://github.com/haskell/directory/pull/8))

  * Add `makeAbsolute`, which should be preferred over `canonicalizePath`
    unless one requires symbolic links to be resolved

## 1.2.1.0 (Mar 2014)

  * Bundled with GHC 7.8.1

  * Add support for sub-second precision in `getModificationTime` when
    linked against `unix>=2.6.0.0`

  * Fix `createDirectoryIfMissing _ "."` in `C:\` on Windows

  * Remove support for NHC98 compiler

  * Update package to `cabal-version >= 1.10` format

  * Enhance Haddock documentation for `doesDirectoryExist` and
    `canonicalizePath`

  * Fix `findExecutable` to check that file permissions indicate executable

  * New convenience functions `findFiles` and `findFilesWith`

[1]: https://hackage.haskell.org/package/directory
