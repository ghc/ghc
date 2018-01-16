# Changelog for [`Win32` package](http://hackage.haskell.org/package/Win32)

## 2.6.1.0 *November 2017*

* Add `terminateProcessById` (See #91)

## 2.6.0.0 *September 2017*

* Make cabal error out on compilation on non-Windows OSes. (See #80)
* Update cabal format to 1.10 and set language 
  default to Haskell2010. (See #81)
* Use `Maybe` in wrappers for functions with nullable pointer parameters (See #83)
* Improve cross compilation support. (See #87)

## 2.5.4.1 *April 2017*

* Fixed GetWindowLong on 32-bit Windows

## 2.5.3.0 *March 2017*

* Fix buffer overflow in `regSetValue`. (See #39)
* Added `getPixel`. (See #37)
* Drop dependency on `ntdll` because of incorrect import library on x86. (See #79)

## 2.5.2.0 *March 2017*

* Fix constant underflows with (-1) and unsigned numbers.
* Add `commandLineToArgv`

## 2.5.1.0 *Feb 2017*

* Add `withHandleToHANDLE` (originally found in the `ansi-terminal` library)
* fixed `PokeTZI` test

## 2.5.0.0 *Jan 2017*

* `failWith` (and the API calls that use it) now throw `IOError`s with proper
  `IOErrorType`s.
* Add function `findWindowByName`
* Fix a bug in the implementation of `poke` for `TIME_ZONE_INFORMATION` which
  would cause it to be marshalled incorrectly.
* Add `System.Win32.MinTTY` module for detecting the presence of MinTTY.
* Add `ULONG` type to `System.Win32.Types`.
* Add function `failIfNeg` to `System.Win32.Types`, which fails if a negative
  number is returned. This simulates the behavior of the `NT_SUCCESS` macro.
* Merged package Win32-extras (See #16)
* `Graphics.Win32.Misc.messageBox` safely imported now https://github.com/haskell/win32/pull/5
* Fixed various alignment calls that were incorrect. These would result in an incorrect alignment
  being returned on certain platforms. (See #66)

## 2.4.0.0 *Nov 2016*

* Add `windows_cconv.h` to the `install-includes` field of `Win32.cabal`,
  allowing packages that transitively depend on `Win32` to use the
  `WINDOWS_CCONV` CPP macro (which expands to `stdcall` or `ccall`
  appropriately depending on the system architecture)
* Added function `getLongPathName`
* Added function `getShortPathName`
* Added function `getUserName`
* Added file attribute `fILE_ATTRIBUTE_REPARSE_POINT`
* Added more [`File Access Rights` constants](https://msdn.microsoft.com/en-us/library/windows/desktop/gg258116%28v=vs.85%29.aspx)
* Added function `getCurrentProcessId`
* Added function `filepathRelativePathTo`
* Added function `pathRelativePathTo`
* Corrected 64 bit types (See #53)

## 2.3.1.1 *May 2016*

* Release for GHC 8.0.1
