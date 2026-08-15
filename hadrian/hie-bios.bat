#!/usr/bin/env bash

:;# When run, this program will output a list of arguments which are necessary to
:;# load the GHC library component into GHCi. The program is used by `ghcide` in
:;# order to automatically set up the correct GHC API session for a project.

:;# This is a POSIX shell/Windows batch polyglot script, which allows hie-bios
:;# to use a single hie.yaml file for both platforms.
:;#
:;#  1. The file extension is '.bat', because Windows requires this.
:;#     On POSIX, the extension doesn't matter, because the #! is what makes
:;#     the script runnable. On Windows, this shebang makes cmd.exe complain on
:;#     stderr, but this doesn't break anything; we add 'Please ignore the #! error'
:;#     to stderr as a courtesy.
:;#  2. On Windows, cmd.exe interprets lines starting with ':' as (goto) labels,
:;#     essentially ignoring them (for our purposes).
:;#     On POSIX, ':' is the no-op command, which we terminate with ';'.
:;#     As '#' is used for shell comment syntax, ':;#' behaves like a comment
:;#     marker in the polyglot language.
:;#  3. The line endings are LF-only, to make this POSIX-compliant.

:;# The POSIX part. Ignored on Windows as it starts with ':'.
:; TERM=dumb CABFLAGS=-v0 TOOL_OUTPUT=$HIE_BIOS_OUTPUT exec "$PWD/hadrian/build-cabal" tool:"$1" -q --build-root=.hie-bios --flavour=ghc-in-ghci -j

:;# The Windows part. Ignored on POSIX, because the above command execs.
@echo off
>&2 echo Please ignore the above error that '#!' is not recognised.
set TERM=dumb
set CABFLAGS=-v0
set TOOL_OUTPUT=%HIE_BIOS_OUTPUT%
%CD%\hadrian\build-cabal.bat tool:%1 -q --build-root=.hie-bios --flavour=ghc-in-ghci -j
