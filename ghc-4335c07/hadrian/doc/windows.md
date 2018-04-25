# Building GHC on Windows

[![Windows status](https://img.shields.io/appveyor/ci/snowleopard/hadrian/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/hadrian)

Here is how you can build GHC, from source, on Windows. We assume that `git` and `stack` are installed
(see [prerequisites](https://github.com/snowleopard/hadrian/blob/master/doc/windows.md#prerequisites)).

```sh
# Get GHC and Hadrian sources; git core.autocrlf should be set to false (see Prerequisites section)
git clone --recursive git://git.haskell.org/ghc.git
cd ghc
git clone git://github.com/snowleopard/hadrian

# Download and install the bootstrapping GHC and MSYS2
cd hadrian
stack setup

# Install utilities required during the GHC build process
stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm

# Build Hadrian and dependencies (including GHC dependencies Alex and Happy)
stack build

# Build GHC
stack exec hadrian -- --directory ".." -j --flavour=quickest

# Test GHC
cd ..
inplace\bin\ghc-stage2 -e 1+2
```

The entire process should take about 20 minutes. Note, this will build GHC without
optimisations. If you need an optimised GHC, drop the `--flavour=quickest` flag from
the build command line (this will slow down the build to about an hour).

These are currently not the
[official GHC building instructions](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows),
but are much simpler and may also be more robust.

The `stack build` and `stack exec hadrian` commands can be replaced by an invocation
of Hadrian's Stack-based build script: `build.stack.bat -j --flavour=quickest`. Use this
script if you plan to work on Hadrian and/or rebuild GHC often.

## Prerequisites

The above works on a clean machine with `git` and `stack` installed (tested with default
installation settings), which you can get from https://git-scm.com/download/win and
https://www.stackage.org/stack/windows-x86_64-installer.

Note that `git` should be configured to check out Unix-style line endings. The default behaviour
of `git` on Windows is to check out Windows-style line endings which can cause issues during the
build. This can be changed using the following command:

    git config --global core.autocrlf false

If you would like to restore the default behaviour later run:

    git config --global core.autocrlf true

## Testing

These instructions have been tested on a clean Windows 10 machine using the
[free VirtualBox image](https://dev.windows.com/en-us/microsoft-edge/tools/vms/windows/),
and are also routinely tested on
[Hadrian's AppVeyor CI instance](https://ci.appveyor.com/project/snowleopard/hadrian/history).

## Notes

Beware of the [current limitations of Hadrian](https://github.com/snowleopard/hadrian#current-limitations).
