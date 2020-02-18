# Building GHC on Windows

Here is how you can build GHC, from source, on Windows with minimal requirements.
We only assume that `git` and `stack` are installed (see
[prerequisites](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/windows.md#prerequisites)).

```
# Get GHC sources; git core.autocrlf should be set to false (see Prerequisites section)
git clone --recursive git@gitlab.haskell.org:ghc/ghc.git
cd ghc

# Download and install the bootstrapping GHC and MSYS2
cd hadrian
stack setup

# Install utilities required during the GHC build process
stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm

# Build Hadrian and dependencies (including GHC dependencies Alex and Happy)
stack build

# Build GHC
# Note that the --configure flag is required only for the first build
stack exec hadrian -- --directory ".." -j --flavour=quickest --configure

# Test GHC
cd ..
_build\stage1\bin\ghc -e 1+2
```

The entire process should take about 20 minutes. Note, this will build GHC
without optimisations. If you need an optimised GHC, drop the `--flavour=quickest`
flag from the build command line (this will slow down the build to about an hour).

These are currently not the
[official GHC building instructions](https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/windows),
but are much simpler and may also be more robust.

The `stack build` and `stack exec hadrian` commands can be replaced by an
invocation of Hadrian's Stack-based build script:
`build-stack.bat -j --flavour=quickest`. Use this script if you plan to work on
Hadrian and/or rebuild GHC often.

## Prerequisites

The above works on a clean machine with `git` and `stack` installed (tested with
default installation settings), which you can get from
https://git-scm.com/download/win and
https://www.stackage.org/stack/windows-x86_64-installer.

Note that `git` should be configured to check out Unix-style line endings. The
default behaviour of `git` on Windows is to check out Windows-style line endings
which can cause issues during the build. This can be changed using the following
command:

    git config --global core.autocrlf false

If you would like to restore the default behaviour later run:

    git config --global core.autocrlf true

## Testing

These instructions have been tested on a clean Windows 10 machine using the
[free VirtualBox image](https://dev.windows.com/en-us/microsoft-edge/tools/vms/windows/).

**TODO:** It would be useful to test the instructions routinely on CI.

## Notes

Beware of the [current limitations of Hadrian](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md#current-limitations).
