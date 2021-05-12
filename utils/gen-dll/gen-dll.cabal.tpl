-- WARNING: gen-dll.cabal is automatically generated from gen-dll.cabal.in by
-- ./configure.  Make sure you are editing gen-dll.cabal.in, not gen-dll.cabal.

Name: gen-dll
Version: 0.1
Copyright: XXX
License: BSD3
-- XXX License-File: LICENSE
Maintainer: ghc-devs@haskell.org
author: Tamar Christina
Synopsis: Generate GHC core boot library dlls
Description:
    This package is responsible for building DLLs that are delay loaded and
    create optimized import libraries that can be used to delay load DLLs.
    Particularly the RTS. This allows us to delay the loading of the DLL while
    still having const data imports work. It also allows us to work around
    certain dlltool limitations and the very slow BFD import lib implementation.

build-type: Simple
cabal-version: >=1.10

Executable gen-dll
    Default-Language: Haskell2010
    Main-Is: Main.hs
    Build-Depends: base       >= 3   && < 5  ,
                   pretty     >= 1.1 && < 1.2,
                   process    >= 1.2 && < 1.9,
                   filepath   >= 1.3 && < 1.5,
                   directory  >= 1.1 && < 1.4,
                   containers >= 0.5 && < 0.7
    Extra-Libraries: Shell32
    ghc-options: -UGEN_SXS
                 -DHAS_GENLIB=@HAVE_GENLIB@
                 -DNM_TOOL_BIN="\"@NmCmd@\""
                 -DLIB_TOOL_BIN="\"@LibtoolCmd@\""
                 -DGENLIB_TOOL_BIN="\"@GenlibCmd@\""
                 -DAR_TOOL_BIN="\"@ArCmd@\""
