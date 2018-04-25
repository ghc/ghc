Name: runghc
Version: @ProjectVersion@
Copyright: XXX
License: BSD3
-- XXX License-File: LICENSE
Author: XXX
Maintainer: XXX
Synopsis: A wrapper around GHC allowing convenient execution of scripts
Description:
    @runghc@ is a small wrapper program around GHC which allows the compiler
    to be used as a UNIX-style script interpreter. For instance,
    .
    @
    $ cat > Hi.hs
    \#!/usr/bin/env runghc
    main = putStrLn "hello!"
    $ chmod u+x Hi.hs
    $ ./Hi.hs
    hello!
    @
Category: Development
build-type: Simple
cabal-version: >=1.10

Executable runghc
    Default-Language: Haskell2010
    Main-Is: Main.hs

    Build-Depends: base       >= 3   && < 5,
                   directory  >= 1   && < 1.4,
                   process    >= 1   && < 1.7,
                   filepath

    if !os(windows)
      build-depends: unix