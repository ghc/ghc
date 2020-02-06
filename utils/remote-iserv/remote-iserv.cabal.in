-- WARNING: iserv-proxy.cabal is automatically generated from remote-iserv.cabal.in by
-- ../../configure.  Make sure you are editing remote-iserv.cabal.in, not
-- remote-iserv.cabal.

Name: remote-iserv
Version: @ProjectVersion@
Copyright: XXX
License: BSD3
-- XXX License-File: LICENSE
Author: Moritz Angermann <moritz.angermann@gmail.com>
Maintainer: Moritz Angermann <moritz.angermann@gmail.com>
Synopsis: iserv allows GHC to delegate Template Haskell computations
Description:
  This is a very simple remote runner for iserv, to be used together
  with iserv-proxy.  The foundamental idea is that this this wrapper
  starts running libiserv on a given port to which iserv-proxy will
  then connect.
Category: Development
build-type: Simple
cabal-version: >=1.10

Executable remote-iserv
   Default-Language: Haskell2010
   Main-Is: Cli.hs
   Hs-Source-Dirs: src
   Build-Depends: base       >= 4   && < 5,
                  libiserv   == @ProjectVersionMunged@
