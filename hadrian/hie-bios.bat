set TERM=dumb
set CABFLAGS=-v0
%CD%\hadrian\build-cabal.bat tool:%1 -q --build-root=_hie-bios --flavour=ghc-in-ghci > %HIE_BIOS_OUTPUT%
echo -ighc >> %HIE_BIOS_OUTPUT%
echo "ghc/Main.hs" >> %HIE_BIOS_OUTPUT%