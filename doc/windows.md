# Building on Windows

Here are a list of instructions to build GHC, from source, on Windows. I tested these instructions on a clean machine using the [free Windows 10 VirtualBox image](https://dev.windows.com/en-us/microsoft-edge/tools/vms/windows/) (I bumped the VM CPUs to 4, and RAM to 4096Mb). These instructions are not currently the official GHC building instructions, but might be simpler and more robust than those.

The first step is to [install Stack](https://www.stackage.org/stack/windows-x86_64-installer) (I just accepted all the defaults), then open a command prompt and run:

	stack setup
	stack install happy alex
	stack exec -- pacman -S gcc binutils git automake-wrapper tar make patch autoconf --noconfirm
	stack exec -- git clone --recursive git://git.haskell.org/ghc.git
	cd ghc
	stack exec -- git clone git://github.com/snowleopard/hadrian
	stack build --stack-yaml=hadrian/stack.yaml --only-dependencies
	stack exec --stack-yaml=hadrian/stack.yaml -- hadrian/build.bat -j --flavour=quick

The entire process should take about 20 minutes. Note, this will build GHC without optimisations. If you need an optimised GHC, drop the `--flavour=quick` flag from the last command line (this will slow down the build to about an hour).

#### Future ideas

Here are some alternatives that have been considered, but not yet tested. Use the instructions above.

* The `pacman` install of `gcc` is probably not necessary, but it does pull in a lot of tools, some of which probably are necessary. Ideally thin the list down.
* Happy/Alex should be able to be installed by adding them as `build-tools` in the Cabal file.
