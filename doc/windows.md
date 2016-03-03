# Compiling on Windows

Here are a list of instructions to compile GHC, from source, on Windows. I tested these instructions on a clean machine using the [free Windows 10 VirtualBox image](https://dev.windows.com/en-us/microsoft-edge/tools/vms/windows/) (I bumped the VM CPUs to 4, and RAM to 4096Mb). These instructions are not currently the official GHC building instructions, but might be simpler and more robust than those.

The first step is to [install Stack](https://www.stackage.org/stack/windows-x86_64-installer) (I just accepted all the defaults), then open a command prompt and run:

	stack setup
	stack install happy alex
	stack exec -- pacman -S gcc binutils git automake-wrapper tar make patch autoconf --noconfirm
	stack exec -- git clone --recursive git://git.haskell.org/ghc.git
	cd ghc
	stack exec -- git clone git://github.com/snowleopard/shaking-up-ghc shake-build
	stack build --stack-yaml=shake-build/stack.yaml --only-dependencies
	stack exec -- perl boot
	stack exec -- bash configure --enable-tarballs-autodownload
	stack exec --stack-yaml=shake-build/stack.yaml -- shake-build/build.bat -j

The entire process should take about an hour.

#### Future ideas

Here are some alternatives that have been considered, but not yet tested. Use the instructions above.

* Use `shake-build/build.bat --setup` to replace `boot` and `configure`.
* The `pacman` install of `gcc` is probably not necessary, but it does pull in a lot of tools, some of which probably are necessary. Ideally thin the list down.
* Can Happy/Alex be installed by adding them as tool dependencies to the Stack file?
