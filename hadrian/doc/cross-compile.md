## Build a cross-compiling GHC

In this example, our host machine is "Ubuntu 16.04.2 LTS, Linux ubuntu 4.4.0-79-generic 86_64".

We need to download necessary tools, including:

- [LLVM-4.0 source](http://releases.llvm.org/4.0.0/llvm-4.0.0.src.tar.xz), you need to build it yourself. Remember to choose release channel and use gold linker (`cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=gold ..`)
- `sudo apt-get install gcc-arm-linux-gnueabihf` to install the GCC cross-compiler
- Download and install [Haskell Platform 8.0.2](https://haskell.org/platform/download/8.0.2/haskell-platform-8.0.2-unknown-posix--full-x86_64.tar.gz). Install it according to [instructions here](https://www.haskell.org/platform/linux.html#linux-generic)

After all the dependencies are in place:

- `git clone https://github.com/ghc/ghc`
- `cd ghc`
- `git clone https://github.com/snowleopard/hadrian`
- `git submodule update --init`
- `./configure --target=arm-linux-gnueabihf`
- `cd hadrian`
- Build the compiler by e.g. `./build.sh --flavour=quickest --integer-simple -V -j`

After that, you should have built `inplace/bin/ghc-stage1` cross compiler. We will go to the next section to validate this.

**NOTE**: Use of `-c` to configure the target is currently not supported. Please manually run `./configure` like above.

## Test run

Write a simple hello world haskell program:

```haskell
module Main where
main = putStrLn "Hello, world!"
```
Compile it with cross-compiling GHC: `<ghc-folder>/inplace/bin/ghc-stage1 -static Main`. Note that we created a static version of it which packs together all depending libraries.

- Install QEMU: `sudo apt-get install qemu-system-arm`
- Download `vmlinuz` (kernel) and `initrd.gz` (initial ramdisk), e.g. from [this mirror](https://mirrors.tuna.tsinghua.edu.cn/ubuntu-ports/dists/xenial/main/installer-armhf/current/images/generic-lpae/cdrom/).
- Add the ARM Linux executable `Main` to the initial ramdisk so we can load it directly into memory. No need for real installation
  + `gunzip initrd.gz` to get `initrd`
  + `mkdir tmp2; cd tmp2; sudo cpio -id < ../initrd` to get a file system
  + `cp /PATH/TO/Main usr/bin`
  + `find . | cpio --create --format='newc' > /tmp/newinitrd` to pack back the `initrd`
  + `gzip /tmp/newinitrd`
  + Move `newinitrd` to where `vmlinuz` is, rename it to `newinitrd.img`
  + Run the following configured QEMU:

```bash
#!/bin/sh
qemu-system-arm \
    -kernel vmlinuz \
    -initrd newinitrd.img \
    -append "root=/dev/vda2 rootfstype=ext4" \
    -no-reboot \
    -nographic \
    -m 1024 \
    -M virt
```

This will lead you to a installer interface. But we don't need to do that, so we can save ourself from the hassle of setting up networks etc. We just keep `Go Back`, until see a line `Execute a shell`, and select it. Now you get a shell, go find `/usr/bin/Main` and run it!
