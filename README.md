Tweag I/O GHC fork with linear types
============================

This is an in-development fork of GHC with support for linear types.


Building & Installing
=====================

There are two ways of running this locally:

1. By using our pre-build docker image, which integrates nicely with [stack](https://www.haskellstack.org/). (**recommended** since building GHC takes quite a long time)

2. By cloning this repository and building locally from source (extra instructions and a good chunk of time required)


Using stack with the Docker image 
============

1. Setup docker locally if you haven't got this already (see documentation for your distribution/Docker instructions)

2. Grab the docker image from Docker Hub with `docker pull tweag/linear-types`

3. In your new/existing stack project, edit the `stack.yaml` so to instruct stack to use a docker build of GHC:

```
[...]
resolver: ghc-8.2
compiler: ghc-8.2
system-ghc: true

docker:
  enable: true
  image: tweag/linear-types
[...]
```

The version numbers will depend on the specific version you want to use, and what the relevant image actually builds.

4. You should now be able to build your project using the GHC from our Docker image simply by running `stack build`!


Building from source
=======================

0. Verify that you have all the build dependencies:
  * autoconf
  * automake
  * ncurses
  * happy
  * alex
  * cabal-install
  * ghc >= 8.0.2 

1. Clone the GHC repository mirror from GitHub: 

  ```
  git clone https://github.com/ghc/ghc
  ```

2. Add our fork as a remote source:

  ```
  cd ghc
  git remote add tweag https://github.com/tweag/ghc.git
  git fetch tweag linear-types
  git checkout tweag/linear-types
  ```

3. Because of differing naming conventions in GHC and GitHub we need to give `git` some extra pointers:

  ```
  git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
git config --global url."ssh://git\@github.com/ghc/packages-".insteadOf ssh://git\@github.com/ghc/packages/
git config --global url."git\@github.com:/ghc/packages-".insteadOf      git\@github.com:/ghc/packages/
  ```

4. Update and initialise all the submodules:

  ```
  git submodule update --init
  ```

5. Then setup the project for building:

  ```
  ./boot
  ./configure
  ```

6. Run the build with `make`. This will take between a few and several hours to complete depending on your machine. You probably want to utilise more than one core, which can be done by passing `-j N` (for maximal speed, try N = #physical cores + 1.

7. Now you can use `./inplace/bin/ghc-stage2` as your GHC, or add `--interactive` to use `ghci`.


Issues & further instructions
=====================

If you run into trouble building, please consult the [GHC Building Guide](https://ghc.haskell.org/trac/ghc/wiki/Building) for possible answers.


Filing bugs
===========

Please note that this is under active development, but if you find something that definitiely looks off you are welcome to submit a bug at GitHub.
