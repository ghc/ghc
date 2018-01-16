These benchmarks where take from [The Computer Language Benchmarks
Game](http://benchmarksgame.alioth.debian.org/) (formerly The Great
Computer Language Shootout). They test highely micro-optimized code.

To run the benchmarks under the official settings, pass `mode=slow` to
make. You might also want to pass `EXTRA_HC_OPTS="-fllvm"`.

Notes:

 * The mandelbrot benchmark wasn't included as its binary output
   (which would have to go in a .stdout file) is too large to check
   into the source tree.

 * The regex-dna benchmark wasn't included as it depend on regex-pcre,
   which requires C libraries that aren't installed by default on many
   platforms.
