# Getting Started

To get started with Shake, you need to:

* Install the [Haskell Stack](https://haskellstack.org/).
* Type `stack install shake`
* Run `stack exec -- shake --demo`, which should produce something similar to the trace below.
* To repeat the build after the initial demo, run either `stack exec ./build.sh` (on Linux) or `stack exec build.bat` (on Windows) in the demo directory.

<pre>
<!-- nosyntax --><b>% Welcome to the Shake v1.0 demo mode!
% Detecting machine configuration... done
&#32;
% The Shake demo uses an empty directory, OK to use:
%     C:\Users\Neil\shake-demo
% [Y/N] (then ENTER): y
% Copying files... done
&#32;
% [1/5] Building an example project with Shake.
% Press ENTER to continue:
% RUNNING: cd C:\Users\Neil\shake-demo
% RUNNING: build</b>
[1 of 1] Compiling Main             ( Shakefile.hs, _shake\Main.o )
Linking _shake/build.exe ...
# gcc (for _build/main.o)
# gcc (for _build/constants.o)
# gcc (for _build/run.exe)
Build completed in 0:04m
&#32;
<b>% [2/5] Running the produced example.
% Press ENTER to continue:
% RUNNING: _build\run.exe</b>
Hello Shake Users!
&#32;
<b>% [3/5] Rebuilding an example project with Shake (nothing should change).
% Press ENTER to continue:
% RUNNING: build</b>
Build completed in 0:01m
&#32;
<b>% [4/5] Cleaning the build.
% Press ENTER to continue:
% RUNNING: build clean</b>
Cleaning files in _build
Build completed in 0:01m
&#32;
<b>% [5/5] Rebuilding with 2 threads and profiling.
% Press ENTER to continue:
% RUNNING: build -j2 --report --report=-</b>
# gcc (for _build/constants.o)
# gcc (for _build/main.o)
# gcc (for _build/run.exe)
Writing report to report.html
Writing report to -
* This database has tracked 1 runs.
* There are 7 rules (7 rebuilt in the last run).
* Building required 3 traced commands (3 in the last run).
* The total (unparallelised) time is 4.15s of which 4.14s is traced commands.
* The longest rule takes 1.55s (_build/main.o), and the longest traced command
takes 1.54s (gcc).
* Last run gave an average parallelism of 1.56 times over 2.66s.
Build completed in 0:03m
&#32;
<b>% See the profiling summary above, or look at the HTML profile report in
%     C:\Users\Neil\shake-demo\report.html
&#32;
% Demo complete - all the examples can be run from:
%     C:\Users\Neil\shake-demo
% For more info see <a href="https://www.shakebuild.com/">https://shakebuild.com</a>
&#32;
% PS. Shake can also execute Ninja build files
% For more info see <a href="https://www.shakebuild.com/ninja">https://shakebuild.com/ninja</a></b>
</pre>
