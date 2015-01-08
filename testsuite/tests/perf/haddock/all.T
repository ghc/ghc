# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [residency]. See #9556.

test('haddock.base',
     [unless(in_tree_compiler(), skip)
     ,stats_num_field('bytes allocated',
          [(wordsize(64), 9014511528,  5)
            # 2012-08-14: 5920822352 (amd64/Linux)
            # 2012-09-20: 5829972376 (amd64/Linux)
            # 2012-10-08: 5902601224 (amd64/Linux)
            # 2013-01-17: 6064874536 (x86_64/Linux)
            # 2013-02-10: 6282746976 (x86_64/Linux)
            # 2013-09-17: 6634886456 (x86_64/Linux)
            # 2013-09-18: 6294339840 (x86_64/Linux)
            # 2013-11-21: 6756213256 (x86_64/Linux)
            # 2014-01-12: 7128342344 (x86_64/Linux)
            # 2014-06-12: 7498123680 (x86_64/Linux)
            # 2014-08-05: 7992757384 (x86_64/Linux - bugfix for #314, Haddock now parses more URLs)
            # 2014-08-08: 7946284944 (x86_64/Linux - Haddock updates to attoparsec-0.12.1.0)
            # 2014-09-09: 8354439016 (x86_64/Linux - Applicative/Monad changes, according to Austin)
            # 2014-09-10: 7901230808 (x86_64/Linux - Applicative/Monad changes, according to Joachim)
            # 2014-10-07: 8322584616 (x86_64/Linux)
            # 2014-12-14: 9502647104 (x86_64/Linux) - Update to Haddock 2.16
            # 2014-01-08: 9014511528 (x86_64/Linux) - Eliminate so-called "silent superclass parameters" (and others)
          ,(platform('i386-unknown-mingw32'), 4202377432, 5)
            # 2013-02-10:                     3358693084 (x86/Windows)
            # 2013-11-13:                     3097751052 (x86/Windows, 64bit machine)
            # 2014-04-04:                     3548581572 (x86/Windows, 64bit machine)
            # 2014-12-01:                     4202377432 (x86/Windows, 64bit machine)

          ,(wordsize(32), 3799130400, 1)])
            # 2012-08-14: 3046487920 (x86/OSX)
            # 2012-10-30: 2955470952 (x86/Windows)
            # 2013-02-10: 3146596848 (x86/OSX)
            # 2014-02-22: 3554624600 (x86/Linux - new haddock)
            # 2014-06-29: 3799130400 (x86/Linux)
            # 2014-08-05: XXX TODO UPDATE ME XXX
      ],
     stats,
     ['../../../../libraries/base/dist-install/doc/html/base/base.haddock.t'])

test('haddock.Cabal',
     [unless(in_tree_compiler(), skip)
     ,stats_num_field('bytes allocated',
          [(wordsize(64), 6387320816, 5)
            # 2012-08-14: 3255435248 (amd64/Linux)
            # 2012-08-29: 3324606664 (amd64/Linux, new codegen)
            # 2012-10-08: 3373401360 (amd64/Linux)
            # 2013-03-13: 3626604824 (amd64/Linux) Cabal updated
            # 2013-03-28: 3517301864 (amd64/Linux) fixed #7796
            # 2013-04-26: 3658801800 (amd64/Linux) Cabal updated
            # 2013-08-26: 3808466816 (amd64/Linux) Cabal updated
            # 2013-11-21: 3908586784 (amd64/Linux) Cabal updated
            # 2013-12-12: 3828567272 (amd64/Linux)
            # 2014-01-12: 3979151552 (amd64/Linux) new parser
            # 2014-06-29: 4200993768 (amd64/Linux)
            # 2014-08-05: 4493770224 (x86_64/Linux - bugfix for #314, Haddock now parses more URLs)
            # 2014-08-29: 4267311856 (x86_64/Linux - w/w for INLINABLE things)
            # 2014-09-09: 4660249216 (x86_64/Linux - Applicative/Monad changes according to Austin)
            # 2014-09-10: 4500376192 (x86_64/Linux - Applicative/Monad changes according to Joachim)
            # 2014-09-24: 5840893376 (x86_64/Linux - Cabal update)
            # 2014-10-04: 6019839624 (x86_64/Linux - Burning Bridges, Cabal update)
            # 2014-12-14: 6387320816 (x86_64/Linux) - Update to Haddock 2.16

          ,(platform('i386-unknown-mingw32'), 3088635556, 5)
            # 2012-10-30:                     1733638168 (x86/Windows)
            # 2013-02-10:                     1906532680 (x86/Windows)
            # 2014-01-28:                     1966911336 (x86/Windows)
            # 2014-04-24:                     2052220292 (x86/Windows)
            # 2014-12-01:                     3088635556 (x86/Windows)

          ,(wordsize(32), 2127198484, 1)])
            # 2012-08-14: 1648610180 (x86/OSX)
            # 2014-01-22: 1986290624 (x86/Linux)
            # 2014-06-29: 2127198484 (x86/Linux)
            # 2014-08-05: XXX TODO UPDATE ME XXX
      ],
     stats,
     ['../../../../libraries/Cabal/Cabal/dist-install/doc/html/Cabal/Cabal.haddock.t'])

test('haddock.compiler',
     [unless(in_tree_compiler(), skip)
     ,stats_num_field('bytes allocated',
          [(wordsize(64), 33562468736, 10)
            # 2012-08-14: 26070600504 (amd64/Linux)
            # 2012-08-29: 26353100288 (amd64/Linux, new CG)
            # 2012-09-18: 26882813032 (amd64/Linux)
            # 2012-11-12: 25990254632 (amd64/Linux)
            # 2014-07-17: 29809571376 (amd64/Linux) general round of updates
            # 2012-11-27: 28708374824 (amd64/Linux)
            # 2014-09-10: 30353349160 (amd64/Linux) post-AMP cleanup
            # 2014-11-22: 33562468736 (amd64/Linux)
          ,(platform('i386-unknown-mingw32'), 217933548, 10)
            # 2012-10-30:                     13773051312 (x86/Windows)
            # 2013-02-10:                     14925262356 (x86/Windows)
            # 2013-11-13:                     14328363592 (x86/Windows, 64bit machine)
            # 2014-12-01:                       104140852 (x86/Windows, sudden shrinkage!)
            # 2014-12-10:                       217933548 increased again
          ,(wordsize(32), 15110426000, 1)])
            # 2012-08-14: 13471797488 (x86/OSX)
            # 2014-01-22: 14581475024 (x86/Linux - new haddock)
            # 2014-06-29: 15110426000 (x86/Linux)
      ],
     stats,
     ['../../../../compiler/stage2/doc/html/ghc/ghc.haddock.t'])
