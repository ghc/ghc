# We do not add peak_megabytes_allocated and max_bytes_used to these tests, as
# they are somewhat unreliable, and it is harder to re-run these numbers to
# detect outliers, as described in Note [residency]. See #9556.

test('haddock.base',
     [extra_files(['../../../../libraries/base/dist-install/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'), 18733710728, 5)
            # 2017-02-19                        24286343184 (x64/Windows) - Generalize kind of (->)
            # 2017-12-24                        18733710728 (x64/Windows) - Unknown

          ,(wordsize(64), 19694554424, 5)
            # 2012-08-14:  5920822352 (amd64/Linux)
            # 2012-09-20:  5829972376 (amd64/Linux)
            # 2012-10-08:  5902601224 (amd64/Linux)
            # 2013-01-17:  6064874536 (x86_64/Linux)
            # 2013-02-10:  6282746976 (x86_64/Linux)
            # 2013-09-17:  6634886456 (x86_64/Linux)
            # 2013-09-18:  6294339840 (x86_64/Linux)
            # 2013-11-21:  6756213256 (x86_64/Linux)
            # 2014-01-12:  7128342344 (x86_64/Linux)
            # 2014-06-12:  7498123680 (x86_64/Linux)
            # 2014-08-05:  7992757384 (x86_64/Linux - bugfix for #314, Haddock now parses more URLs)
            # 2014-08-08:  7946284944 (x86_64/Linux - Haddock updates to attoparsec-0.12.1.0)
            # 2014-09-09:  8354439016 (x86_64/Linux - Applicative/Monad changes, according to Austin)
            # 2014-09-10:  7901230808 (x86_64/Linux - Applicative/Monad changes, according to Joachim)
            # 2014-10-07:  8322584616 (x86_64/Linux)
            # 2014-12-14:  9502647104 (x86_64/Linux) - Update to Haddock 2.16
            # 2014-01-08:  9014511528 (x86_64/Linux) - Eliminate so-called "silent superclass parameters" (and others)
            # 2015-07-22:  9418857192 (x86_64/Linux) - Just slowly creeping up.
            # 2015-10-03:  9894189856 (x86_64/Linux) - Still creeping
            # 2015-12-11: 11119767632 (amd64/Linux) - TypeInType (see #11196)
            # 2015-12-17: 26282821104 (x86_64/Linux) - Update Haddock to master
            # 2015-12-17: 27812188000 (x86_64/Linux) - Move Data.Functor.* into base
            # 2016-02-25: 30987348040 (x86_64/Linux) - RuntimeRep
            # 2016-05-12: 32855223200 (x86_64/Linux) - Make Generic1 poly-kinded
            # 2017-01-11: 31115778088 (x86_64/Linux) - Join points (#12988)
            # 2017-02-11: 34819979936 (x86_64/Linux) - OccurAnal / One-Shot  (#13227)
            # 2017-02-16: 32695562088 Better Lint for join points
            # 2017-02-17: 38425793776 (x86_64/Linux) - Generalize kind of (->)
            # 2017-02-12: 25592972912 (x86_64/Linux) - Type-indexed Typeable
            # 2017-06-05: 27868466432 (x86_64/Linux) - Desugar modules compiled with -fno-code
            # 2017-06-06: 25173968808 (x86_64/Linux) - Don't pass on -dcore-lint in Haddock.mk
            # 2017-07-12: 23677299848 (x86_64/Linux) - Use getNameToInstancesIndex
            # 2017-08-22: 19694554424 (x86_64/Linux) - Various Haddock optimizations

          ,(platform('i386-unknown-mingw32'), 2885173512, 5)
            # 2013-02-10:                     3358693084 (x86/Windows)
            # 2013-11-13:                     3097751052 (x86/Windows, 64bit machine)
            # 2014-04-04:                     3548581572 (x86/Windows, 64bit machine)
            # 2014-12-01:                     4202377432 (x86/Windows, 64bit machine)
            # 2015-01-20:                     4434804940 (x86/Windows, 64bit machine)
            # 2017-04-02:                     2885173512 update

          ,(wordsize(32), 3445319728, 5)])
            # 2012-08-14: 3046487920 (x86/OSX)
            # 2012-10-30: 2955470952 (x86/Windows)
            # 2013-02-10: 3146596848 (x86/OSX)
            # 2014-02-22: 3554624600 (x86/Linux - new haddock)
            # 2014-06-29: 3799130400 (x86/Linux)
            # 2016-04-06: 5509757068 (x86/Linux)
            # 2017-03-24: 3819657568 (x86/Linux)
            # 2017-04-06: 3445319728 (x86/Linux)
      ],
     stats,
     ['haddock.t'])

test('haddock.Cabal',
     [extra_files(['../../../../libraries/Cabal/Cabal/dist-install/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,stats_num_field('bytes allocated',
          [(wordsize(64), 25261834904, 5)
            # 2012-08-14:  3255435248 (amd64/Linux)
            # 2012-08-29:  3324606664 (amd64/Linux, new codegen)
            # 2012-10-08:  3373401360 (amd64/Linux)
            # 2013-03-13:  3626604824 (amd64/Linux) Cabal updated
            # 2013-03-28:  3517301864 (amd64/Linux) fixed #7796
            # 2013-04-26:  3658801800 (amd64/Linux) Cabal updated
            # 2013-08-26:  3808466816 (amd64/Linux) Cabal updated
            # 2013-11-21:  3908586784 (amd64/Linux) Cabal updated
            # 2013-12-12:  3828567272 (amd64/Linux)
            # 2014-01-12:  3979151552 (amd64/Linux) new parser
            # 2014-06-29:  4200993768 (amd64/Linux)
            # 2014-08-05:  4493770224 (x86_64/Linux - bugfix for #314, Haddock now parses more URLs)
            # 2014-08-29:  4267311856 (x86_64/Linux - w/w for INLINABLE things)
            # 2014-09-09:  4660249216 (x86_64/Linux - Applicative/Monad changes according to Austin)
            # 2014-09-10:  4500376192 (x86_64/Linux - Applicative/Monad changes according to Joachim)
            # 2014-09-24:  5840893376 (x86_64/Linux - Cabal update)
            # 2014-10-04:  6019839624 (x86_64/Linux - Burning Bridges, Cabal update)
            # 2014-12-14:  6387320816 (x86_64/Linux) - Update to Haddock 2.16
            # 2015-01-22:  6710234312 (x86_64/Linux) - Cabal updated
            # 2015-06-29:  7413958344 (x86_64/Linux) - due to #10482, not yet investigated
            # 2015-12-11:  8114833312 (amd64/Linux) - TypeInType (See #11196)
            # 2015-12-17:  9982130512 (amd64/Linux) - Update Haddock to master
            # 2015-12-22: 10519532424 (amd64/Linux) - Lots of new Semigroup instances in Cabal
            # 2016-03-29: 11517963232 (amd64/Linux) - not yet investigated
            # 2016-03-30: 10941742184 (amd64/Linux) - defer inlining of Int* Ord methods
            # 2016-04-06: 11542374816 (amd64/Linux) - CSE improvements and others
            # 2016-04-07: 10963514352 (amd64/Linux) - Revert to what phabricator claims
            # 2016-05-22: 11805238152 (amd64/Linux) - Make Generic1 poly-kinded
            # 2016-06-05: 10997887320 (amd64/Linux) - Refactor derived Generic instances to reduce allocations
            # 2016-06-21: 10070330520 (amd64/Linux) - D2350: Make checkFamInstConsistency less expensive
            # 2016-08-07: 16001233464 (amd64/Linux) - Cabal update
            #   It's worth noting that allocations scale up with the number
            #   of modules in Cabal.  This Cabal update added a large number
            #   of new modules; if you exclude them from the haddock run
            #   the stats are comparable.
            # 2016-10-01: 20619433656 (amd64/Linux) - Cabal update
            # 2016-10-03: 21554874976 (amd64/Linux) - Cabal update
            # 2016-10-06: 23706190072 (amd64/Linux) - Cabal update
            # 2016-12-20: 25478853176 (amd64/Linux) - Cabal update
            # 2017-01-14: 23272708864 (amd64/Linux) - Join points (#12988)
            # 2017-02-11: 25533642168 (amd64/Linux) - OccurAnal / One-Shot  (#13227)
            # 2017-02-16: 23867276992  Better Lint for join points
            # 2017-02-17: 27784875792 (amd64/Linux) - Generalize kind of (->)
            # 2017-02-12: 18865432648 (amd64/Linux) - Type-indexed Typeable
            # 2017-05-31: 18269309128 (amd64/Linux) - Faster checkFamInstConsistency
            # 2017-06-05: 22294859000 (amd64/Linux) - Desugar modules compiled with -fno-code
            # 2017-06-05: 18753963960 (amd64/Linux) - Don't pass on -dcore-lint in Haddock.mk
            # 2017-08-22: 15857428040 (amd64/Linux) - Various Haddock optimizations
            # 2017-11-02: 17133915848 (amd64/Linux) - Phabricator D4144
            # 2017-11-06: 18936339648 (amd64/Linux) - Unknown
            # 2017-11-09: 20104611952 (amd64/Linux) - Bump Cabal
            # 2018-01-22: 25261834904 (amd64/Linux) - Bump Cabal

          ,(platform('i386-unknown-mingw32'), 3293415576, 5)
            # 2012-10-30:                     1733638168 (x86/Windows)
            # 2013-02-10:                     1906532680 (x86/Windows)
            # 2014-01-28:                     1966911336 (x86/Windows)
            # 2014-04-24:                     2052220292 (x86/Windows)
            # 2014-12-01:                     3088635556 (x86/Windows)
            # 2015-01-20:                     3293415576

          ,(wordsize(32), 3511151136, 5)])
            # 2012-08-14: 1648610180 (x86/OSX)
            # 2014-01-22: 1986290624 (x86/Linux)
            # 2014-06-29: 2127198484 (x86/Linux)
            # 2016-04-06: 6268156056 (x86/Linux)
            # 2017-03-24: 3511151136 (x86/Linux)
      ],
     stats,
     ['haddock.t'])

test('haddock.compiler',
     [extra_files(['../../../../compiler/stage2/haddock.t']),
      unless(in_tree_compiler(), skip), req_haddock
     ,stats_num_field('bytes allocated',
          [(platform('x86_64-unknown-mingw32'),   56775301896, 10),
            # 2017-12-24:                     56775301896 (x64/Windows)
            (wordsize(64), 51592019560, 10)
            # 2012-08-14: 26070600504 (amd64/Linux)
            # 2012-08-29: 26353100288 (amd64/Linux, new CG)
            # 2012-09-18: 26882813032 (amd64/Linux)
            # 2012-11-12: 25990254632 (amd64/Linux)
            # 2014-07-17: 29809571376 (amd64/Linux) general round of updates
            # 2012-11-27: 28708374824 (amd64/Linux)
            # 2014-09-10: 30353349160 (amd64/Linux) post-AMP cleanup
            # 2014-11-22: 33562468736 (amd64/Linux)
            # 2015-06-02: 36740649320 (amd64/Linux) unknown cause
            # 2015-06-29: 40624322224 (amd64/Linux) due to #10482, not yet investigated
            # 2015-12-03: 44721228752 (amd64/Linux) slow creep upwards
            # 2015-12-15: 49395782136 (amd64/Linux) more creep, following kind-equalities
            # 2015-12-17: 58017214568 (amd64/Linux) update Haddock to master
            # 2016-06-21: 55314944264 (amd64/Linux) D2350: Make checkFamInstConsistency less expensive
            # 2016-11-29: 60911147344 (amd64/Linux) unknown cause
            # 2017-02-11: 62070477608 (amd64/Linux) OccurAnal / One-Shot  (#13227) (and others)
            # 2017-02-25: 55777283352 (amd64/Linux) Early inline patch
            # 2017-05-31: 52762752968 (amd64/Linux) Faster checkFamInstConsistency
            # 2017-06-05: 65378619232 (amd64/Linux) Desugar modules compiled with -fno-code
            # 2017-06-06: 55990521024 (amd64/Linux) Don't pass on -dcore-lint in Haddock.mk
            # 2017-07-12: 51592019560 (amd64/Linux) Use getNameToInstancesIndex

          ,(platform('i386-unknown-mingw32'),   367546388, 10)
            # 2012-10-30:                     13773051312 (x86/Windows)
            # 2013-02-10:                     14925262356 (x86/Windows)
            # 2013-11-13:                     14328363592 (x86/Windows, 64bit machine)
            # 2014-12-01:                       104140852 (x86/Windows, sudden shrinkage!)
            # 2014-12-10:                       217933548 increased again
            # 2017-04-02:                       367546388 update

          ,(wordsize(32), 3775852520, 5)])
            # 2012-08-14: 13471797488 (x86/OSX)
            # 2014-01-22: 14581475024 (x86/Linux - new haddock)
            # 2014-06-29: 15110426000 (x86/Linux)
            # 2016-04-06: 16222702892 (x86/Linux)
            # 2017-03-24: 3775852520  (x86/Linux)
      ],
     stats,
     ['haddock.t'])
