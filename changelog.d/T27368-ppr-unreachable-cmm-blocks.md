section: cmm
issues: #27368
mrs: !16417
synopsis:
  Cmm dumps now show unreachable blocks under ``-dppr-debug``
description:
  Unreachable blocks stay in a Cmm graph's block map for most of the Cmm
  pipeline, but ``-ddump-cmm-*`` only ever printed the blocks reachable from
  the graph's entry. Adding ``-dppr-debug`` now appends the stored but
  unreachable blocks, which makes bugs like #27368 visible in the dumps.
