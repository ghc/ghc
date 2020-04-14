#pragma once

static StgClosure *c10, *c11, *c1102, *c20;

static synthHeap initializeTestHeap(void)
{
    synthHeap sh = allocSynthHeap();

/*
  1.0) Just a simple case to start with.

   1
  /
  0---2---3
  \
   4
*/
    c10 = node3(&sh, 1000,
                node0(&sh, 1001),
                node1(&sh, 1002,
                      node0(&sh, 1003)),
                node0(&sh, 1004));

/*
  1.1) Now with a merge

   1
  /` \,
  0--->2--->3
  \,
   4
*/
    c1102 = node1(&sh, 1102,
              node0(&sh, 1103));
    c11 = node3(&sh, 1100,
                node1(&sh, 1101,
                      c1102),
                c1102,
                node0(&sh, 1104));

/*
  2.0) This tests the chain optimization.

   1     5
  /     /
  0-2-3-4-6
  \     \
   8     7
*/
    c20 = node3(&sh, 2000,
                node0(&sh, 2001),
                node1(&sh, 2002,
                      node1(&sh, 2003,
                            node3(&sh, 2004,
                                  node0(&sh, 2005),
                                  node0(&sh, 2006),
                                  node0(&sh, 2007)))),
                node0(&sh, 2008));

    return sh;
}
