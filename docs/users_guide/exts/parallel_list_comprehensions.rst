.. _parallel-list-comprehensions:

Parallel List Comprehensions
----------------------------

.. index::
   single: list comprehensions; parallel
   single: parallel list comprehensions

.. extension:: ParallelListComp
    :shortdesc: Enable parallel list comprehensions.

    :since: 6.8.1

    Allow parallel list comprehension syntax.

Parallel list comprehensions are a natural extension to list
comprehensions. List comprehensions can be thought of as a nice syntax
for writing maps and filters. Parallel comprehensions extend this to
include the ``zipWith`` family.

A parallel list comprehension has multiple independent branches of
qualifier lists, each separated by a ``|`` symbol. For example, the
following zips together two lists: ::

       [ (x, y) | x <- xs | y <- ys ]

The behaviour of parallel list comprehensions follows that of zip, in
that the resulting list will have the same length as the shortest
branch.

We can define parallel list comprehensions by translation to regular
comprehensions. Here's the basic idea:

Given a parallel comprehension of the form: ::

       [ e | p1 <- e11, p2 <- e12, ...
           | q1 <- e21, q2 <- e22, ...
           ...
       ]

This will be translated to: ::

       [ e | ((p1,p2), (q1,q2), ...) <- zipN [(p1,p2) | p1 <- e11, p2 <- e12, ...]
                                             [(q1,q2) | q1 <- e21, q2 <- e22, ...]
                                             ...
       ]

where ``zipN`` is the appropriate zip for the given number of branches.


