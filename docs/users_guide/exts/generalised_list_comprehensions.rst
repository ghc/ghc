.. _generalised-list-comprehensions:

Generalised (SQL-like) List Comprehensions
------------------------------------------

.. index::
   single: list comprehensions; generalised
   single: extended list comprehensions
   single: group
   single: SQL

.. extension:: TransformListComp
    :shortdesc: Enable generalised list comprehensions.

    :since: 6.10.1

    Allow use of generalised list (SQL-like) comprehension syntax. This
    introduces the ``group``, ``by``, and ``using`` keywords.

Generalised list comprehensions are a further enhancement to the list
comprehension syntactic sugar to allow operations such as sorting and
grouping which are familiar from SQL. They are fully described in the
paper `Comprehensive comprehensions: comprehensions with "order by" and
"group by" <https://www.microsoft.com/en-us/research/wp-content/uploads/2007/09/list-comp.pdf>`__,
except that the syntax we use differs slightly from the paper.

The extension is enabled with the extension :extension:`TransformListComp`.

Here is an example:

::

    employees = [ ("Simon", "MS", 80)
                , ("Erik", "MS", 100)
                , ("Phil", "Ed", 40)
                , ("Gordon", "Ed", 45)
                , ("Paul", "Yale", 60) ]

    output = [ (the dept, sum salary)
             | (name, dept, salary) <- employees
             , then group by dept using groupWith
             , then sortWith by (sum salary)
             , then take 5 ]

In this example, the list ``output`` would take on the value:

::

    [("Yale", 60), ("Ed", 85), ("MS", 180)]

There are three new keywords: ``group``, ``by``, and ``using``. (The
functions ``sortWith`` and ``groupWith`` are not keywords; they are
ordinary functions that are exported by ``GHC.Exts``.)

There are five new forms of comprehension qualifier, all introduced by
the (existing) keyword ``then``:

-  ::

       then f

   This statement requires that
   f
   have the type
   forall a. [a] -> [a]
   . You can see an example of its use in the motivating example, as
   this form is used to apply
   take 5
   .
-  ::

       then f by e

   This form is similar to the previous one, but allows you to create a
   function which will be passed as the first argument to f. As a
   consequence f must have the type
   ``forall a. (a -> t) -> [a] -> [a]``. As you can see from the type,
   this function lets f "project out" some information from the elements
   of the list it is transforming.

   An example is shown in the opening example, where ``sortWith`` is
   supplied with a function that lets it find out the ``sum salary`` for
   any item in the list comprehension it transforms.

-  ::

       then group by e using f

   This is the most general of the grouping-type statements. In this
   form, f is required to have type
   ``forall a. (a -> t) -> [a] -> [[a]]``. As with the ``then f by e``
   case above, the first argument is a function supplied to f by the
   compiler which lets it compute e on every element of the list being
   transformed. However, unlike the non-grouping case, f additionally
   partitions the list into a number of sublists: this means that at
   every point after this statement, binders occurring before it in the
   comprehension refer to *lists* of possible values, not single values.
   To help understand this, let's look at an example:

   ::

       -- This works similarly to groupWith in GHC.Exts, but doesn't sort its input first
       groupRuns :: Eq b => (a -> b) -> [a] -> [[a]]
       groupRuns f = groupBy (\x y -> f x == f y)

       output = [ (the x, y)
       | x <- ([1..3] ++ [1..2])
       , y <- [4..6]
       , then group by x using groupRuns ]

   This results in the variable ``output`` taking on the value below:

   ::

       [(1, [4, 5, 6]), (2, [4, 5, 6]), (3, [4, 5, 6]), (1, [4, 5, 6]), (2, [4, 5, 6])]

   Note that we have used the ``the`` function to change the type of x
   from a list to its original numeric type. The variable y, in
   contrast, is left unchanged from the list form introduced by the
   grouping.

-  ::

       then group using f

   With this form of the group statement, f is required to simply have
   the type ``forall a. [a] -> [[a]]``, which will be used to group up
   the comprehension so far directly. An example of this form is as
   follows:

   ::

       output = [ x
       | y <- [1..5]
       , x <- "hello"
       , then group using inits]

   This will yield a list containing every prefix of the word "hello"
   written out 5 times:

   ::

       ["","h","he","hel","hell","hello","helloh","hellohe","hellohel","hellohell","hellohello","hellohelloh",...]


