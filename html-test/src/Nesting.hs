module Nesting where

{-|
* We can

    * easily go back

        1. some indentation

    * levels

1. @back at the top@

-}
d :: t
d = undefined


{-|
* Beginning of list

    * second list

        * Some indented list but
the presence of this text pushes it out of nesting back to the top.
-}
e :: t
e = undefined


{-|
* Beginning of list

    @
    nested code
        we preserve the space correctly
    @
-}
f :: t
f = undefined


{-|
* Beginning of list

    * Nested list
-}
g :: t
g = undefined

{-|
* Beginning of list

    > nested
    > bird
    > tracks
-}
h :: t
h = undefined

{-|
* Beginning of list
This belongs to the list above!

    > nested
    > bird
    > tracks
    >
    > another line
    >   with indentation

    >nested bird tracks
    >  without leading space

    * Next list
    More of the indented list.

        * Deeper

            * Deeper

                    * Even deeper!
                    * No newline separation even in indented lists.
-}
i :: t
i = undefined



{-|
[All this] Works for
definition lists too.

    > nested
    > bird
    > tracks

    * Next list
    with more of the indented list content.

        Even more content on a new line.

        1. Different type of list

            (2) Deeper

            >>> Here's an example in a list
            example result

                    [b] Even deeper!
                    [c] No newline separation even in indented lists.
                    We can have any paragraph level element that we normally
                    can, like headers

                    === Level 3 header
                    with some content…

                    * and even more lists inside
-}
j :: t
j = undefined
