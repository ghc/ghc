# Difference Lists

[![test-badge][]][test]
[![hackage-badge][]][hackage-dlist]
[![packdeps-badge][]][packdeps]

_**List-like types supporting O(1) `append` and `snoc` operations.**_

## Installation

[`dlist`][hackage-dlist] is a Haskell package available from [Hackage][hackage].
It can be installed with [`cabal`][cabal] or [`stack`][stack].

See the [change log][changelog] for the changes in each version.

## Usage

Here is an example of “flattening” a `Tree` into a list of the elements in its
`Leaf` constructors:

```haskell
import qualified Data.DList as DList

data Tree a = Leaf a | Branch (Tree a) (Tree a)

flattenSlow :: Tree a -> [a]
flattenSlow = go
  where
    go (Leaf x) = [x]
    go (Branch left right) = go left ++ go right

flattenFast :: Tree a -> [a]
flattenFast = DList.toList . go
  where
    go (Leaf x) = DList.singleton x
    go (Branch left right) = go left `DList.append` go right
```

(The above code can be found in the [benchmark][].)

`flattenSlow` is likely to be slower than `flattenFast`:

1. `flattenSlow` uses `++` to concatenate lists, each of which is recursively
   constructed from the `left` and `right` `Tree` values in the `Branch`
   constructor.

2. `flattenFast` does not use `++` but constructs a composition of functions,
   each of which is a “cons” introduced by `DList.singleton` (`(x :)`). The
   function `DList.toList` applies the composed function to `[]`, constructing
   a list in the end.

To see the difference between `flattenSlow` and `flattenFast`, consider some
rough evaluations of the functions applied to a `Tree`:

```haskell
flattenSlow (Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  = go (Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  = go (Branch (Leaf 'a') (Leaf 'b')) ++ go (Leaf 'c')
  = (go (Leaf 'a') ++ go (Leaf 'b')) ++ "c"
  = ("a" ++ "b") ++ "c"
  = ('a' : [] ++ "b") ++ "c"
  = ('a' : "b") ++ "c"
  = 'a' : "b" ++ "c"
  = 'a' : 'b' : [] ++ "c"
  = 'a' : 'b' : "c"
```

```haskell
flattenFast (Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  = toList $ go (Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  = toList $ go (Branch (Leaf 'a') (Leaf 'b')) `append` go (Leaf 'c')
  = unsafeApplyDList (go (Branch (Leaf 'a') (Leaf 'b'))) . unsafeApplyDList (go (Leaf 'c')) $ []
  = unsafeApplyDList (go (Branch (Leaf 'a') (Leaf 'b'))) (unsafeApplyDList (go (Leaf 'c')) [])
  = unsafeApplyDList (go (Branch (Leaf 'a') (Leaf 'b'))) (unsafeApplyDList (singleton 'c') [])
  = unsafeApplyDList (go (Branch (Leaf 'a') (Leaf 'b'))) (unsafeApplyDList (UnsafeDList ((:) 'c')) [])
  = unsafeApplyDList (go (Branch (Leaf 'a') (Leaf 'b'))) "c"
  = unsafeApplyDList (UnsafeDList (unsafeApplyDList (go (Leaf 'a')) . unsafeApplyDList (go (Leaf 'b')))) "c"
  = unsafeApplyDList (go (Leaf 'a')) (unsafeApplyDList (go (Leaf 'b')) "c")
  = unsafeApplyDList (go (Leaf 'a')) (unsafeApplyDList (singleton 'b') "c")
  = unsafeApplyDList (go (Leaf 'a')) (unsafeApplyDList (UnsafeDList ((:) 'b')) "c")
  = unsafeApplyDList (go (Leaf 'a')) ('b' : "c")
  = unsafeApplyDList (singleton 'a') ('b' : "c")
  = unsafeApplyDList (UnsafeDList ((:) 'a')) ('b' : "c")
  = 'a' : 'b' : "c"
```

The left-nested `++` in `flattenSlow` results in intermediate list constructions
that are immediately discarded in the evaluation of the outermost `++`. On the
other hand, the evaluation of `flattenFast` involves no intermediate list
construction but rather function applications and `newtype` constructor wrapping
and unwrapping. This is where the efficiency comes from.

_**Warning!**_ Note that there is truth in the above, but there is also a lot of
hand-waving and intrinsic complexity. For example, there may be GHC rewrite
rules that apply to `++`, which will change the actual evaluation. And, of
course, strictness, laziness, and sharing all play a significant role. Also, not
every function in the `dlist` package is the most efficient for every situation.

_**Moral of the story:**_ If you are using `dlist` to speed up your code, check
to be sure that it actually does. Benchmark!

## Design Notes

These are some notes on design and development choices made for the `dlist`
package.

### Avoid `++`

The original intent of Hughes' representation of lists as first-class functions
was to provide an abstraction such that the list `append` operation found in
functional programming languages (and now called `++` in Haskell) would not
appear in left-nested positions to avoid duplicated structure as lists are
constructed. The lesson learned by many people using list over the years is that
the `append` operation can appear, sometimes surprisingly, in places they don't
expect it.

One of our goals is for the `dlist` package to avoid surprising its users with
unexpected insertions of `++`. Towards this end, there should be a minimal set
of functions in `dlist` in which `++` can be directly or indirectly found. The
list of known uses of `++` includes:

* `DList`: `fromList`, `fromString`, `read`
* `DNonEmpty`: `fromList`, `fromNonEmpty`, `fromString`, `read`

If any future requested functions involve `++` (e.g. via `fromList`), the burden
of inclusion is higher than it would be otherwise.

### Abstraction

The `DList` representation and its supporting functions (e.g. `append`, `snoc`,
etc.) rely on an invariant to preserve its safe use. That is, without this
invariant, a user may encounter unexpected outcomes.

(We use safety in the sense that the semantics are well-defined and expected,
not in the sense of side of referential transparency. The invariant does not
directly lead to side effects in the `dlist` package, but a program that uses an
unsafely generated `DList` may do something surprising.)

The invariant is that, for any `xs :: DList a`:

```haskell
fromList (toList xs) = xs
```

To see how this invariant can be broken, consider this example:

```haskell
xs :: DList a
xs = UnsafeDList (const [])

fromList (toList (xs `snoc` 1))
  = fromList (toList (UnsafeDList (const []) `snoc` 1))
  = fromList (toList (UnsafeDList (unsafeApplyDList (UnsafeDList (const [])) . (x :))))
  = fromList (toList (UnsafeDList (const [] . (x :))))
  = fromList (($ []) . unsafeApplyDList $ UnsafeDList (const [] . (x :)))
  = fromList (const [] . (x :) $ [])
  = fromList (const [] [x])
  = fromList []
  = UnsafeDList (++ [])
```

The invariant can also be stated as:

```haskell
toList (fromList (toList xs)) = toList xs
```

And we can restate the example as:

```haskell
toList (fromList (toList (xs `snoc` 1)))
  = toList (UnsafeDList (++ []))
  = []
```

It would be rather unhelpful and surprising to find ``(xs `snoc` 1)`` turned out
to be the empty list.

To preserve the invariant on `DList`, we provide it as an abstract type in the
`Data.DList` module. The constructor, `UnsafeDList`, and record label,
`unsafeApplyDList`, are not exported because these can be used, as shown above,
to break the invariant.

All of that said, there have been numerous requests to export the `DList`
constructor. We are not convinced that it is necessary, but we are convinced
that users should decide for themselves.

To use the constructor and record label of `DList`, you import them as follows:

```haskell
import Data.DList.Unsafe (DList(UnsafeDList, unsafeApplyDList))
```

If you are using Safe Haskell, you may need to add this at the top of your
module:

```haskell
{-# LANGUAGE Trustworthy #-}
```

Just be aware that the burden of proof for safety is on you.

## References

These are various references where you can learn more about difference lists.

### Research

* **A novel representation of lists and its application to the function
  “reverse.”** John Hughes. Information Processing Letters. Volume 22, Issue 3.
  1986-03. Pages 141-144. [PDF][hughes-pdf]

  This is the original published source for a representation of lists as
  first-class functions.

### Background

* [Wikipedia][wikipedia]
* [Haskell Wiki][wiki-haskell]
* [Stack Overflow][stack-overflow]

### Blogs and Mailing Lists

* [Using Difference Lists][blog-auclair-1]. Douglas M. Auclair. 2008-08-13.
* [A Sort of Difference][blog-kmett]. Edward Kmett. 2008-09-18.
* [Reference for technique wanted][mail-okeefe]. Richard O'Keefe, et al.
  2010-10-31.
* [24 Days of Hackage: dlist][blog-charles]. Oliver Charles. 2012-12-14.
* [Constructing a list in a Monad][blog-breitner]. Joachim Breitner. 2013-11-13.
* [Demystifying DList][blog-ellis] ([Reddit][blog-ellis-reddit]). Tom Ellis.
  2014-01-24.
* [keepEquals with Difference Lists][blog-auclair-2]. Douglas M. Auclair.
  2014-06-21.

### Books

* [Chapter 13. Data Structures][book-real-world-haskell]. Real World Haskell.
  2008-12-05.

## License

[BSD 3-Clause “New” or “Revised” License][license] © Don Stewart, Sean Leather,
contributors

[changelog]: https://github.com/spl/dlist/blob/main/changelog.md#change-log
[benchmark]: https://github.com/spl/dlist/blob/main/bench/Main.hs
[blog-auclair-1]: https://logicaltypes.blogspot.com/2008/08/using-difference-lists.html
[blog-auclair-2]: https://logicaltypes.blogspot.com/2014/06/keepequals-with-difference-lists.html
[blog-breitner]: https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad
[blog-charles]: https://ocharles.org.uk/blog/posts/2012-12-14-24-days-of-hackage-dlist.html
[blog-ellis-reddit]: https://www.reddit.com/r/haskell/comments/1w5duf/demystifying_dlist/
[blog-ellis]: http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
[blog-kmett]: https://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/
[book-real-world-haskell]: http://book.realworldhaskell.org/read/data-structures.html
[cabal]: https://cabal.readthedocs.io/
[hackage-badge]: https://img.shields.io/hackage/v/dlist.svg?maxAge=3600
[hackage-dlist]: https://hackage.haskell.org/package/dlist
[hackage]: https://hackage.haskell.org/
[hughes-pdf]: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf
[license]: https://github.com/spl/dlist/blob/main/license.md
[mail-okeefe]: https://www.mail-archive.com/haskell-cafe@haskell.org/msg83699.html
[packdeps-badge]: https://img.shields.io/hackage-deps/v/dlist.svg?maxAge=3600
[packdeps]: http://packdeps.haskellers.com/feed?needle=dlist
[stack-overflow]: https://stackoverflow.com/questions/3352418/what-is-a-dlist
[stack]: https://docs.haskellstack.org/
[test-badge]: https://github.com/spl/dlist/workflows/test/badge.svg
[test]: https://github.com/spl/dlist/actions?query=workflow%3Atest
[wiki-haskell]: https://wiki.haskell.org/Difference_list
[wikipedia]: https://en.wikipedia.org/wiki/Difference_list
