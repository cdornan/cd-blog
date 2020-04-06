---
title: Functor
issue: 10
---

The first place to look for an introduction to any structure is
[Hackage](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html),
which covers `Functor` pretty well. There we are told Functors are:

> uniform action over a parameterized type, generalizing the map
  function on lists.

The class provides a single higher order method for converting the parameter
type of the functor.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

If the transformer function, `f` is of type `Foo->Bar` then `fmap f` will
convert `Baz Foo` values into `Baz Bar` values.

Often the `<$>` operator is used. It is but an alias for `fmap`.

Functors are really quite primal and I would advise anyone starting out with
Haskell prioritising getting used using them, stacks of them. If you have an
`IO` action that returns a `Maybe String`

```haskell
my_action :: Int -> IO (Maybe String)
```

 but you need a `Maybe T.Text` then this should be quite natural (bearing in
 mind that, like many useful types, `IO` and `Maybe` are functors):

```haskell
txt <- fmap T.pack <$> my_action 42
```

Of course `fmap T.pack` (in this context) is of type

```haskell
fmap T.pack :: Maybe String -> Maybe T.Text
```

which is just a function we can apply as an argument to `fmap`:

```haskell
fmap (fmap T.pack) :: IO (Maybe String) -> IO (Maybe T.Text)
```

and so,

```haskell
fmap (fmap T.pack) (my_action 42) :: IO (Maybe T.Text)
```

but `fmap` and `<$>` are the same function (in the standard `Prelude` anyway) so
this is equivalent to:

```haskell
fmap T.pack <$> my_action 42 :: IO (Maybe T.Text)
```

[Notice the way that whole explanation of stacked functors took place entirely
in the land of types &mdash; this is a central feature of Haskell that every
Haskell program or fragment has a dual life: a static life in types and a
dynamic life at runtime.]

The above pattern is so pervasive that it is worth getting familiar with,
especially in an `IO` context. `f <$> action` (or `fmap f action`) applies
some arbitrary function `f` to the output of the `IO` `action`.

But this is bur a single pattern. Functors can be applied pretty much anywhere.
