---
title: Functor
subtl: The basis of things to come
issue: 10
---

_[This article was expanded on 2020-04-06.]_

The first place to look for an introduction to any structure is
[Hackage](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html),
which covers `Functor` pretty well. Consider this article a commentary on that
page.

There we are told Functors are:

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

## Other Operators

Functor provides some useful auxiliary operators and functions.

### (<$)

```haskell
(<$) :: Functor f => a -> f b -> f a
```

is equivalent to this

```haskell
c (<$) s = const c <$> s
```

forcing the output of `s` to a constant value:

```haskell
-- | uses `getLine` to read a line from stdin and discards it
discardLine :: IO ()
discardLine = () <$ getLine
```


### ($>)

This operator

```haskell
($>) :: Functor f => f a -> b -> f b
```

is the same operator as `<$`, but with its arguments flipped:

```haskell
s $> c = c <$ s
```

so we could have written above:

```haskell
-- | uses `getLine` to read a line from stdin and discards it
discardLine :: IO ()
discardLine = getLine $> ()
```


### (<&>)

This operator,

```haskell
(<&>) :: Functor f => f a -> (a -> b) -> f b
```

is a flipped version of `($>)`

```haskell
s <&> f = f <$> s

```

so we can say

```haskell
import Data.Char (toUpper)

-- | read string from stdin, mapping each char to upper case
getUpper :: IO String
getUpper = readLine <&> map toUpper
```


### void

```haskell
void :: Functor f => f a -> f ()
```

This is just a special case of `<$` (or `$>`) that force the output of the
functor to the unit type `()`.

```haskell
void s = () <$ s
```

Above, e could have written:

```haskell
-- | uses `getLine` to read a line from stdin and discards it
discardLine :: IO ()
discardLine = void getLine
```


## Best is yet to come

In [the notes for
[`($)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html#v:-60--36--62-)
on the Hackage `Functor` page notes the symmetries between the function application operator `($)` and
`(<$>)`.

```haskell
( $ ) ::              (a -> b) ->   a ->   b
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

This is not accidental and totally in keeping with the way it is used, including
the examples at the top of this article.

Just as `($)` can be used to transform the output of a function, so `(<$>)`
can be used to transform the output of a Functor.

```haskell
import Data.Char            (toLower)
import System.Environment   (getEnv)

data Env = Env { envLabel :: String }

prjLabel :: Env    ->    String
getLabel :: String -> IO String

prjLabel env = map toLower  $  envLabel env
getLabel nme = map toLower <$> getEnv   nme
```

But hold on a minute, aren't we getting ahead of ourselves, `getEnv` is an `IO`,
a famous monad, not a Functor? But all monads are applicatives and all
applicatives are functors, so monads are functors of course.

Everything we are saying about functors is true of applicatives (and monads),
and everything we say about applicatives will be true of monads. This
compositional understanding is important to understanding the big picture,
and as we can see it provides an accessible place to start.


## Identities

Functors come from abstract algebra and, as we are reminded at the top of the
[top Functor Hackage
page](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html#t:Functor),
where they are characterised by the following identities.

```haskell
fmap    id    ==  id
fmap (f . g)  ==  fmap f . fmap g
```

That these simple identities (along with those for the other structures) can
give rise to all of this is truly a wonderous thing!
