---
title: data-default
subtl: A brilliant little package
issue: 9
---

It has taken me a while to properly appreciate `data-default`, but I am
getting there. It is deceptively simple.

[Beware, Haddocks don't always deal gracefully with packages re-exporting
things from other packages. In this case the `Default` class is actually defined
in
[`data-default-class`](https://hackage.haskell.org/package/data-default-class-0.1.2.0/docs/Data-Default-Class.html)
where you can see its definition clearly, unlike in `data-default` (currently).
]

```haskell
class Default a where
  def :: a
```

`Default` allows you to nominate defaults for your types, allowing them
to be deployed with the drop of a `def`. If your type `Foo` supports
generics you can let the class work out the default,

```haskell
class Default Foo where
```

and use it in situations where a `Foo` is needed,

```haskell
renderFoo :: Foo -> String

main :: Foo -> IO ()
main = putStrLn $ "the Foo : "++renderFoo def
```

or maybe:

```haskell
main :: Foo -> IO ()
main = putStrLn $ "the Foo : "++show (def::Foo)
```

This works best where you have a type with an obvious canonical value, such as a
configuration with an obvious default.
