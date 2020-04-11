---
title: OverloadedStrings
subtl: The most important extension
issue: 8
---

With this extension classic string literals (of type `[Char]`) gets wrapped
in the `fromString` from the `IsString` class in `Data.String`:

```haskell
class IsString a where
  fromString :: String -> a
```

So, for example, this,

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text as T

main = T.putStrLn "this is a string"
```

would get turned into this:

```haskell
import Data.String
import Data.Text as T

main = T.putStrLn (fromString "this is a string")
```

Got a type that wants to join the party and be representable by strings? Then
make it an instance `IsString`.

Should probably be always on in Haskell 2020.
