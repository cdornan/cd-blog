---
title: heredoc
subtl: a simple but highly useful package
issue: 26
---

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Heredoc

main = putStr [here|
heredoc is an insanely simple but useful package that makes
the most straightforward use of `QuasiQuotes`. If you need
to embed a block of text in a module then `heredoc` is your
friend.
|]
```

`heredocs` has a couple of extra tricks. The beautifully clear and simple
[`heredoc`
Haddocks](https://hackage.haskell.org/package/heredoc-0.2.0.0/docs/Text-Heredoc.html)
have the details.
