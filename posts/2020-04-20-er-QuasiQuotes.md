---
title: QuasiQuotes
subtl: a conceptually simple but crucial extension
issue: 25
---

`QuasiQuotes` allow you to drop arbitrary text inside `[foo|`, `|]` brackets,
where `foo` names your `QuasiQuote` compiler.

```haskell
foothing = [foo|

Here I can put anything I want as long as it isn't a '|'
immediately followed by a ']'. The String will be fed
through the 'foo' function at *compile time*, which can
reject the input to force a compilation error, or
generate some Haskell abstract syntax tree that will be
compiled and used in place of the quasi quote. They are
in effect a way to package up user-defined literals
which may implement arbitrary DSLs.

|]
```

The magic is explained in [Quasiquotation 101](https://www.schoolofhaskell.com/user/marcin/quasiquotation-101).
