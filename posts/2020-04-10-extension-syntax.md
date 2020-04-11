---
title: Fixing Language Extensions
subtl: It isn't rocket science
issue: 15
---

I do have a beef with Haskell language extensions though. But first a
digression.

## Where to place extensions

Some people like to declare a liberal set of language extensions in the cabal
file and thereby be spared writing them at the head of modules. I accept that
opinions will differ on this practice but I would not advocate it for these
reasons.

  1. Each source module now needs the cabal file to make sense.

  2. Copying a source module between code bases needs to be done with care and
     could lead to silent breakage.

  3. With the extensive collection of Haskell language extensions I would really
     like to know just what is required for each source module that I read. The
     extensions provide me with a really useful handle to find the documentation
     when necessary.

To me (and I am not alone) throwing the switches on long list of extensions in
the cabal file is taking liberties and weakens the case for having them.

That said, as this post atests, I am not wild about the status quo either, but
feel there is much we could be doing to reduce this temptation (including
[publishing Haskell 2020](/posts/2020-04-10-extension-syntax.html).

## Fixing In-Source-File Extensions

I really, really want to write:

```haskell
{-# LANGUAGE
      DeriveAnyClass
      DerivingVia
      OverloadedStrings
      RecordWildCards
#-}
```

but I am forced to write (with obvious variants),

```haskell
{-# LANGUAGE
      DeriveAnyClass
      , DerivingVia
      , OverloadedStrings
      , RecordWildCards
#-}
```

so I actually mostly write,

```haskell
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
```

which is really unfortunate, and I suspect unnecessary.

## Background

I tend to build up extensions as I write the module, and often have the relevant
extension staring at me in a nearby module. It is useful to be able just
transfer the relevant line.

I also like to keep them in a standard order, lexicographical order, which is
really easy to do if every line is laid out uniformly regardless of where it
appears in the list.

All of this gets broken when the compiler insists on comma separators.

The option of preceding the first extension with a comma fix this.

```haskell
{-# LANGUAGE
      , DeriveAnyClass
      , DerivingVia
      , OverloadedStrings
      , RecordWildCards
#-}
```

but why not make them all optional?
