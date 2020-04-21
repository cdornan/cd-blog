---
title: RecordWildCards
subtl: a mighty handy extension not universally appreciated
issue: 23
---

Haskell records get a bad rap but I must say I have got so used to them that I
no longer see the warts and `RecordWildCards` certainly help, but they are not
without problems.

A quick reminder form [some
code](https://github.com/cdornan/cd-blog/blob/master/src/CDBlog/Types/ExtensionReviews.hs#L19-L68)
used to generate this page. Given the following definition,

```haskell
data ExtensionReview =
  ExtensionReview
    { _er_id      :: ExtensionID
    , _er_date    :: Date
    , _er_home    :: URL
    , _er_stars   :: Stars
    }
```

the pattern form `ExtensionReview{..}` can be used to make available the fields
of the record under their own name.

```haskell
instance Buildable ExtensionReview where
  build er@ExtensionReview{..} =
      "<div class=cd-er-row>"
        +|exnm _er_id   |+"\n"
        +|date _er_date |+"\n"
        +|home _er_home |+"\n"
        +|strs _er_stars|+
      "</div>\n"
    where
      ...
```

The problem are twofold:

  * the record fields are normally used as projection functions to extract the
    fields from values of the type, which are no longer are available inside the
    scope of this pattern construction;

  * In addition to identifiers flipping their meaning inside the scope of the
    pattern, Haskellers are not used to a bunch of identifiers springing into
    scope inside an expression. It is just not the way Haskell rolls normally.

So `RecordWildCards` are considered a bit icky by some Haskellers and maybe they
are. But I will tell you what, they are just way too useful to allow such
scruples get in the way of their use. My advice to anyone avoiding them is to
park your scruples, bite down and give them a try. You might find them just too
useful.

(I only consider `RecordWildCards` used in patterns here &mdash; see [Dmitrii
Kovanikov's blog post](https://kodimensional.dev/recordwildcards) for a
discussion that includes their use to construct records in expressions.)
