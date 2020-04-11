---
title: Pointless Style
subtl: Point-free considered useful
issue: 11
---

This should perhaps be called point free style, but I could not resist the pun
(and I am not the first) being an occasional consumer of
[Pointless](https://www.bbc.co.uk/programmes/b00rhg2r) where pointless things
are the most valuable of all. I like pointless style because I like pipelines.

I remember [Steve Furber](https://en.wikipedia.org/wiki/Steve_Furber), the chief
architect of the original ARM processor, opining of the [RISC
revolution](https://en.wikipedia.org/wiki/Reduced_instruction_set_computer) that
the point was that microprocessor architecture had been simplified to the extent
that pipelining &mdash; executing several instructions concurrently with each
instruction being processed in different stages &mdash; became feasible. This
informs my thinking on pipelines. As an organising principle, they are really
powerful.

## Point Free

A classic pointy function definition looks like this,

```haskell
f x = p (q (r (s (t x))))
```

while in the point-free style it would look like this:

```haskell
f = p . q . r . s . t
```

The distinction is that the argument of the function &mdash; the point &mdash;
gets named in the pointy style, but not in the pointless style.

## A Real Example

Pipelines can be found everywhere. Take this fragment from the main module of
the Hakyll generator used to prepare this blog:

```haskell
  compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
```

The `compile` Hakyll library function takes a monadic value as its argument,
which it runs to determine how to prepare a particular kind of blog artefact for
publication. The point police could detect an illegal pipeline here and insist
that, for readability reasons, this be rewritten in the pointy style.

```haskell
  compile $ do
    pandocStuffPresumablyHtml <- pandocCompiler
    blahurg <- loadAndApplyTemplate "templates/post.html" postCtx pandocStuffPresumablyHtml
    _wibble <- loadAndApplyTemplate "templates/default.html" postCtx blahurg
    bloggyOutput <- relativizeUrls blahurg
    return bloggyOutput
```

Well almost, except I have misplaced one of my points for which I am not going
to get even a warning.

Hakyll has been beautifully constructed to be accessible to non-Haskellers
around an elegant DSL that makes heavy use of pipelines. The user really does
not want to be concerned with naming and getting involved with all of the
points.

For non-Haskellers.

There is a point to the way of pointlessness!


## The Real Problem

I care about readable code and consider readability almost paramount. (I mean
within reason, of course.)

The real problem comes when folks try to construct pipelines where there are
none, using an incomprehensible jumble of combinators. The problem here is
not point-free code but poorly written code.
