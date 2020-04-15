---
title: Equational Reasoning
subtl: File it under marvellous
issue: 19
---

One of the most pleasing aspects of writing Haskell code is how well it supports
equational reasoning. Transforming all programs is enjoyable, but Haskell is
super amenable.

For example, it is highly routine to rewrite

```haskell
      fmap K $ fmap HS.fromList $ mapM parse es
```

as this,

```haskell
      K . HS.fromList <$> mapM parse es
```

while coding Haskell, a bit like breathing while going about one's business. (As
I said, all programming involves this but Haskell is particularly receptive.)

Equational reasoning is more than this. (Indulge me for a moment, this won't
take long.) The program from which the above fragment was taken is a
pretty-printing application in which I only want to disrupt particular regions
of the program. The program text is first scanned, preserving the whitespace as
separate tokens. A subsequent phase moves the whitespace as needed in
preparation for a third phase that would make the actual moves needed to get the
job done.

Why split the preprocessor into two phases? Because each phase is essentially
carrying out a simple correctness-preserving transformation. The scanner merely
dices the text of the program into tokens which can be stitched together again
to reconstitute the original. The second phase for redistributing the whitespace
can focus on that while preserving the original program's exact text.

Writing functions that do nothing is way easier than those that make arbitrary
changes, and they are easy to test. (Checking that a function terminate, without
error while preserving a text provides a lot of confidence.)

Done right, the phase for doing the actual work will be simplicity itself.

It should come as no surprise that lazy functional programmers love nothing
better than writing functions that do nothing. Equational reasoning really is a
marvellous thing.

(I will revisit this in a subsequent post with the full source code when it is
ready &mdash; consider this a teaser.)
