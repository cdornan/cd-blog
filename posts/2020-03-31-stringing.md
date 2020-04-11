---
title: Stringing Along
subtl: It could be a feature not a bug
issue: 5
---

Heading through the fourth decade and the default Haskell type for representing
text is _still_ the venerable, naive `[Char]` with it's dire _continental-drift_
performance. Instead of sorting this all out we have at least five different
central types for representing text.

And it is not for a want of trying! There is the `Foundation`
[`String`](https://hackage.haskell.org/package/foundation-0.0.25/docs/Foundation-String.html#t:String)
type, [text-utf8 Text](https://hackage.haskell.org/package/text-utf8) and
[Utf8Builder](https://hackage.haskell.org/package/rio-0.1.14.1/docs/RIO.html#t:Utf8Builder)
from `RIO` being the most recent initiatives.

That last idea is a beautiful, brilliant proposal though: `newtype`-wrapping a
Utf8-encoded `ByteString` builder. Everything is right there hiding in
plain sight, just requiring some gentle organisation.

Am I buying it? Not quite. `ByteString` is great for bulk reading, writing and
processing of text, which is almost certainly UTF8-encoded. But a proper array
of Unicode characters, as provided by text, is a cleaner abstraction and more
preferable for all those everyday situations that are not I/O dominated.

We just don't seem to be able get up the escape velocity. Maybe we are doomed to
inhabit this circle of hell for eternity. Better get comfortable then and turn
the bug into a feature &mdash; not the first time Haskellers would have done
that!
