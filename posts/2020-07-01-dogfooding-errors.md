---
title: Dogfooding Type Error Advice
subtl: blog posts are for their authors, first
issue: 33
---

I had a non-trivial code refactoring that I chose to launch into yesterday. It
might have failed (well, hey, sometimes you find out in the middle that it is
not going to work out) and I knew that with suitable care the type checker would
locate the patches, but I suspected not all those patches would be
straightforward &mdash; for _some_ patches, there would be a little detective
work needed between reading the type error and making a reasonably
straightforward adjustment. And I had good test suite coverage.

After writing [yesterday's blog article](/posts/2020-06-30-type-errors.html)
this was a surprisingly pleasant task.
