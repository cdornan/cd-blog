---
title: Language Extensions
subtl: Much maligned but key to Haskell vitality
issue: 14
---

## Extensions are bad?

It is common to hear complaints about the menagerie of Haskell `LANGUAGE`
[extensions](/posts/2020-04-02-series.html) as listed at the top of Haskell
source modules. Those complaints have been most strongly expressed by those new
to the language in my experience, but I get the sense that many Haskellers feel
uneasy about the situation, that it is all a bit untidy and suggestive of a
chaotic evolution of the language.

I get it. If you are relatively new to such a technical language then it is
going to be alarming top see a quite extensive list of all the things that,
for the most part, you are not going to have the foggiest idea of what it looks
like or how it works.

## Extensions are great!

I think the Haskell language extensions are good because:

  * The extensions are good in themselves, all of them. There are few that I
    have never made use of and many that I make extensive use of. If we did not
    have them then I am convinced Haskell would be a much less expressive
    language.

  * The language extension promote development and experimentation, which has
    been an important aspect of Haskell from the start &mdash; part of the
    Haskell DNA, so as to speak.

We have a tension here. We are frightening the
[bejasus](https://www.irishslang.info/leitrim/leitrim/bejasus) out of those new
to the language ([stupid](/posts/2020-03-29-stupid.html), huh) but would
otherwise stop the progression of the language ([streams
I/O](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf)
anyone?).

Let's explore the alternatives.

## Alternative 1 : Just enable them

Just enabling them all would be easy enough (though we might have to ditch pr
modify a few where they clash, but let's not get too pedantic) and of course the new
users would be happy. For a while.

The problem is that some of these extensions are really, really terrible and are
only there as break class options for those that really, know what they are
doing (trust me). So maybe we can do without those. But then we have a
Goldilocks problem &mdash; which extensions are too hot (too unsafe to include)
and which are too cold (can't exclude these without breaking the language).
Different people are going to have different opinions on this and it is going to
be very difficult to find a consensus (to put it mildly). I have even essayed at
a recent Haskell eXchange (in a social context) that 'well, at least we could
have [OverloadedStrings](/posts/2020-04-03-er-OverloadedStrings.html)' only to
be met with breath being sucked in 'I don't know about that ...'.

## Alternative 2 : Waiting for Godot

Alternatively we can just insist that the extensions are passed fit for use
before being let loose on the community. This sounds nice in theory and looks
neat and tidy, sparing the blushes in those discussions with advocates of
programming languages unencumbered with a lot less legacy than Haskell, but in
reality we would still most likely be stuck with Haskell 2010 with the promised
[Haskell 202X morphing into Haskell
203X](https://www.reddit.com/r/haskell/comments/byokbb/are_there_any_updates_on_haskell_2020/).
Programming language reports require an enormous effort to bring together and
seem to not fit well with the way we work now.

But fear not, I have a [cunning plan](https://www.youtube.com/watch?v=AsXKS8Nyu8Q)!

## Haskell 2020

Why don't we get the committee to vote on the extensions they want and when they
are done with all of the voting raise a GHC issue requesting the new language be
accommodated &mdash; the GHC patch would probably take all of a minute to write
by a competent GHC developer. From start to end, Haskell 2020 could be ready in
a morning (all the hard work being done already).

Some folks might say we need extensive work to get the documentation all up to
_report_ standard. This is just letting the best become the enemy of the good.
Surely, at the cost of extending the process, we could ask that the GHC
documentation be improved before certain extensions could be accepted, after
which said documentation can be aggregated into an addendum to the 2010 report
to make the 2020 report.  We should not be too fussy about the levels of
documentation either as otherwise the risk of the whole process stalling
re-presents itself. After all Haskell code bases have been developed for many
years with the current levels of documentation (or much worse indeed, as the
literature has been improving of late).
