---
title: Type Errors
subtl: the secret confessions of a Haskeller
issue: 32
---

> Our deepest fears are like dragons guarding our deepest treasure.
>
> &mdash; Rainer Maria Rilke

I remember in my postgraduate days a lecturer remarking that there was a
real problem with students resenting Haskell type errors. I suggested that
they should be on their knees in gratitude for each of these static errors
but was told that that wasn't an option!

That was way back when, in the early days of Haskell, when (1) errors were
simpler (because the type constructions used by packages were much simpler)
but also (2) much less effort had been expended in making error messages
informative.

This issue really hasn't gone away in the meantime, but I have been trying to
get to the bottom of this basic frustration by paying attention to my own
frustration when the ~~messenger~~ compiler let's me know that I have been a
tribute act to those [proverbial random
monkeys](https://en.wikipedia.org/wiki/Infinite_monkey_theorem) trying to write
~~Hamlet~~ solid Haskell.

Of course, the frustrations of an experienced developer are not of much
consequence. I have been _got at_, suitably indoctrinated, and know
why it is all cool, but those in the early stages of trying out Haskell won't
have the benefits of that perspective hard earned from lived experience.

But if experienced Haskellers could explain why type errors can be frustrating
from their own experience it could be useful. Something that would certainly not
be useful is any idea that feelings of frustration  with compiler errors is
merely the mark noobs and bozos, and not the experience of accomplished masters
of the universe that know just enough to keep quiet about frustrating
experiences with the compiler.

## What Happens: anatomy of a type error

You have been hacking for fourteen days straight on this unbelievably brilliant
package, using language extensions that you didn't even know existed at the
start. Now all you need to do is to (finally) let the compiler in on your
brilliance, but wait, what is this I see? a quite incomprehensible type error!
You gaze at it in horror &mdash; I don't have time for this! Looking at the
breakdown, none of the things it is saying are making any sense, the types of
the sub-terms are not what they should be and the message is incomprehensible. I
mean I _almost_ understood that language extension!

## What is it telling me?

The compiler has called you on you simian coding techniques and the natural
reaction to being called out is to resent that which is calling you out bearing
in mind that you have as yet very little perspective. In this situation it it
the most natural thing in the world to blame the tools.

## Why?

This does really happen so very often in a big way, but it does from time to
time. (if it never happens could it be an indication of an overly conservative
approach to writing code?) But it has become clear to me that my episodes with
frustration correlated with certain factors, and the times where everything went
smoothly contrasting factors where in play.

  * **Impatience**: it goes without saying that frustration are going to arise
    from an impatient state of mind. In the opposite state of mind, when things
    are more spacious, surprises tend to give rise to curiosity not frustration.

  * **Not doing research**: when impatient, there is no time to research new
  * techniques and technologies, giving rise to surprises.

  * **Not breaking things down properly**: when impatient things are not broken
    down properly, monolitchic designs are favoured, corners are cut, minimising
    visibility (statically and dynamically) of what is going on.

  * **Not writing out all of the type signatures**: it goes without saying that
    the impatient mind has very little truck with superfluous type signatures.

## Two conclusions

Now there are different ways this will manifest depending upon the kinds of
programs you write, but I hope there is some familiarity about this, even if you
have to adjust for your own contexts.

Two things.

### For beginners it is harder and they have less choice

Experienced Haskellers might create the conditions for incomprehensible errors
as a result of work pressure but novice Haskellers don't have the experience and
knowledge to avoid these problems and are naturally stressed out, making it even
harder to avoid the impatient state of mind.

### Everyone should be on their knees!

However unpleasant the experience, it is really, really good to get feedback,
that something is wrong, as early as possible, before bad habits have set in
(for beginners) or (for developers in the field) the bad design has a chance to
spread through the whole system poisoning other sub-systems and effecting end
users, future maintainers, etc.

## Dealing with frustration

These are some of the ways I have dealt with compiler surpises.

  * The first thing to do is to recognise what is happening, that the impatience
    is the source of the problem, kick back take an analytic approach. If you
    are under time pressure and structural mistakes have been made, ignoring
    those mistakes will not help in completing the task faster. Even when
    everything is generally sound a less reactive more analytical approach will
    provide a surer route to the source of the problem.

  * Review the component that is causing the trouble &mdash; have corners been
    cut? If so what can be reasonably done in the time available to reach a
    better compromise between pragmatism and (good) principles? Are all the
    technologies (e.g., packages, language extensions) thoroughly understood?
    Are there references, articles or similar examples that can help?

  * Are there large constructions with mysterious interiors &mdash; large
    complicated function definitions or expressions in which the types of major
    sub-components are not specified?

  * When a review or rebuild is not appropriate it is a good idea to start
    swapping out components and replacing them with stubs with type signatures,
    until the source of the problem becomes apparent.

  * Most valuable, of course, is to gain insight into what has led to break down
    and so avoid it in the future.

I really do find that when I am _not_ pushing the envelope and writing solid
code in a progressive fashion (be it bottom-up, top-down, middle-out or
whatever), drip feeding the emerging type-signature-laden code into the
compiler, I encounter very few surprises from the type checker.

Of course these techniques will be familiar to all developers, including
developers working with non-statically typed languages, because they are the
techniques all developers use to deal with surprising program behaviour, so we
should not be amazed at their salience where type-level programming is involved.
To have your flow unexpectedly disrupted  by the compiler can be difficult to
take &mdash; at whatever level &mdash; but really it is quite marvellous that
all of this is taking place so early in the development cycle.

Haskellers might have been fearing these type error frustrations but maybe they
could be guarding our deepest treasure.

(But, of course, it is important to continue improving Haskell error
diagnostics.)
