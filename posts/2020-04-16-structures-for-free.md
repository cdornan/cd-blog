---
title: Structures for Free
subtl: how base is rightly taken for granted
issue: 21
---

Listening to [this week's CoRecursive
podcast](https://corecursive.com/050-sam-ritchie-portal-abstractions-2/) in
which Sam Ritchie explains to Adam Gordon Bell his idea of _portal abstractions_
through a specific example &mdash; using Semigroups and Monoids to structure an
data analytics system for processing Twitter metrics &mdash; I realised that I
had made almost the exact same evolution of development, but in Haskell, and
with a somewhat different understanding of what I had done.

_Portal abstractions_, the title of the podcast, captures the idea of
translating a tool from one area (e.g., abstract algebra) to another (analytics
software). In the discussion Sam explains how he used some really cool (but
familiar) structures from abstract algebra to restructure analytics software to
be cleaner and easier to scale.

Listening to the podcast I realised that I had solved almost the exact same
problem in the same way, but understood the process somewhat differently. I had
stumbled into the area when we adjusted our model at IRIS Connect to dump some
counters in response to a request from one of our partners to provide some
analytics data. I knew that analytics systems were often based on simple integer
counters &mdash; of the number of videos in each user's library on a video
platform, for example. To work out what has happened in an interval you just had
to observe how the counters had transitioned between the start and end of the of
that interval.

This worked fine for the original simple application, but fairly quickly became
unmanageable, the idea processing counter dumps being rather unscalable in
practice for a number of reasons, not least that any disruption to a counter in
the interval being measured would be liable to cause a large loss of information
that produced visible anomalies, with the chances of such anomalies growing
linearly with the timescales involved. It was also quite difficult to anticipate
all the different questions that people would ask and provide appropriate
counters.

Like Sam Ritchie I concluded that `this was no way to live` and rebuilt the
metrics system around a transaction log that captured each transaction in the
platform, and processors of said log that counted whatever needed to be counted
over an interval. These were of course functions yielding monoidal products of
the activity counts I was interested in, all of which could be composed to
assemble data sets over longer time periods. This compositionality was
important.

  * The reports would be run each week compiling data going back over years,
    requiring the processing of each transaction on the platform in the period,
    so it was important that each run would provide a partial result for
    succeeding runs.

  * If for some reason a multi-year report need to be generated from scratch
    a compute cluster could be applied to turn the data around in a
    sensible timescale. The monoidal structure of the data allows the dataset to
    be decomposed into individual days or weeks, to be generated on separate
    nodes, and then combined into the final aggregate dataset.

As well as breaking down the datasets in time, they can be broken down by user
and composed up to get data for an organization or group.

### The Key Point

What I did _not_ do was to make a study of abstract algebra in order to
port some _very_ nifty tools to produce a clean, scalable architecture.

Rather I observed that the structure of the original system were in a sense
unscalable and had become somewhat unmanageable, and rebuilt the system around
some structures that were entirely natural and familiar from the Haskell [base
package](https://hackage.haskell.org/package/base). No more would I think that
when I was using a Haskell list that I was porting powerful ideas from
Lisp, though of course that would be the case, but implicitly.

The Haskell _base_ package, maintained and developed since its origins in the
original Haskell Prelude to better reflect those mathematical structures, [even
at the cost of breakage borne by the whole
community](/posts/2020-04-13-vitality.html), integrates these beautiful
mathematical structures into the fabric of Haskell so Haskellers can use them
naturally without even being much aware of the toolset that has been assembled
for their use, as [the
podcast](https://corecursive.com/050-sam-ritchie-portal-abstractions-2/) reminds
us, over hundreds of years.
