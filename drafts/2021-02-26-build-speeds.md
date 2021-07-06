---
title: Better Build Times
subtl: The low hanging fruit
issue: 35
---

Who doesn't want better build times? In response to a call for ideas in the for the Haskell
Foundation Technical Agenda Task Force I posted  [a
suggestion](https://discourse.haskell.org/t/call-for-ideas-forming-a-technical-agenda/1901/6?u=chris)
that we look at build times, especially the performance of large-ish code bases on workstations with
many cores.


---

> ### A Faster Compiler

> We need to talk about build times.

>Anyone wanting to kick the tyres on Haskell could plausibly try out its most famous application,
setting it out to build its second most famous application — i.e., ghc building pandoc.

> Initially things would go rather, especially if they have a decent workstation. They would observe
the enormous stack which it would duly tear through, until they got to the application itself at
which point it would all slow down to a trickle, for 15+ minutes, concluding that these Haskellers can
configure make to run in parallell but clearly that is as far as Haskell goes in parallell
execution. Haskellers will talk you to death with all of the fancy papers but can’t put it into
practice.

> If our Haskell-curious developer were to make some enquiries of people using Haskell in anger on
non-trivial code bases they would quickly discover that build times are a real issue.

> And this is all about to get worse. Disposable, fanless productivity laptops are being shipped with
8 cores and the rest of the industry will follow suit, while top-end workstations are being built
with 64 cores, soon to be many more, with mid-range prosumer workstations getting tens of cores.

> We cannot have our flagship, critical, app being both slow and (out of the box) single threaded.

> I am told that one issue is that the problem is the difficulty of effective load balancing when
cabal/stack and GHC are doing their own thing. Surely there is some low hanging fruit here. -->

---


This looks like a bad news post but it really is not. It is great news! Great news because there
is plenty of low hanging fruit.

Briefly, while cabal/stack will typically do a great job building your stack, building a non-trivial
main project tends to be somewhat unsatisfactory as it compiles each module, one-by-one. If the
project has many modules, or if the prject has a significant number of modules that are slow to
compile. A great example of this is `pandoc`, one of our most high profile packages.

There is a quite a straightforward fix to this for stack and cabal.

For `cabal-install`, add this line to your `cabal.project` file:

```
package pandoc
  ghc-options: -j
```

For `stack`, the `stack.yaml` file should get:

```
ghc-options:
  pandoc: -j
```

or if you want a generic stanza that could be added to any build tree:

```
ghc-options:
   "$locals": -j
```

The point here is that when it comes to building the local package, there is no complicated scenario
where many modules in many packages are being built &mdash; that has all been done &mdash; and now the user is
watching the paint dry as their package is copmpiled module-by-module, one after another. Of course
if they are in a development loop they would really appreciate if their whole workstaion were
available to turn round the build rather than being confined to single core in the corner.

But this is the behaviour out of the box, and consequently that is what seems to happen with new
users and experienced users alike; for users installing a tool like pandoc, or for developers
building a project. New users won't of course &mdash; why would they download the sources and hack
build configuration files to specify obscure, barely documented flags to the compiler to
build configuration files. They would expect the tools to have their backs and do it already.

But I have anecdotal evidence that experienced developers tend not to do this also.

  * Experienced devs know about this conflict with the scheduling of the build tool (stack or
  cabal-install) compiling packages in parallel, and GHC, compiling modules in parallel according
  to their dependency graph. The out-of-the-box behaviour (compile packages in parallel) is clean
  well understood and gets the job done. For sure some developers will ensure the compiler is
  making proper use of the build machine's resurces by explictly specifying a `-j` value in
  the project file but I know of those that have not, including myself (before I became aware of
  the issue.) One reson that I suspect this is quite widespread among experienced developers is
  lack of responses to the original Haskell Discourse post saying that this is not a problem
  because every experienced developer does/can/should specify this flag in their project file
  and the problem will be solved.

  * Another serious issue is that it is difficult to specify a value that works well across all the
  build environments it is expected to work in. Sometimes "_all of the cores_" (`-j`) is the right
  answer but sometimes it is definitely not (in a workstation with many hardware threads are
  available but where hyperthreading is being used to provide the illusion of twice as many cores
  as appears to be the case).

### Does this matter anyway?

When I switched on the cores, the build times for pandoc dropped from about 14 minutes to about six
minutes &mdash; that was on an 2.4GHz 8 core i9, but that cutting from 15 odd minutes to five
minutes held up for a slower, but wider Xeon with most of the performance being collected on 8 cores
(`5:51`) with relatively marginal gains up to 16 cores (`5:12`). This works particularly well for
Pandoc as it has many driver modules that do not depend upon each other but require a fair amount of
energy to build.

I tried speeding up the builds of a code base I spend much of my time working on in my day job and
build times dropped from `7m24.345s` to `2m49.580s` on 8 cores (`3m16.954s` on four cores) &mdash;
this was with GHC 8.6.5 building inside a Linux VM on the i9-based laptop. Seven minute build times
are generally fine but a 2.5x odd speedup is better than a poke in the eye with a sharp stick!

Thirty minute build times can hurt though. When I had a look at another working code base the times
dropped to about 20 minutes on an eight core workstation. The EC2 build server is advertised at four
cores, so we can expect it to work well with two threads and the build time droped from `43m21.157s`
to `35m53.017s` with `-j2`. The module dependency graph is not so favourable in this code base but
cutting the build time from thirty odd minutes to about twenty minutes (`43:26` to `35:53`) are not
to be sniffed at.

### Proposed Remedies

There are some thing we could do to make things better which really boil down to a single point:
get Cabal and Stack to take an interest in instructing GHC the number of cores it should use. Folks
tend to use these build tools and not use the compiler directly and if these use controls, which
would be well-publicised and smart (but not too smart).

  * If a virgin installation were to deploy half of the cores on building the main top-level
    package that would probably improve things. We might need to proceed cautiously and change the
    turn-key behaviour after a few releases but having the build tools instruct GHC to build
    with some portion (generally `>1`) of the available cores would improve matters greatly.

  * Make the control of the paralelism used to compile the top-level package a thing that can be
    easily controlled from Stack/Cabal, say with a `-J` flag.

  * Allow the default `-J` to be specified in the global and local configuration
    files. The user probably knows exactly what to do when building main packages for the given
    machine and could provide definitive answer for account/machine in question.

  * Allow users to specify "half of the cores" as well as all of the cores, say by specifying `-JH`.

  * Allow the same controls to be applied to specific packages. This blog is based on Hakyll and the
    package dependency graph is such that Pandoc is typically being built all on its lonesone. I
    definitely want to give GHC the hurry up in that case.
