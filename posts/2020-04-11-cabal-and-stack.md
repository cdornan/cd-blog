---
title: Cabal or Stack
subtl: What we do at IRIS Connect
issue: 16
---

Cabal or Stack? At IRIS connect we have built an app that:

  * reads in a YAML file that describes the stack in terms of a
    [Stackage](https://www.stackage.org/) LTS snapshot plus extra dependencies
    that override and extend the snapshot;

  * we maintain for each of our own library packages a database associating
    the packages with the GitHub repositories and the SHA of the commit
    for the current release, which gets used instead of the version number
    in `stack.yaml` files;

  * we also upload our packages into a local Hackage server;

  * we build a `stack.yaml` and then use `stack ls dependencies` to extract
    the vector of packages that makes up our stack;

  * we generate the `stack.yaml` and `cabal.project` files to be checked into
    the source tree;

  * stack loads our packages from our GitHub organization while cabal gets them
    from our Hackage server;

  * developers are free to build with stack or cabal, whichever works best
    with their workflow.

I know of others that base their stacks on Stackage releases even if cabal might
be used to build in the end.

It is a remarkably satisfying arrangement of some outstanding tools.

Cabal _and_ stack; Hackage _and_ Stackage.
