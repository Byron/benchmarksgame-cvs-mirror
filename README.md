This repository contains a nightly mirror of the [benchmarksgame][bmg-cvs] repository.

# Why ?

All other git-mirrors I found were out-of-date, and personally I feel bad seeing that something as common as the [The Computer Language Benchmarks Game][bmg-website] is using technology that makes contributions harder than they have to be.

Even though I don't think this repository will help easing contributions, at least it helps to get a good look at the latest code.

# How ?

Presuming `cvsps` *v2.x* is installed, `git cvsimport` can be used to import and update all commits and tags from the cvs repository at `:pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame`.

All changes are pushed to this repository, using the `update-git-from-cvs.sh` script, which runs *every night*.

You can run the update yourself, affecting only your local clone, using

`make update`

Please note that docker is required for this to work.

# About Branches

There are only two branches:

* **master**
  - contains origin, as well as all files added by me to manage the mirror.
* **origin**
  - a direct copy of the original cvs repository, which is managed entirely by `git cvsimport`.

# Problems ?

If this repository appears to be out-of-date, please let me know using a [github-issue][issues].


[bmg-cvs]: https://alioth.debian.org/scm/?group_id=100815
[bmg-website]: http://benchmarksgame.alioth.debian.org
[issues]: https://github.com/Byron/benchmarksgame-cvs-mirror/issues
