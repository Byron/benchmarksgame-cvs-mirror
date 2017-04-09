This repository contains a nightly mirror of the [benchmarksgame][bmg-cvs] repository.

# Why ?

All other git-mirrors I found were out-of-date, and personally I feel bad seeing that something as common as the [The Computer Language Benchmarks Game][bmg-website] is using technology that makes contributions harder than they have to be.

Even though I don't think this repository will help easing contributions, at least it helps to get a good look at the latest code.

# How ?

You can run the update yourself, affecting only your local clone, using

`make update`

Please note that `docker` is required for this to work.

# About Branches

* **rebased-master**
  - contains `filtered-cvs-origin, as well as all files added by me to manage the mirror.
* **filtered-cvs-origin**
  - `origin` with large files removed. They are not permitted by GitHub, see the [related
    issue][first bug]
* **origin**
  - a direct copy of the original cvs repository, which is managed entirely by `git cvsimport`.

# Problems/Repository out of date ?

If this repository appears to be out-of-date, please let me know using a [github-issue][issues], and
I will update it.

Unfortunately, even if you update a local clone, you won't be able to send a PR as the history is
always recreated.

[bmg-cvs]: https://alioth.debian.org/scm/?group_id=100815
[bmg-website]: http://benchmarksgame.alioth.debian.org
[issues]: https://github.com/Byron/benchmarksgame-cvs-mirror/issues
[first bug]: https://github.com/Byron/benchmarksgame-cvs-mirror/issues/1#issuecomment-292712784
