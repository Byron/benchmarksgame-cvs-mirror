This repository contains a nightly mirror of the [benchmarksgame][bmg-cvs] repository.

# Why ?

All other git-mirrors I found were out-of-date, and personally I feel bad seeing that something as common as the [The Computer Language Benchmarks Game][bmg-website] is using technology that makes contributions harder than they have to be.

Even though I don't think this repository will help easing contributions, at least it helps to get a good look at the latest code.

# How ?

You can run the update yourself, affecting only your local clone, using

`make update`

Please note that `docker` is required for this to work.

# About Branches

* **origin**
  - `origin` without the `website/` directory. 
* **master**
  - Always contains the latest `origin`, as well as files added by me to help the update process.

# Problems/Repository out of date ?

If this repository appears to be out-of-date, please let me know using a [github-issue][issues], and
I will update it.
You should easily be able to send PRs for the `origin` and `master` branch in case you want to run
`make update` yourself.

[bmg-cvs]: https://alioth.debian.org/scm/?group_id=100815
[bmg-website]: http://benchmarksgame.alioth.debian.org
[issues]: https://github.com/Byron/benchmarksgame-cvs-mirror/issues
[first bug]: https://github.com/Byron/benchmarksgame-cvs-mirror/issues/1#issuecomment-292712784
