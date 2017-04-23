#!/bin/bash
set -e

[ -n "$TRAVIS" ] && git fetch origin origin:refs/remotes/origin/origin && git checkout -b origin origin/origin && git checkout master
git cvsimport -R -a -v -S '^website/' -d ":pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame"  benchmarksgame
