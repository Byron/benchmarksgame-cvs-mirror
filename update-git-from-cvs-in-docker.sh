#!/bin/bash
set -e

git checkout origin || { git checkout --orphan origin && git commit --allow-empty -m "initial commit"; }
git cvsimport -R -a -v -S '^website/' -d ":pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame"  benchmarksgame
