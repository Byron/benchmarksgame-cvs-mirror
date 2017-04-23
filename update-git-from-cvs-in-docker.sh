#!/bin/bash
set -e

[ -n "$TRAVIS" ] && git checkout origin && git checkout master
git cvsimport -R -a -v -S '^website/' -d ":pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame"  benchmarksgame
