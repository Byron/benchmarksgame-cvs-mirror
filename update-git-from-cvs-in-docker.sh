#!/bin/bash
set -e

git checkout origin
git cvsimport -R -a -v -S '^website/' -d ":pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame"  benchmarksgame
