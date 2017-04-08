#!/bin/bash
set -e

git cvsimport -R -a -v -d ":pserver:anonymous@cvs.debian.org:/cvs/benchmarksgame"  benchmarksgame
