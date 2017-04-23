#!/bin/bash
set -e

git checkout master

git merge --no-ff origin

REMOTE=origin
[ -n "$TRAVIS" ] && REMOTE=https://${GH_PAT}@github.com/Byron/benchmarksgame-cvs-mirror

git push $REMOTE master:master origin:origin
