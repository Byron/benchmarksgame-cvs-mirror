#!/bin/bash
set -e

git checkout master
git merge --no-ff origin
git push origin master origin
