#!/bin/bash
set -e

git checkout master
git merge origin
git push origin master origin
