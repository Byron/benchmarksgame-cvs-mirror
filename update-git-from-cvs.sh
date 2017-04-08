#!/bin/bash
set -e

# This script must be run after the CVS import, usually done within docker.
NCVS=filtered-cvs-origin
CVS=origin
NM=rebased-master

git branch -D $NCVS || true
git branch -D $NM || true
git checkout -b $NCVS $CVS

git filter-branch --force --index-filter \
        "git rm --cached --ignore-unmatch website/websites/u64q/code/fasta.4.php.log" \
            --prune-empty --tag-name-filter cat -- --all

git checkout -b $NM our-changes
git rebase $NCVS
git push origin +$NCVS +$NM


