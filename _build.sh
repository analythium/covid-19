#!/bin/sh

set -ev

set -o errexit -o nounset

[ -z "${GH_TOKEN}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "psolymos@gmail.com"
git config --global user.name "Peter Solymos"

git clone -b gh-pages https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git output


Rscript update.R
Rscript scrape.R
#Rscript render.R


cd output
cp -r ../_stats/* ./
cp -r ../www/* ./
git add --all *
git commit -m "Update json data (${TRAVIS_BUILD_NUMBER})" || true
git push -q origin gh-pages
