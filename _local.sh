#!/bin/sh

set -ev

Rscript update.R
Rscript scrape.R
#Rscript render.R

set -o errexit -o nounset

#git config --global user.email "psolymos@gmail.com"
#git config --global user.name "Peter Solymos"

git clone -b gh-pages https://github.com/analythium/covid-19.git output
cd output
cp -r ../_stats/* ./
cp -r ../www/* ./
git add --all *
git commit -m "Update json data (local)" || true
git push -q origin gh-pages
cd ..
git add --all *
git commit -m "Update map (local)" || true
git push -q origin master

