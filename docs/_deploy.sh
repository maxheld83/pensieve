#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "info@maxheld.de"
git config --global user.name "Maximilian Held"

git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git _site
cd _site
git rm -rf *
cp -r ../_site/* ./
git add --all *
git commit -m"update homepage (travis build ${TRAVIS_BUILD_NUMBER})"
git push origin gh-pages
