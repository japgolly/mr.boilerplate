#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

files=(
  webapp/src/main/html/index.html
  webapp/target/scala-2.12/mr-boilerplate.js
  webapp/target/scala-2.12/mr-boilerplate.js.map
)

for f in ${files[@]}; do
  rm -f $(basename $f)
done

sbt webapp/fullOptJS

for f in ${files[@]}; do
  cp $f .
done

sed -i '/!-- REACT.DEV/,/!-- REACT.PROD/d; /REACT.END --/d' index.html

for f in ${files[@]}; do
  git add $(basename $f)
done

git status

echo "git commit -m 'Update gh-pages'"
echo

