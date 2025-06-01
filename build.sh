#!/bin/bash

set -e

cabal build --project-file cabal-js.project --allow-newer

mkdir -p build

LISTBIN_RESULT=$(cabal list-bin ItemAmountCalculator-exe --project-file cabal-js.project --allow-newer)

cp -v $(echo $LISTBIN_RESULT | grep -o '/[^"]*').jsexe/all.js build/index.js

cp -v -r static/* build

echo ""
echo "BUILD SUCCESSFUL."
