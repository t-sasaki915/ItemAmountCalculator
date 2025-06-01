#!/bin/bash

set -e

cabal build --project-file cabal-js.project --allow-newer

mkdir -p build

cp -v $(cabal list-bin ItemAmountCalculator-exe --project-file cabal-js.project --allow-newer).jsexe/all.js build/index.js

cp -v -r static/* build

echo ""
echo "BUILD SUCCESSFUL."
