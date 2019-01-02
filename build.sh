#!/usr/bin/env bash

echo "Building Breakdown..."

# Cleanup
rm -rf dist/

# Setup
cp -a static/. dist/
## Setup favicon
mv dist/favicon/* dist/
rm -r dist/favicon

# Build
elm make --optimize --output=dist/elm.js src/Main.elm