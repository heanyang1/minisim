#!/bin/sh
# One-time setup for hdelk2svg.js (see ../README.md, "Circuit diagrams").
#
# Downloads HDElk's sources from GitHub (shallow clone into tools/hdelk) and
# npm-installs jsdom (into tools/node_modules).  None of it is committed to
# this repository; remove the two directories to undo.
#
# Environment overrides:
#   HDELK_URL  git URL to clone hdelk from
#               (default: https://github.com/davidthings/hdelk.git)
set -e
cd "$(dirname "$0")"

HDELK_URL="${HDELK_URL:-https://github.com/davidthings/hdelk.git}"

if [ ! -f hdelk/js/hdelk.js ]; then
  echo ">> shallow-cloning $HDELK_URL"
  rm -rf hdelk
  git clone --depth 1 "$HDELK_URL" hdelk
else
  echo ">> hdelk sources already present (tools/hdelk)"
fi

if [ ! -d node_modules/jsdom ]; then
  echo ">> npm install: jsdom (into tools/node_modules)"
  npm install --no-audit --no-fund jsdom@24
else
  echo ">> jsdom already present (tools/node_modules/jsdom)"
fi

echo ">> hdelk tools ready: node ../hdelk2svg.js diagram.json -o diagram.svg"
