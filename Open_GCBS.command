#!/bin/sh

APP_DIR="$(cd "$(dirname "$0")" && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  osascript -e 'display dialog "R is required to run GCBS. Install R from https://cran.r-project.org, then open this file again." buttons {"OK"} default button "OK"'
  exit 1
fi

cd "$APP_DIR" || exit 1
Rscript "$APP_DIR/launch_app.R"
