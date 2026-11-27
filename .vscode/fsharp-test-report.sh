#!/usr/bin/env bash
set -euo pipefail

report="fsharp/TestReports"

rm -rf "$report"
mkdir -p "$report"

dotnet test fsharp/fsharp.sln \
  --results-directory "$report" \
  --logger html
mv "$report"/*.html "$report/TestReport.html"
