#!/usr/bin/env bash
# Runs every question from the project folder.  Usage: ./run_all.sh
# Needs R with the packages listed in README.md and internet access for Q3 and Q4.
# Package start-up messages and warnings go to outputs/*_messages.txt.
# Set PGHOST, PGUSER, PGPASSWORD and PGDATABASE first if you want the PostgreSQL step.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p outputs

echo "== Q1: keyword search (MapReduce pipeline, keyword = africa)"
./q1_mapreduce/run_local.sh africa | tee outputs/q1_output_africa.txt

echo "== Q2: LDA topic model"
Rscript q2_lda.R > outputs/q2_console.txt 2> outputs/q2_messages.txt

echo "== Q3: web text summarisation"
Rscript q3_webtext.R > outputs/q3_console.txt 2> outputs/q3_messages.txt

echo "== Q4: scrape, clean, export"
Rscript q4_scrape_clean.R > outputs/q4_console.txt 2> outputs/q4_messages.txt

if [ -n "${PGPASSWORD:-}" ]; then
  echo "== Q4: PostgreSQL load and queries"
  Rscript q4_load_sql.R | tee outputs/q4_sql_console.txt
else
  echo "== Q4: PostgreSQL step skipped (PGPASSWORD not set)"
fi
