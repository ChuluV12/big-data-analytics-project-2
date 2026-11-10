#!/usr/bin/env bash
# Local run of the same map -> shuffle/sort -> reduce pipeline that Hadoop Streaming executes.
# Usage: ./run_local.sh <keyword> [input_dir]
set -euo pipefail
KEYWORD="${1:?Usage: ./run_local.sh <keyword> [input_dir]}"
INPUT_DIR="${2:-../data/map_reduce_test_docs}"
cd "$(dirname "$0")"

# One mapper call per file, as with one input split per small file.
for f in "$INPUT_DIR"/*.txt; do
  mapreduce_map_input_file="$f" Rscript mapper.R "$KEYWORD" < "$f"
done | LC_ALL=C sort -t $'\t' -k1,1 | Rscript reducer.R
