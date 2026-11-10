#!/usr/bin/env bash
# Submits the keyword search to a Hadoop cluster with Hadoop Streaming.
# Usage: ./run_hadoop.sh <keyword> [hdfs_input_dir] [hdfs_output_dir]
# Requires: HADOOP_HOME set, R installed on every worker node.
set -euo pipefail
KEYWORD="${1:?Usage: ./run_hadoop.sh <keyword> [hdfs_input_dir] [hdfs_output_dir]}"
INPUT="${2:-/user/$USER/map_reduce_test_docs}"
OUTPUT="${3:-/user/$USER/keyword_out}"
cd "$(dirname "$0")"

STREAMING_JAR=$(ls "$HADOOP_HOME"/share/hadoop/tools/lib/hadoop-streaming-*.jar | head -n 1)

# Stage the corpus in HDFS and remove any earlier output (Hadoop refuses to overwrite it).
hdfs dfs -mkdir -p "$INPUT"
hdfs dfs -put -f ../data/map_reduce_test_docs/*.txt "$INPUT"
hdfs dfs -rm -r -f "$OUTPUT"

# Generic options (-D, -files) must come before the streaming options.
hadoop jar "$STREAMING_JAR" \
  -D mapreduce.job.name="keyword-locator-$KEYWORD" \
  -D mapreduce.job.reduces=1 \
  -D mapreduce.input.fileinputformat.split.minsize=134217728 \
  -D mapreduce.map.memory.mb=1024 \
  -D mapreduce.reduce.memory.mb=1024 \
  -files mapper.R,reducer.R \
  -input  "$INPUT" \
  -output "$OUTPUT" \
  -mapper  "Rscript mapper.R $KEYWORD" \
  -reducer "Rscript reducer.R"

hdfs dfs -cat "$OUTPUT"/part-*
