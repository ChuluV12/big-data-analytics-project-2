#!/usr/bin/env Rscript
# mapper.R
# Reads one input split from stdin and emits one record per keyword match:
#     <file name> TAB <row>,<column>
# The keyword is passed as the first command-line argument, so nothing is hardcoded.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript mapper.R <keyword>")
keyword <- args[1]

# Hadoop Streaming exposes the current input file as this environment variable.
file_name <- basename(Sys.getenv("mapreduce_map_input_file", unset = "stdin"))

# Whole-word, case-insensitive match. \\Q...\\E treats the keyword as plain text.
pattern <- paste0("\\b\\Q", keyword, "\\E\\b")

con   <- file("stdin", encoding = "UTF-8")
lines <- readLines(con, warn = FALSE)
close(con)

for (row in seq_along(lines)) {
  hits <- gregexpr(pattern, lines[row], ignore.case = TRUE, perl = TRUE)[[1]]
  if (hits[1] != -1) {
    cat(sprintf("%s\t%d,%d\n", file_name, row, as.integer(hits)), sep = "")
  }
}
