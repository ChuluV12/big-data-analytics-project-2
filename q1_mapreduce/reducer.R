#!/usr/bin/env Rscript
# reducer.R
# Input (sorted by key):  <file name> TAB <row>,<column>
# Output: <file name> TAB <number of matches> TAB (row,col) (row,col) ...

con   <- file("stdin", encoding = "UTF-8")
lines <- readLines(con, warn = FALSE)
close(con)
lines <- lines[nzchar(lines)]
if (length(lines) == 0) quit(save = "no")

parts <- strsplit(lines, "\t", fixed = TRUE)
key   <- vapply(parts, `[`, character(1), 1)
value <- vapply(parts, `[`, character(1), 2)

for (k in sort(unique(key))) {
  loc <- do.call(rbind, strsplit(value[key == k], ",", fixed = TRUE))
  row <- as.integer(loc[, 1])
  col <- as.integer(loc[, 2])
  ord <- order(row, col)
  cat(sprintf("%s\t%d\t%s\n", k, length(ord),
              paste0("(", row[ord], ",", col[ord], ")", collapse = " ")))
}
