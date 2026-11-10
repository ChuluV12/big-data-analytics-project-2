# Data Analytics Project (R)

Four questions, all solved in R. Each question has one script, saved outputs and a notebook.

| Question | Topic | Script | Notebook |
|---|---|---|---|
| 1 | Keyword row and column search with MapReduce | `q1_mapreduce/` | `notebooks/q1_mapreduce.html` |
| 2 | LDA topic model on the news files | `q2_lda.R` | `notebooks/q2_lda.html` |
| 3 | Web text extraction and summarisation | `q3_webtext.R` | `notebooks/q3_webtext.html` |
| 4 | ETL with R and PostgreSQL | `q4_scrape_clean.R`, `q4_load_sql.R`, `sql/` | `notebooks/q4_etl.html` |

## Folder layout

```
data/             news files, MapReduce test documents, scraped PyPI data (raw and clean CSV)
q1_mapreduce/     mapper.R, reducer.R, run_local.sh, run_hadoop.sh, hadoop_conf/
R/scrape_utils.R robots.txt check and polite page fetching (used by Q3 and Q4)
sql/              table definition and the two SQL investigations
outputs/          figures, tables and run logs produced by the scripts
notebooks/        R Markdown notebooks and their rendered HTML
docs/             project documentation (PDF)
```

## Requirements

R 4.3 or later with these packages: tm, topicmodels, tidytext, dplyr, tidyr, ggplot2, ggraph,
igraph, wordcloud, RColorBrewer, stringr, SnowballC, slam, rvest, xml2, httr, readr, DBI,
RPostgres, rmarkdown, knitr. PostgreSQL 14 or later is needed for the SQL step.
Q1 needs only base R. Running it on a cluster also needs Hadoop with Hadoop Streaming and R on every worker node.

## How to run

Run everything from the project folder:

```bash
./run_all.sh
```

To include the PostgreSQL step, set the connection first. The password is read from the
environment and is not stored in the code.

```bash
export PGHOST=localhost PGDATABASE=pypi_etl PGUSER=etl_user PGPASSWORD='your password'
./run_all.sh
```

Single questions:

```bash
./q1_mapreduce/run_local.sh africa            # any keyword works; local run of the map/sort/reduce pipeline
./q1_mapreduce/run_hadoop.sh africa           # same job on a Hadoop cluster
Rscript q2_lda.R
Rscript q3_webtext.R https://pypi.org/project/transformers/
Rscript q4_scrape_clean.R
Rscript q4_load_sql.R
```

## Notes

* Q3 and Q4 scrape public PyPI project pages. The helper in `R/scrape_utils.R` reads robots.txt,
  refuses disallowed paths, identifies itself and waits one second between requests.
* PyPI sometimes answers an automated request with a "Client Challenge" page. Which pages get one
  changes from run to run. The Q4 script keeps these in the raw CSV and removes them during cleaning.
* The notebooks for Q3 and Q4 show the saved run log so that knitting does not hit the site again.
  Knit with `params = list(rerun = TRUE)` to fetch live data.
