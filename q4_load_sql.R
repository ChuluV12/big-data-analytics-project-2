# q4_load_sql.R  -  Question 4.3: load the cleaned CSV into PostgreSQL and run two investigations
# Run from the project folder:   Rscript q4_load_sql.R
# Connection settings come from the standard PostgreSQL environment variables, so no
# password is stored in the code:  PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD
#   e.g.  export PGHOST=localhost PGDATABASE=pypi_etl PGUSER=etl_user PGPASSWORD=...

library(DBI)
library(RPostgres)
options(width = 200)

read_sql <- function(path) paste(readLines(path), collapse = "\n")

# The driver accepts one statement per call, so split a script at the semicolons.
run_script <- function(con, path) {
  statements <- trimws(strsplit(read_sql(path), ";")[[1]])
  for (s in statements[nzchar(statements)]) dbExecute(con, s)
}

con <- dbConnect(Postgres(),
                 dbname = Sys.getenv("PGDATABASE", "pypi_etl"),
                 host   = Sys.getenv("PGHOST", "localhost"),
                 port   = as.integer(Sys.getenv("PGPORT", "5432")))

# Create the table, then load the CSV into it.
run_script(con, "sql/01_create_table.sql")
pkgs <- readr::read_csv("data/q4_pypi_clean.csv", show_col_types = FALSE)   # UTF-8, dates parsed
invisible(dbAppendTable(con, "pypi_packages", pkgs))
cat("Rows loaded:", as.integer(dbGetQuery(con, "SELECT COUNT(*) AS n FROM pypi_packages")$n), "\n")

# Investigation 1 and 2
inv1 <- dbGetQuery(con, read_sql("sql/02_investigation_1_documentation.sql"))
inv2 <- dbGetQuery(con, read_sql("sql/03_investigation_2_freshness.sql"))

cat("\nInvestigation 1: documentation by licence family\n")
print(inv1)
cat("\nInvestigation 2: release age against Python support\n")
print(inv2)

readr::write_csv(inv1, "outputs/q4_sql_investigation_1.csv")
readr::write_csv(inv2, "outputs/q4_sql_investigation_2.csv")
dbDisconnect(con)
