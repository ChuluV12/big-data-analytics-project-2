# q4_scrape_clean.R  -  Question 4.1 and 4.2: scrape, regex extraction, cleaning
# Run from the project folder:   Rscript q4_scrape_clean.R
#
# Use case: a catalogue of popular Python data-analysis packages on PyPI. For each
# package we want to know how well documented it is, how recently it was released,
# which Python versions it supports and under which licence. PyPI project pages
# (allowed by robots.txt) hold this as free text, so regex does most of the work.

library(rvest)
library(dplyr)
library(stringr)

source("R/scrape_utils.R")
Sys.setlocale("LC_TIME", "C")            # month names such as "Nov" parse in English
dir.create("data",    showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

packages <- c("pandas", "numpy", "scipy", "matplotlib", "seaborn", "scikit-learn",
              "statsmodels", "requests", "flask", "django", "beautifulsoup4", "lxml",
              "nltk", "gensim", "plotly", "sqlalchemy", "psycopg2", "pytest", "joblib",
              "tqdm", "six", "colorama", "pytz", "tabulate", "simplejson", "docutils",
              "mock", "nose")

# ---- 4.1 Scrape --------------------------------------------------------------
scrape_one <- function(pkg) {
  url  <- paste0("https://pypi.org/project/", pkg, "/")
  page <- tryCatch(fetch_page(url, delay = 1), error = function(e) {
    message("Skipped ", pkg, ": ", conditionMessage(e)); NULL })
  if (is.null(page)) return(NULL)

  side     <- html_element(page, ".project-description__sidebar")
  side_txt <- str_replace_all(html_text2(side), "\n+", " | ")   # sidebar as one line
  desc     <- html_element(page, ".project-description__content")
  desc_txt <- if (inherits(desc, "xml_missing")) NA_character_ else html_text2(desc)

  # Classifiers are the leaf links; each href carries the full "A :: B :: C" label.
  hrefs <- html_attr(html_elements(side, ".sidebar-section__classifiers a")[
             seq_len(length(html_elements(side, ".sidebar-section__classifiers a")))], "href")
  classifiers <- URLdecode(str_replace_all(str_remove(hrefs, "^.*\\?c="), "\\+", " "))

  tibble(
    package        = pkg,
    page_title     = html_text(html_element(page, "title")),
    raw_title      = html_text(html_element(page, ".project-header__name")),
    raw_summary    = html_text(html_element(page, ".project-header__summary")),
    raw_released   = str_extract(side_txt, "Released: *[^|]+"),
    raw_requires   = str_extract(side_txt, "Requires \\| *Python[^|]*"),
    raw_license    = str_extract(side_txt, "License expression \\| *[^|]+"),
    raw_maintainer = str_extract(side_txt, "[0-9]+ maintainers?"),
    classifiers    = paste(classifiers, collapse = ";"),
    n_code_blocks  = if (inherits(desc, "xml_missing")) NA_integer_ else length(html_elements(desc, "pre")),
    n_links        = if (inherits(desc, "xml_missing")) NA_integer_ else length(html_elements(desc, "a")),
    raw_description = desc_txt,
    scraped_on     = Sys.Date()
  )
}

raw <- bind_rows(lapply(packages, scrape_one))
cat("Records scraped:", nrow(raw), "\n")
readr::write_csv(raw, "data/q4_pypi_raw.csv")

cat("\nMissing values in the raw data\n")
print(colSums(is.na(raw) | raw == ""))

# ---- 4.2 Clean ---------------------------------------------------------------
tidy_text <- function(x) {
  x <- str_replace_all(x, "[\\x{00A0}\\x{200B}]", " ")               # odd spaces
  x <- str_replace_all(x, "https?://\\S+|\\S+@\\S+\\.\\S+", " ")     # urls, e-mail
  x <- str_replace_all(x, "[\\[\\]{}|*_`#~^]", " ")                  # markup residue
  x <- str_squish(x)
  na_if(x, "")
}

# Spurious records: pages that are not package pages at all (for example the
# "Client Challenge" page a site shows to automated clients) have no title or version.
# They carry no information, so they are removed before imputation rather than filled in.
spurious <- is.na(raw$raw_title) | str_detect(raw$page_title, "Challenge|Access Denied|Not Found")
cat("\nSpurious records removed:", sum(spurious), "->",
    paste(raw$package[spurious], collapse = ", "), "\n")

clean <- raw[!spurious, ] %>%
  mutate(
    # regex extraction of structured values from the messy strings
    version        = str_extract(str_squish(raw_title), "[^ ]+$"),
    major_version  = as.integer(str_extract(version, "^[0-9]+")),
    is_prerelease  = str_detect(version, "(a|b|rc|dev)[0-9]*$"),
    released       = as.Date(str_extract(raw_released, "[A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4}"),
                             format = "%b %d, %Y"),
    min_python_txt = str_extract(raw_requires, "(?<=>=)\\s*[0-9]+(\\.[0-9]+)?"),
    # 3.9 -> 3.09 and 3.11 -> 3.11, so that versions sort correctly as numbers
    min_python     = as.numeric(str_extract(min_python_txt, "^\\s*[0-9]+")) +
                     coalesce(as.numeric(str_extract(min_python_txt, "(?<=\\.)[0-9]+")), 0) / 100,
    n_maintainers  = as.integer(str_extract(raw_maintainer, "^[0-9]+")),
    n_classifiers  = str_count(classifiers, ";") + (classifiers != ""),
    n_py_versions  = str_count(classifiers, "Python :: 3\\.[0-9]+(;|$)"),
    license_text   = coalesce(str_remove(raw_license, "^License expression \\| *"),
                              str_extract(classifiers, "(?<=License :: )[^;]+")),
    # text fields
    summary        = tidy_text(raw_summary),
    description    = tidy_text(raw_description),
    description    = if_else(str_detect(description, "has not provided a project description"),
                             NA_character_, description)
  ) %>%
  mutate(
    # licence family from free-text licence names
    license_family = case_when(
      str_detect(license_text, regex("\\bMIT\\b", ignore_case = TRUE))          ~ "MIT",
      str_detect(license_text, regex("BSD", ignore_case = TRUE))                ~ "BSD",
      str_detect(license_text, regex("Apache", ignore_case = TRUE))             ~ "Apache",
      str_detect(license_text, regex("GPL|GNU", ignore_case = TRUE))            ~ "GPL family",
      str_detect(license_text, regex("PSF|Python Software", ignore_case = TRUE)) ~ "PSF",
      is.na(license_text)                                                       ~ NA_character_,
      TRUE                                                                      ~ "Other"
    ),
    description_words = coalesce(str_count(description, "\\S+"), 0L),
    has_description   = !is.na(description),
    days_since_release = as.integer(scraped_on - released),
    # spurious values: a release date in the future or before 2000 cannot be right
    released = if_else(!is.na(released) & (released > scraped_on | released < as.Date("2000-01-01")),
                       as.Date(NA), released),
    days_since_release = if_else(is.na(released), NA_integer_, days_since_release)
  )

# ---- Missing-value imputation (flags keep track of what was filled in) --------
med <- function(x) as.numeric(median(x, na.rm = TRUE))
clean <- clean %>%
  mutate(
    min_python_imputed     = is.na(min_python),
    n_maintainers_imputed  = is.na(n_maintainers),
    summary_imputed        = is.na(summary),
    license_imputed        = is.na(license_family),
    min_python             = if_else(is.na(min_python), med(min_python), min_python),
    n_maintainers          = as.integer(if_else(is.na(n_maintainers), round(med(n_maintainers)), as.numeric(n_maintainers))),
    days_since_release     = as.integer(if_else(is.na(days_since_release), round(med(days_since_release)), as.numeric(days_since_release))),
    n_code_blocks          = coalesce(n_code_blocks, 0L),
    n_links                = coalesce(n_links, 0L),
    license_family         = coalesce(license_family, "Unknown"),
    summary                = coalesce(summary, str_extract(description, "^[^.]+\\.?"), "No summary available"),
    description_excerpt    = str_trunc(coalesce(description, ""), 300)
  ) %>%
  distinct(package, .keep_all = TRUE) %>%
  select(package, version, major_version, is_prerelease, released, days_since_release,
         min_python, n_py_versions, n_classifiers, n_maintainers, license_family,
         description_words, n_code_blocks, n_links, has_description,
         summary, description_excerpt,
         min_python_imputed, n_maintainers_imputed, summary_imputed, license_imputed)

cat("\nMissing values after cleaning\n")
print(colSums(is.na(clean)))
cat("\nImputed cells: min_python =", sum(clean$min_python_imputed),
    ", n_maintainers =", sum(clean$n_maintainers_imputed),
    ", summary =", sum(clean$summary_imputed),
    ", license =", sum(clean$license_imputed), "\n\n")
print(as.data.frame(select(clean, package, version, released, days_since_release,
                           min_python, n_py_versions, n_maintainers, license_family,
                           description_words)), row.names = FALSE)

readr::write_csv(clean, "data/q4_pypi_clean.csv")
cat("\nSaved data/q4_pypi_raw.csv and data/q4_pypi_clean.csv\n")
