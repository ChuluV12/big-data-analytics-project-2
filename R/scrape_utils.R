# R/scrape_utils.R
# Polite scraping helpers shared by Q3 and Q4:
#   * identifies itself with a User-Agent
#   * reads robots.txt and refuses to fetch disallowed paths
#   * waits between requests (default 1 second)

library(httr)
library(xml2)

UA <- user_agent("Mozilla/5.0 (compatible; coursework-text-mining; polite, 1 request per second)")
.robots_cache <- new.env()

# Turn one robots.txt path rule into a regular expression ("*" = anything, "$" = end).
rule_to_regex <- function(rule) {
  rule <- gsub("([.+?(){}|^\\[\\]\\\\])", "\\\\\\1", rule, perl = TRUE)
  rule <- gsub("\\*", ".*", rule)
  paste0("^", rule)
}

# TRUE if robots.txt of the site allows the generic user-agent to fetch this URL.
robots_allowed <- function(url) {
  u <- parse_url(url)
  host <- paste0(u$scheme, "://", u$hostname)
  if (is.null(.robots_cache[[host]])) {
    resp <- tryCatch(GET(paste0(host, "/robots.txt"), UA, timeout(20)), error = function(e) NULL)
    rules <- character(0)
    if (!is.null(resp) && status_code(resp) == 200) {
      lines <- strsplit(content(resp, "text", encoding = "UTF-8"), "\r?\n")[[1]]
      lines <- trimws(sub("#.*$", "", lines))
      in_star <- FALSE
      for (ln in lines) {
        value <- trimws(sub("^[^:]*:", "", ln))
        if (grepl("^user-agent:", ln, ignore.case = TRUE)) {
          in_star <- identical(value, "*")
        } else if (in_star && grepl("^disallow:", ln, ignore.case = TRUE) && nzchar(value)) {
          rules <- c(rules, value)
        }
      }
    }
    .robots_cache[[host]] <- rules
  }
  path <- paste0("/", u$path)
  if (!is.null(u$query)) path <- paste0(path, "?", paste(names(u$query), u$query, sep = "=", collapse = "&"))
  !any(vapply(.robots_cache[[host]], function(r) grepl(rule_to_regex(r), path), logical(1)))
}

# Download one page and return it as an xml2 document.
fetch_page <- function(url, delay = 1) {
  if (!robots_allowed(url)) stop("robots.txt disallows fetching ", url)
  Sys.sleep(delay)
  resp <- GET(url, UA, timeout(30))
  stop_for_status(resp)
  read_html(content(resp, "text", encoding = "UTF-8"))
}
