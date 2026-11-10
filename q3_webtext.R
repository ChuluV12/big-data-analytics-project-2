# q3_webtext.R  -  Question 3: web link -> clean text -> two summaries
# Run from the project folder:   Rscript q3_webtext.R [url]
# Default url: https://pypi.org/project/transformers/   (robots.txt allows /project/ pages)

library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(SnowballC)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

source("R/scrape_utils.R")
set.seed(42)
dir.create("outputs", showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
url  <- if (length(args) >= 1) args[1] else "https://pypi.org/project/transformers/"
n_sentences <- 5   # length of each summary

# ---- 3.1 Extract the page and build an R data structure ----------------------
page <- fetch_page(url)

# Choose the main content block; fall back to generic containers on other sites.
pick_content <- function(page) {
  for (sel in c(".project-description__content", "article", "main", "body")) {
    node <- html_element(page, sel)
    if (!inherits(node, "xml_missing")) return(node)
  }
}
content <- pick_content(page)

# Remove parts that are not prose: code listings, scripts, menus, tables, images.
xml2::xml_remove(html_elements(content, "pre, script, style, nav, footer, aside, svg, button, table"))

# One row per heading, paragraph or list item, in reading order.
nodes <- html_elements(content, xpath = ".//h1|.//h2|.//h3|.//h4|.//p|.//li[not(.//p)]")
blocks <- tibble(
  block_id = seq_along(nodes),
  tag      = html_name(nodes),
  text     = html_text2(nodes)
)

web_doc <- list(
  url        = url,
  title      = html_text2(html_element(page, "title")),
  fetched_at = Sys.time(),
  blocks     = blocks
)

cat("Title:", web_doc$title, "\n")
cat("Blocks extracted:", nrow(blocks), " Characters:", sum(nchar(blocks$text)), "\n")
print(head(blocks, 8))

# ---- 3.2 Pre-processing ------------------------------------------------------
clean_text <- function(x) {
  x <- str_replace_all(x, "https?://\\S+|www\\.\\S+|\\S+@\\S+\\.\\S+", " ")  # links, e-mail
  x <- str_replace_all(x, "[\\x{2018}\\x{2019}]", "'")                        # curly quotes
  x <- str_replace_all(x, "[\\x{201C}\\x{201D}]", "\"")
  x <- str_replace_all(x, "[\\x{00A0}\\x{200B}\\x{2010}-\\x{2015}]", " ")     # odd spaces, dashes
  x <- str_replace_all(x, "[\\[\\]{}|*_`#~^]", " ")                         # markup residue
  x <- str_replace_all(x, "\\(\\s*\\)", " ")                                  # empty brackets
  str_squish(x)
}

clean_blocks <- blocks %>%
  mutate(text = clean_text(text),
         n_words = str_count(text, "\\S+")) %>%
  filter(n_words >= 4, str_detect(text, "[A-Za-z]{3}"),            # drop fragments
         !str_detect(text, "^(pip|conda) install")) %>%             # install commands
  distinct(text, .keep_all = TRUE)                                  # drop repeated lines

cat("\nBlocks kept after cleaning:", nrow(clean_blocks), "of", nrow(blocks), "\n")
cat("Words before:", sum(str_count(blocks$text, "\\S+")),
    " after:", sum(clean_blocks$n_words), "\n")

# Sentences used for summarisation (headings are not summarised).
sentences <- clean_blocks %>%
  filter(tag %in% c("p", "li")) %>%
  select(block_id, text) %>%
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%
  mutate(sent_id = row_number(),
         n_words = str_count(sentence, "\\S+")) %>%
  filter(between(n_words, 8, 60)) %>%
  mutate(sent_id = row_number())

# Word level: lower case, letters only, stop words out, then stem.
web_noise <- c("https", "http", "www", "com", "org", "github", "pypi", "pip")
words <- sentences %>%
  unnest_tokens(word, sentence) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) > 2,
         !word %in% stop_words$word, !word %in% web_noise) %>%
  mutate(stem = wordStem(word, language = "english"))

cat("Candidate sentences:", nrow(sentences), " Tokens kept:", nrow(words), "\n")
readr::write_csv(clean_blocks, "outputs/q3_clean_blocks.csv")

# ---- 3.3 Summarisation -------------------------------------------------------
pick_summary <- function(scores, n) {
  top <- scores %>% slice_max(score, n = n, with_ties = FALSE) %>% arrange(sent_id)
  top$sentence
}

# Technique 1: word-frequency scoring (Luhn style).
# A sentence scores the average relative frequency of its stems in the whole page.
stem_freq <- words %>% count(stem) %>% mutate(freq = n / sum(n))
score_freq <- words %>%
  left_join(stem_freq, by = "stem") %>%
  group_by(sent_id) %>%
  summarise(score = mean(freq), .groups = "drop") %>%
  left_join(sentences, by = "sent_id")
summary_freq <- pick_summary(score_freq, n_sentences)

# Technique 2: TextRank. Sentences are nodes, edges are tf-idf cosine similarity,
# and PageRank gives each sentence an importance score.
tfidf <- words %>% count(sent_id, stem) %>% bind_tf_idf(stem, sent_id, n)
X <- tfidf %>% cast_sparse(sent_id, stem, tf_idf) %>% as.matrix()
X <- X / pmax(sqrt(rowSums(X^2)), 1e-12)          # unit length rows
sim <- X %*% t(X)
diag(sim) <- 0
g <- graph_from_adjacency_matrix(sim, mode = "max", weighted = TRUE)
pr <- page_rank(g, weights = E(g)$weight)$vector
score_rank <- tibble(sent_id = as.integer(rownames(X)), score = pr) %>%
  left_join(sentences, by = "sent_id")
summary_rank <- pick_summary(score_rank, n_sentences)

cat("\n=== Summary 1: word-frequency scoring ===\n")
cat(paste0(seq_along(summary_freq), ". ", summary_freq), sep = "\n")
cat("\n=== Summary 2: TextRank ===\n")
cat(paste0(seq_along(summary_rank), ". ", summary_rank), sep = "\n")

top_freq <- slice_max(score_freq, score, n = n_sentences, with_ties = FALSE)$sent_id
top_rank <- slice_max(score_rank, score, n = n_sentences, with_ties = FALSE)$sent_id
cat("\nSentences chosen by both methods:", length(intersect(top_freq, top_rank)),
    "of", n_sentences, "\n")
cat("Original words:", sum(sentences$n_words),
    " Summary words (freq / TextRank):",
    sum(str_count(summary_freq, "\\S+")), "/", sum(str_count(summary_rank, "\\S+")), "\n")

writeLines(summary_freq, "outputs/q3_summary_frequency.txt")
writeLines(summary_rank, "outputs/q3_summary_textrank.txt")

# ---- 3.3 Visualisations ------------------------------------------------------
# (1) Most frequent words
top_words <- words %>% count(word, sort = TRUE) %>% slice_head(n = 20)
cat("\nTwenty most frequent words\n")
print(as.data.frame(top_words))
p1 <- ggplot(top_words, aes(reorder(word, n), n)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(title = "Twenty most frequent words on the page",
       subtitle = paste("Source:", url),
       x = "Word (stop words removed)", y = "Number of occurrences")
ggsave("outputs/q3_term_frequency.png", p1, width = 8, height = 6, dpi = 200)

# (2) Word cloud
all_words <- words %>% count(word, sort = TRUE) %>% slice_head(n = 70)
png("outputs/q3_wordcloud.png", width = 1600, height = 1200, res = 200)
par(mar = c(0, 0, 2, 0))
wordcloud(all_words$word, all_words$n, scale = c(4, 0.6), random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
title("Word cloud of the 70 most frequent words")
dev.off()

# (3) Sentence scores of the two methods, shown for the sentences either method picked
label_of <- function(id) {
  s <- sentences$sentence[sentences$sent_id == id]
  paste0("S", id, ": ", str_trunc(s, 55))
}
chosen <- union(top_freq, top_rank)
scores_long <- bind_rows(
  score_freq %>% transmute(sent_id, method = "Word frequency", score = score / max(score)),
  score_rank %>% transmute(sent_id, method = "TextRank",       score = score / max(score))
) %>%
  filter(sent_id %in% chosen) %>%
  mutate(label = vapply(sent_id, label_of, character(1)))
p3 <- ggplot(scores_long, aes(reorder(label, sent_id, decreasing = TRUE), score, fill = method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "How the two methods score the sentences they select",
       subtitle = "Scores are divided by each method's highest score (1 = top sentence)",
       x = NULL, y = "Relative score", fill = "Method") +
  theme(legend.position = "bottom")
ggsave("outputs/q3_sentence_scores.png", p3, width = 10, height = 6, dpi = 200)

# (4) TextRank similarity network (only edges with similarity above 0.2)
g2 <- delete_edges(g, E(g)[weight < 0.2])
g2 <- delete_vertices(g2, degree(g2) == 0)
V(g2)$rank     <- pr[as.integer(V(g2)$name)]
V(g2)$selected <- ifelse(as.integer(V(g2)$name) %in% top_rank, "In TextRank summary", "Other")
V(g2)$label    <- ifelse(V(g2)$selected == "Other", "", paste0("S", V(g2)$name))
p4 <- ggraph(g2, layout = "fr") +
  geom_edge_link(aes(alpha = weight), colour = "grey50", show.legend = FALSE) +
  geom_node_point(aes(size = rank, colour = selected)) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3.5) +
  scale_colour_manual(values = c("In TextRank summary" = "#d95f02", "Other" = "#7570b3")) +
  scale_size(range = c(2, 9), name = "PageRank") +
  labs(title = "Sentence similarity network used by TextRank",
       subtitle = "Each node is a sentence; a line joins two sentences with cosine similarity above 0.2",
       colour = NULL) +
  theme_void() + theme(legend.position = "bottom")
ggsave("outputs/q3_textrank_network.png", p4, width = 9, height = 7, dpi = 200)

cat("\nSaved figures, summaries and cleaned text to outputs/\n")
