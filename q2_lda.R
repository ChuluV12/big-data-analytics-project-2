# q2_lda.R  -  Question 2: LDA topic modelling of the news files
# Run from the project folder:  Rscript q2_lda.R
# Packages: tm, topicmodels, tidytext, dplyr, tidyr, ggplot2, wordcloud, RColorBrewer

library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

set.seed(123)
n_topics <- 3     # number of topics (n)
top_k    <- 10    # words shown per topic (k)
dir.create("outputs", showWarnings = FALSE)

# ---- 2.1 Data frame: file_name, content --------------------------------------
files <- list.files("data/news", pattern = "\\.txt$", full.names = TRUE)
files <- files[order(as.integer(gsub("\\D", "", basename(files))))]

news <- data.frame(
  file_name = basename(files),
  content   = unname(vapply(files, function(f)
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    character(1))),
  stringsAsFactors = FALSE
)

# ---- 2.1 Denoising -----------------------------------------------------------
days   <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
months <- tolower(month.name)
extra_stop <- c("said", "says", "also", "will", "must", "can", "may", "one", "two",
                "new", "per", "cent", "according", "including")

clean_text <- function(x) {
  # Unicode characters are written as PCRE escapes so the code works in any locale.
  x <- gsub("[\\x{2018}\\x{2019}]", "'", x, perl = TRUE)    # curly apostrophes -> '
  x <- gsub("[\\x{201C}\\x{201D}]", "\"", x, perl = TRUE)   # curly quotes
  x <- gsub("[\\x{2010}-\\x{2015}]", " ", x, perl = TRUE)   # odd hyphens and dashes
  x <- tolower(x)
  x <- gsub("http\\S+|www\\.\\S+|\\S+@\\S+", " ", x)  # urls and e-mail addresses
  x <- gsub("'s\\b", "", x)                           # possessives
  x <- removeWords(x, c(stopwords("en"), days, months, extra_stop))
  x <- gsub("[^a-z ]", " ", x)                        # digits, punctuation, symbols
  x <- gsub("\\b[a-z]{1,2}\\b", " ", x)               # words of one or two letters
  x <- stripWhitespace(x)
  trimws(x)
}

news$clean <- clean_text(news$content)

# Document-term matrix. Keep words found in at least two articles but in no more than
# 60% of them. Words in nearly every article (for example the site tags "gov" and
# "sanews") carry no information about topic, and words in one article only are noise.
dtm_all  <- DocumentTermMatrix(VCorpus(VectorSource(news$clean)))
doc_freq <- slam::col_sums(dtm_all > 0)
dtm      <- dtm_all[, doc_freq >= 2 & doc_freq <= 0.6 * nrow(dtm_all)]
rownames(dtm) <- news$file_name
dtm <- dtm[slam::row_sums(dtm) > 0, ]

cat("Documents:", nrow(dtm), " Terms kept:", ncol(dtm), "\n\n")
readr::write_csv(news[, c("file_name", "content", "clean")], "outputs/q2_news_dataframe.csv")

# ---- 2.2 LDA (Gibbs sampling) ------------------------------------------------
lda <- LDA(dtm, k = n_topics, method = "Gibbs",
           control = list(seed = 123, burnin = 1000, iter = 3000, thin = 100))

# Top k words per topic
top_terms <- as.data.frame(terms(lda, top_k))
cat("Top", top_k, "words per topic\n")
print(top_terms)

# Word distribution per topic: beta = P(word | topic)
beta <- tidy(lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = top_k, with_ties = FALSE) %>%
  arrange(topic, desc(beta)) %>%
  ungroup()
cat("\nWord probabilities per topic (beta)\n")
print(as.data.frame(beta))

# Topic distribution per document: gamma = P(topic | document)
gamma <- tidy(lda, matrix = "gamma") %>%
  mutate(topic = paste("Topic", topic))
gamma_wide <- gamma %>% pivot_wider(names_from = topic, values_from = gamma)
gamma_wide$dominant <- names(gamma_wide)[-1][max.col(as.matrix(gamma_wide[, -1]))]
cat("\nTopic distribution per document (gamma)\n")
print(as.data.frame(gamma_wide), digits = 3)

readr::write_csv(top_terms,  "outputs/q2_top_terms.csv")
readr::write_csv(beta,       "outputs/q2_beta_top_words.csv")
readr::write_csv(gamma_wide, "outputs/q2_doc_topic_gamma.csv")

# ---- 2.3 Visualisation -------------------------------------------------------
# (a) Bar charts of the top words in each topic
p_bar <- beta %>%
  mutate(topic = paste("Topic", topic), term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Top words in each topic",
       x = "Word", y = "Probability of the word within the topic (beta)")
ggsave("outputs/q2_topic_bars.png", p_bar, width = 10, height = 5, dpi = 200)

# (b) Word clouds, one panel per topic
png("outputs/q2_topic_wordclouds.png", width = 2000, height = 600, res = 200)
par(mfrow = c(1, n_topics), mar = c(0, 0, 2, 0))
pal <- brewer.pal(8, "Dark2")
for (t in seq_len(n_topics)) {
  d <- beta[beta$topic == t, ]
  wordcloud(d$term, d$beta, scale = c(2.6, 0.7), random.order = FALSE,
            colors = pal[t])
  title(paste("Topic", t), cex.main = 1.2)
}
dev.off()

# (c) Heatmap of topic share in each article
p_heat <- ggplot(gamma, aes(topic, factor(document, levels = rev(news$file_name)),
                            fill = gamma)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", gamma)), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "Share") +
  labs(title = "Topic share in each article", x = NULL, y = "Article")
ggsave("outputs/q2_doc_topic_heatmap.png", p_heat, width = 6.5, height = 5, dpi = 200)

cat("\nSaved figures and tables to outputs/\n")
