library(tm)
library(topicmodels)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

zip_file <- "C:/Users/Tbose/Downloads/ITBDA4-14 Project 2 Datasets-20261112/news.zip"
if (!file.exists(zip_file)) stop("Cannot find news.zip at: ", zip_file)

# Report of the  ten most probable terms per topic.
n_topics <- 5L
top_k <- 10L
random_seed <- 1234L

output_dir <- "C:/Users/Tbose/Downloads/Q2_Output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2.1
archive_names <- utils::unzip(zip_file, list = TRUE)$Name
article_names <- archive_names[
  grepl("^sa_news/[^/]+\\.txt$", archive_names, ignore.case = TRUE)
]
if (!length(article_names)) stop("No article text files found under sa_news/ in news.zip.")

extract_dir <- tempfile("news_articles_")
dir.create(extract_dir)
utils::unzip(zip_file, files = article_names, exdir = extract_dir)
article_paths <- file.path(extract_dir, article_names)

read_article <- function(path) {
  connection <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(connection))
  paste(readLines(connection, warn = FALSE), collapse = " ")
}

news_df <- data.frame(
  file_name = basename(article_names),
  content = vapply(article_paths, read_article, character(1)),
  stringsAsFactors = FALSE
)
write.csv(news_df, file.path(output_dir, "news_documents_raw.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Denoising and normalisation.
stop_words <- tolower(stopwords::stopwords("en"))
preprocess_document <- function(text) {
  text <- tolower(text)
  text <- gsub("https?://[^[:space:]]+|www\\.[^[:space:]]+", " ", text, perl = TRUE)
  text <- gsub("<[^>]+>", " ", text, perl = TRUE)
  text <- gsub("[^a-zA-Z[:space:]]", " ", text)
  text <- gsub("\\s+", " ", text, perl = TRUE)
  text <- trimws(text)
  
  words <- strsplit(text, "\\s+", perl = TRUE)[[1]]
  words <- words[nzchar(words)]
  words <- words[nchar(words) > 2 & !(words %in% stop_words)]
  
 
  paste(words, collapse = " ")
}

news_clean <- data.frame(
  file_name = news_df$file_name,
  content = vapply(news_df$content, preprocess_document, character(1)),
  stringsAsFactors = FALSE
)
write.csv(news_clean, file.path(output_dir, "news_documents_clean.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 2.2 Tm corpus and document-term matrix
model_documents <- news_clean[nzchar(news_clean$content), , drop = FALSE]
if (nrow(model_documents) < n_topics) {
  stop("Fewer than five non-empty documents remain; LDA needs at least five here.")
}

corpus <- tm::Corpus(
  tm::VectorSource(stats::setNames(model_documents$content, model_documents$file_name))
)
dtm <- tm::DocumentTermMatrix(corpus)
rownames(dtm) <- model_documents$file_name
if (ncol(dtm) < n_topics) stop("The document-term matrix has too few terms for LDA.")

set.seed(random_seed)
lda_model <- topicmodels::LDA(
  dtm,
  k = n_topics,
  control = list(seed = random_seed)
)


top_term_matrix <- terms(lda_model, top_k)
if (ncol(top_term_matrix) != n_topics && nrow(top_term_matrix) == n_topics) {
  top_term_matrix <- t(top_term_matrix)
}
if (ncol(top_term_matrix) != n_topics) {
  stop("Unexpected orientation returned by topicmodels::terms().")
}

posterior_values <- topicmodels::posterior(lda_model)
topic_word_probabilities <- posterior_values$terms
document_topic_probabilities <- posterior_values$topics
rownames(document_topic_probabilities) <- model_documents$file_name
colnames(document_topic_probabilities) <- paste0("Topic_", seq_len(n_topics))

top_words_by_topic <- vector("list", n_topics)
for (topic_number in seq_len(n_topics)) {
  topic_terms <- as.character(top_term_matrix[, topic_number])
  term_probabilities <- topic_word_probabilities[topic_number, topic_terms]
  top_words_by_topic[[topic_number]] <- data.frame(
    topic = topic_number,
    term = topic_terms,
    probability = as.numeric(term_probabilities),
    probability_percent = 100 * as.numeric(term_probabilities),
    stringsAsFactors = FALSE
  )
}
top_words <- do.call(rbind, top_words_by_topic)
write.csv(top_words, file.path(output_dir, "top_words_by_topic.csv"),
          row.names = FALSE)


all_topic_words <- data.frame(
  topic = rep(seq_len(n_topics), each = ncol(topic_word_probabilities)),
  term = rep(colnames(topic_word_probabilities), times = n_topics),
  probability = as.vector(t(topic_word_probabilities)),
  stringsAsFactors = FALSE
)
write.csv(all_topic_words,
          file.path(output_dir, "all_topic_word_probabilities.csv"),
          row.names = FALSE)


document_topics <- data.frame(
  file_name = rownames(document_topic_probabilities),
  document_topic_probabilities,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
document_topics$dominant_topic <- apply(
  document_topic_probabilities, 1, which.max
)
write.csv(document_topics,
          file.path(output_dir, "document_topic_distribution.csv"),
          row.names = FALSE)

# The top terms
topic_report_lines <- c("Top words by LDA topic", "Beta is P(word | topic).", "")
for (topic_number in seq_len(n_topics)) {
  topic_table <- top_words[top_words$topic == topic_number, ]
  topic_report_lines <- c(
    topic_report_lines,
    paste0("Topic ", topic_number, ":"),
    paste(sprintf("  %s: %.5f (%.2f%%)", topic_table$term,
                  topic_table$probability, topic_table$probability_percent)),
    ""
  )
}
writeLines(topic_report_lines,
           file.path(output_dir, "topic_word_distribution_report.txt"))
cat(paste(topic_report_lines, collapse = "\n"), "\n")

# 2.3 Visualization 1
for (topic_number in seq_len(n_topics)) {
  topic_table <- top_words[top_words$topic == topic_number, ]
  draw_wordcloud <- function() {
    set.seed(random_seed + topic_number)
    wordcloud::wordcloud(
      words = topic_table$term,
      freq = topic_table$probability,
      min.freq = 0,
      max.words = top_k,
      random.order = FALSE,
      rot.per = 0.25,
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
    title(main = paste("Top words in topic", topic_number))
  }
  

  draw_wordcloud()

  png(file.path(output_dir, sprintf("topic_%02d_wordcloud.png", topic_number)),
      width = 1200, height = 900, res = 150)
  draw_wordcloud()
  dev.off()
}

# Visualization 2
for (topic_number in seq_len(n_topics)) {
  topic_table <- top_words[top_words$topic == topic_number, ]
  bar_plot <- ggplot2::ggplot(
    topic_table,
    ggplot2::aes(x = reorder(term, probability), y = probability)
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", top_k, "terms in topic", topic_number),
      x = "Term",
      y = "Word probability within topic"
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  
  print(bar_plot)
  ggplot2::ggsave(
    filename = file.path(output_dir,
                         sprintf("topic_%02d_bar_chart.png", topic_number)),
    plot = bar_plot,
    width = 8,
    height = 6,
    dpi = 160
  )
}

cat("\nDocuments in corpus:", nrow(news_df),
    "\nTopics fitted:", n_topics,
    "\nTop words per topic:", top_k,
    "\nOutput folder:", output_dir, "\n")
