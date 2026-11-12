library(rvest)
library(xml2)
library(httr)
library(stopwords)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# 3.1 Main article paragraphs.
source_url <- "https://en.wikipedia.org/wiki/Big_data"
source_title <- "Big data"

output_dir <- "C:/Users/Tbose/Downloads/Q3_Output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

request <- httr::GET(
  source_url,
  httr::user_agent("Eduvos-TextAnalysis/1.0 (student text analysis coursework)"),
  httr::timeout(30)
)
httr::stop_for_status(request)
page <- xml2::read_html(request)
paragraph_nodes <- rvest::html_elements(page, "#mw-content-text .mw-parser-output p")
if (length(paragraph_nodes) == 0) paragraph_nodes <- rvest::html_elements(page, "main p")
if (length(paragraph_nodes) == 0) paragraph_nodes <- rvest::html_elements(page, "article p")
if (length(paragraph_nodes) == 0) paragraph_nodes <- rvest::html_elements(page, "p")
paragraph_text <- rvest::html_text2(paragraph_nodes)
paragraph_text <- trimws(gsub("\\s+", " ", paragraph_text))
paragraph_text <- paragraph_text[nchar(paragraph_text) >= 60]
if (length(paragraph_text) == 0) {
  page_title <- rvest::html_text2(rvest::html_element(page, "title"))
  stop("No article paragraphs were extracted. Retrieved page title: ", page_title,
       ". The site may have returned a block, consent, or error page.")
}

raw_content <- paste(paragraph_text, collapse = " ")
web_df <- data.frame(
  file_name = source_title,
  source_url = source_url,
  extracted_at = as.character(Sys.time()),
  content = raw_content,
  stringsAsFactors = FALSE
)
write.csv(web_df, file.path(output_dir, "q3_1_extracted_web_content.csv"), row.names = FALSE)

# 3.2 Denoised text
clean_text <- tolower(raw_content)
clean_text <- gsub("https?://\\S+|www\\.\\S+", " ", clean_text, perl = TRUE)
clean_text <- gsub("\\[[0-9]+\\]", " ", clean_text, perl = TRUE)
clean_text <- gsub("[^a-z\\s]", " ", clean_text, perl = TRUE)
clean_text <- gsub("\\s+", " ", clean_text, perl = TRUE)
tokens <- unlist(strsplit(trimws(clean_text), "\\s+"), use.names = FALSE)
tokens <- tokens[nchar(tokens) > 2]
tokens <- tokens[!tokens %in% stopwords::stopwords("en")]
clean_content <- paste(tokens, collapse = " ")
write.csv(data.frame(file_name = source_title, cleaned_content = clean_content),
          file.path(output_dir, "q3_2_cleaned_web_content.csv"), row.names = FALSE)


sentences <- unlist(strsplit(raw_content, "(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
sentences <- trimws(sentences[nchar(trimws(sentences)) >= 40])
clean_sentence <- function(x) {
  x <- tolower(x)
  x <- gsub("https?://\\S+|www\\.\\S+|\\[[0-9]+\\]", " ", x, perl = TRUE)
  x <- gsub("[^a-z\\s]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  w <- unlist(strsplit(trimws(x), "\\s+"), use.names = FALSE)
  w <- w[nchar(w) > 2 & !w %in% stopwords::stopwords("en")]
  paste(w, collapse = " ")
}
sentence_clean <- vapply(sentences, clean_sentence, character(1))
keep <- nzchar(sentence_clean)
sentences <- sentences[keep]
sentence_clean <- sentence_clean[keep]
if (length(sentences) < 2) stop("Too few sentences were extracted for summarisation.")

# 3.3 Technique 1: lead summary
lead_n <- min(3L, length(sentences))
lead_summary <- paste(sentences[seq_len(lead_n)], collapse = " ")
writeLines(lead_summary, file.path(output_dir, "q3_3_lead_summary.txt"), useBytes = TRUE)

# Technique 2: Sentences ranked by summed TF-IDF of their cleaned terms.
term_lists <- strsplit(sentence_clean, "\\s+", perl = TRUE)
all_terms <- unlist(term_lists, use.names = FALSE)
term_frequency <- table(all_terms)
document_frequency <- table(unique_terms <- unlist(lapply(term_lists, unique), use.names = FALSE))
sentence_score <- numeric(length(term_lists))
for (i in seq_along(term_lists)) {
  tf <- table(term_lists[[i]])
  terms_i <- names(tf)
  idf <- log(length(term_lists) / as.numeric(document_frequency[terms_i]))
  sentence_score[i] <- sum((as.numeric(tf) / sum(tf)) * idf)
}
ranked_indices <- order(sentence_score, decreasing = TRUE)
summary_indices <- sort(ranked_indices[seq_len(min(3L, length(ranked_indices)))])
tfidf_summary <- paste(sentences[summary_indices], collapse = " ")
writeLines(tfidf_summary, file.path(output_dir, "q3_3_tfidf_summary.txt"), useBytes = TRUE)

# Exported scores and term frequencies
score_df <- data.frame(sentence_number = seq_along(sentences), sentence = sentences,
                       tfidf_score = sentence_score, selected_for_summary = seq_along(sentences) %in% summary_indices)
write.csv(score_df, file.path(output_dir, "q3_3_sentence_tfidf_scores.csv"), row.names = FALSE)
freq_df <- sort(term_frequency, decreasing = TRUE)
word_df <- data.frame(word = names(freq_df), frequency = as.integer(freq_df))
write.csv(word_df, file.path(output_dir, "q3_3_word_frequencies.csv"), row.names = FALSE)

# Visualisation 1: word cloud of the most frequent cleaned content words.
draw_wordcloud <- function() {
  wordcloud::wordcloud(words = word_df$word, freq = word_df$frequency,
                       min.freq = 2, max.words = 80, random.order = FALSE,
                       colors = RColorBrewer::brewer.pal(8, "Dark2"),
                       scale = c(4, 0.8), rot.per = 0.15)
  title("Most Frequent Words in the Web Article")
}
png(file.path(output_dir, "q3_3_wordcloud.png"), width = 1400, height = 900, res = 150)
draw_wordcloud()
dev.off()
draw_wordcloud() # Display in the active RStudio Plots pane as well.

# Visualisation 2: sentence importance scores
plot_df <- score_df[order(score_df$tfidf_score, decreasing = TRUE), ]
plot_df <- head(plot_df, min(15L, nrow(plot_df)))
plot_df$sentence_label <- paste0("Sentence ", plot_df$sentence_number)
p <- ggplot(plot_df, aes(x = reorder(sentence_label, tfidf_score), y = tfidf_score,
                         fill = selected_for_summary)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c(`FALSE` = "#8FB9D4", `TRUE` = "#D97941"),
                    labels = c(`FALSE` = "Other", `TRUE` = "In TF-IDF summary"),
                    name = "Sentence selection") +
  labs(title = "Sentence Importance for Extractive Summarisation",
       subtitle = "Higher summed TF-IDF scores indicate more distinctive terms",
       x = "Sentence", y = "Summed TF-IDF score",
       caption = paste("Source:", source_url)) +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "q3_3_sentence_importance.png"), p, width = 10, height = 7, dpi = 300)
print(p)

cat("Source:", source_url, "\n")
cat("Extracted paragraphs:", length(paragraph_text), "\n")
cat("Sentences scored:", length(sentences), "\n")
cat("\nLead summary:\n", lead_summary, "\n")
cat("\nTF-IDF summary:\n", tfidf_summary, "\n")
cat("\nResults saved in:", output_dir, "\n")
