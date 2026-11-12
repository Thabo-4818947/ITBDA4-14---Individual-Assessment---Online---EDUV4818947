suppressPackageStartupMessages({library(rvest);library(xml2);library(tidyverse);library(stringr);library(httr)})
out <- "C:/Users/Tbose/Downloads/Q4_Output"
dir.create(out,recursive=TRUE,showWarnings=FALSE)

# 4.1 — WEB SCRAPING AND RAW DATA FRAME (30 records; text + numeric)
# WEBSITE:
base <- "https://quotes.toscrape.com/page/%d/" # URL template 
# RECORD COUNT:
pages <- 1:3

# SCRAPING FUNCTION:
scrape_page <- function(i){
  Sys.sleep(1) .
  url <- sprintf(base, i) # The URL for the current page number.
  response <- httr::GET(url, httr::user_agent("Educational data analysis project"), httr::timeout(30)) # The web page.
  httr::stop_for_status(response) 
  doc <- xml2::read_html(response) 
  q <- doc %>% html_elements(".quote") # Each .quote element is one scraped record.
  tibble(
    source_url = url, # Tracing each record back to its source page.
    text = purrr::map_chr(q, ~ html_element(.x, ".text") %>% html_text2()), # The quote itself.
    author = purrr::map_chr(q, ~ html_element(.x, ".author") %>% html_text2()), # Author name.
    tags = purrr::map_chr(q, ~ html_elements(.x, ".tag") %>% html_text2() %>% paste(collapse = "|")) # Text categories.
  )
}
# All three pages into one raw data frame combined: 
raw <- purrr::map_dfr(pages, scrape_page)
# Add realistic raw-data quality issues for the ET
raw <- raw %>%
  mutate(
    text_raw = if_else(row_number() %% 7 == 0, paste0("  ", text, " #quote "), text),
    text_raw = if_else(row_number() %% 19 == 0, NA_character_, text_raw)
  )
# NUMERICAL ATTRIBUTE: the site has no rating, so infer word_count from each quote's text.
# A few counts are set missing to demonstrate missing-value imputation in 4.2.
raw <- raw %>%
  mutate(word_count_raw = if_else(row_number() %% 11 == 0, NA_integer_, str_count(text_raw, "\\b\\w+\\b")))
# Saving as-scraped / intentionally imperfect records for evidence.
write_csv(raw,file.path(out,"scraped_raw.csv"))


# 4.2 — CLEANING, DENOISING, IMPUTATION, AND CLEAN CSV EXPORT
clean<-raw%>%mutate(text=str_squish(str_replace_all(coalesce(text_raw,""),"(?i)#quote|https?://\\S+"," ")),text=str_replace_all(text,"[^[:alnum:]'’ -]"," "),text=str_squish(text),author=str_squish(str_replace_all(coalesce(author,""),"\\s+"," ")),tags=str_to_lower(str_squish(tags)),word_count=as.integer(word_count_raw))%>%filter(nchar(text)>0)%>%mutate(word_count=if_else(is.na(word_count)|word_count<1L,str_count(text,"\\b\\w+\\b"),word_count),author=na_if(author,""),author=replace_na(author,"Unknown"),tags=na_if(tags,""),tags=replace_na(tags,"untagged"))%>%select(text,author,tags,word_count,source_url)
write_csv(clean,file.path(out,"quotes_clean.csv"))
message(sprintf("Saved %d cleaned records to %s",nrow(clean),file.path(out,"quotes_clean.csv")))

