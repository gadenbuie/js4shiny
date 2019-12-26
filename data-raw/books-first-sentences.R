library(purrr)

first_sentences <- function(page) {
  url <- glue::glue("https://firstsentencesofbooks.tumblr.com/page/{page}")
  h <- xml2::read_html(url)
  quote <- h %>%
    rvest::html_nodes(".quote-content") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("(^\"|\"$)")
  quote_src <- h %>%
    rvest::html_nodes(".quote-source") %>%
    rvest::html_text()
  quote_author <-
    stringr::str_extract(quote_src, " .+?(\\(|, )") %>%
    stringr::str_remove("(\\(|, )$") %>%
    stringr::str_trim()
  quote_title <-
    stringr::str_match(quote_src, "first\\ssentence\\sof([^)]+)")[, 2] %>%
    stringr::str_trim()
  tibble::tibble(quote = quote, title = quote_title, author = quote_author)
}

message("page 1")
pages <- first_sentences(1)
for (i in 2:4) {
  Sys.sleep(1)
  message("page ", i)
  page <- first_sentences(i)
  pages <- dplyr::bind_rows(pages, page)
}

pages <- pages %>%
  dplyr::mutate_all(~ stringr::str_replace_all(.x, "\\s", " "))

saveRDS(pages, here::here("inst", "data", "first-sentences.rds"), compress = "xz", version = 2)
jsonlite::write_json(
  pages,
  here::here("inst", "data", "first-sentences.json"),
  auto_unbox = TRUE,
  pretty = 2
)
