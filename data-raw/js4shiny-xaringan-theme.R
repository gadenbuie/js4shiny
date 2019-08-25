library(xaringanthemer)
style_duo_accent(
  primary_color = "#4D8DC9",
  secondary_color = "#f0db4f",
  background_color = "#FFFFFF",
  text_color = "#2f4858",
  text_font_size = "26px",
  link_color = "#44bc96", # var(--green)
  text_bold_color = "#d33f49", # "var(--red)"
  code_inline_color = "#466683", # "var(--dark-blue)"
  code_inline_background_color = "#46668310", # "var(--washed-blue)"
  text_slide_number_color = "#6d7e8a", # "var(--text-med)"
  title_slide_text_color = "#FFF",
  blockquote_left_border_color = "var(--text-light)",
  table_row_even_background_color = "#f4f5f6",
  header_background_padding = "26px 26px 26px 78px",
  header_background_content_padding_top = "112px",
  text_font_google   = google_font("Lato", "400", "400i", "600"),
  header_font_google = google_font("Nunito Sans", "800"),
  code_font_google   = google_font("Source Code Pro", "400", "500"),
  extra_fonts        = list(
    google_font("Permanent Marker"),
    google_font("Lobster"),
    google_font("Galada"),
    google_font("Pacifico")
  ),
  outfile = here::here(
    "inst", "rmarkdown", "templates", "js4shiny-xaringan", "skeleton", "assets",
    "css", "js4shiny-xaringan-base.css"
  )
)
