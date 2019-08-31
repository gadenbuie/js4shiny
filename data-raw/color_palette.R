library(tidyverse)

css_vars <-
  readLines(
    js4shiny:::js4shiny_file("template-html", "css", "_variables.css")
  )

color_hex <-
  css_vars %>%
  str_extract("#([a-fA-F0-9]{6}|[a-fA-F0-9]{3})")

color_names <-
  css_vars %>%
  str_extract("--[[:alnum:]-]+")

colors <-
  tibble(color = color_hex, name = color_names) %>%
  filter(!is.na(color)) %>%
  mutate(name = fct_inorder(paste(name, color, sep = "\n")))

gg_palette <-
  ggplot(colors) +
  aes(0, 0, fill = color) +
  geom_tile() +
  facet_wrap(~ name) +
  scale_fill_identity() +
  theme_void()

ggsave(
  filename = here::here("inst", "template-html", "color-palette.pdf"),
  plot = gg_palette,
  width = 6,
  height = 3,
  scale = 2
)
