# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(cowplot)
library(ragg)
library(ggtext)

# Load fonts --------------------------------------------------------------

font_add_google("Rubik")
font_add_google("Roboto Mono")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font   <- "Rubik"
body_font    <- "Roboto Mono"

bg_col      <- "#fafaf9"  
col_other   <- "#e7e5e4"  
border_col  <- "#ffffff"
col_text    <- "#202020"  

# Load data and map -------------------------------------------------------

title_block <- ggplot() +
  labs(
    title    = "What If Yogyakarta City Had a Central Park of Its Own?",
    subtitle = paste0(
      "Indonesian Law No. 26/2007 mandates that urban areas maintain at least 30% green open space (RTH). ",
      "Currently, Yogyakarta City only covers 18.6% (6.05 km² of its 32.50 km² total area). ",
      "Imagining a utopian 2.20 km² Central Park stretching from Jl. Brigjen Katamso to Jl. Batikan ",
      "would elevate total green coverage to 25.37% (+6.77%), yet still leaving a 4.63% statutory deficit."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family  = title_font, face = "bold", size = 24, color = col_text,
      margin  = margin(t = 25, b = 10, l = 30, r = 30)
    ),
    plot.subtitle = element_textbox_simple(
      family     = body_font, size = 10, color = "#57534e", lineheight = 1.35,
      margin     = margin(b = 15, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

img_file <- "08_Utopia/Yogyakarta's Central Park.png"

map_plot <- ggdraw() +
  draw_image(img_file, interpolate = TRUE) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin     = margin(0, 30, 0, 30)
  )

caption_block <- ggplot() +
  labs(caption = "#30DayMapChallenge2026 · Day 8: Utopia\nMap: Mapbox · Graphic: Bayu Prahara") +
  theme_void() +
  theme(
    plot.caption = element_text(family = body_font, size = 8.5, color = "#78716c", hjust = 1, margin = margin(t = 5, b = 25, r = 30)),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.22, 0.72, 0.06)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 5. Save Plot ------------------------------------------------------------

out_dir    <- if (dir.exists("08_Utopia")) "08_Utopia" else "."
output_png <- file.path(out_dir, "08_Utopia.png")

ggsave(
  filename = "08_Utopia/08_Utopia.png",
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)