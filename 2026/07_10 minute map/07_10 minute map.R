# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(cowplot)
library(ragg)
library(ggtext)
library(tmaptools)

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

map_diy <- st_read("C:/Users/acer/Documents/Latihan R/30 Day Map Challenge/2026/07_10 minute map/Provinsi Daerah Istimewa Yogyakarta-KEL_DESA.geojson")
map_diy$color_id <- as.factor(map_coloring(map_diy, minimize = TRUE))

# Graph data --------------------------------------------------------------
# 1. Title, Subtitle, and Subheadings Block -------------------------------

title_block <- ggplot() +
  labs(
    title    = "Coloring 438 Villages in Five Hues",
    subtitle = paste0(
      "Using greedy graph coloring across 438 kelurahan and desa in the D.I. Yogyakarta. ",
      "While four colors theoretically suffice for ideal planar maps, administrative exclaves and shared ",
      "boundary topologies require a minimum of 5 colors so that no adjacent villages share the same hue."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family  = title_font, face = "bold", size = 26, color = col_text,
      padding = margin(0, 0, 0, 0),
      margin  = margin(t = 30, b = 10, l = 30, r = 30)
    ),
    plot.subtitle = element_textbox_simple(
      family     = body_font, size = 11.5, lineheight = 1.4, color = "#57534e",
      padding    = margin(0, 0, 0, 0),
      margin     = margin(b = 10, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 2. Map Plot -------------------------------------------------------------

map_plot <- ggplot(data = map_diy) +
  geom_sf(
    aes(fill = color_id),
    color     = border_col,
    linewidth = 0.25
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin     = margin(10, 30, 10, 30)
  )

# 3. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = "#30DayMapChallenge 2026 · Day 7: 10 minute map\nData: Alf-Anas (GitHub) · Graphic: Bayu Prahara") +
  theme_void() +
  theme(
    plot.caption = element_text(
      family = body_font, size = 9, color = "#78716c",
      hjust  = 1,
      margin = margin(t = 5, b = 30, r = 30, l = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Combine with cowplot -------------------------------------------------

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.2, 0.6, 0.06)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 5. Save Plot ------------------------------------------------------------

out_dir    <- if (dir.exists("07_10 minute map")) "07_10 minute map" else "."
output_png <- file.path(out_dir, "07_10 minute map.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)