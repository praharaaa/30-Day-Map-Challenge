# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(cowplot)
library(ragg)
library(ggtext)
library(terra)
library(tidyterra)

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
    title    = "Where Settlements Settle: Built-Up Probability Across Yogyakarta",
    subtitle = paste0(
      "Built-up probability derived from Sentinel-2 across Yogyakarta. ",
      "The <b style='color: #ea580c;'>deeper orange</b> indicates a higher likelihood of built-up settlements, ",
      "clustering heavily across the flat central plains. In contrast, the rugged terrain of the <b>Gunung Sewu karst</b> ",
      "in Gunungkidul naturally restricts settlement expansion. Elevation contour lines (SRTM) highlight how topography shapes this form."
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

r_built <- rast("09_Urban-rural/yogyakarta_built_probability_2025_50m.tif")
r_elev  <- rast("09_Urban-rural/yogyakarta_elevation_srtm.tif")

map_plot <- ggplot() +
  geom_spatraster(data = r_built, aes(fill = built)) +
  geom_spatraster_contour(
    data      = r_elev,
    binwidth  = 250,
    color     = "#464555",
    linewidth = 0.3,
    alpha     = 0.6
  ) +
  scale_fill_stepsn(
    colors   = c("#ffffff", "#facc15", "#f97316", "#dc2626"),
    breaks   = c(0.15, 0.35, 0.55),
    labels   = c("15%", "35%", "55%"),
    na.value = "transparent",
    name     = "Built-up\nProbability",
    guide   = "none"
  ) +
  coord_sf(crs = 4326) +
  theme_void()

caption_block <- ggplot() +
  labs(caption = "#30DayMapChallenge2026 · Day 9: Urban-rural\nData: Sentinel-2 & SRTM (via Google Earth Engine) · Graphic: Bayu Prahara") +
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

out_dir    <- if (dir.exists("09_Urban-rural")) "09_Urban-rural" else "."
output_png <- file.path(out_dir, "09_Urban-rural.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)