# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(cowplot)
library(ragg)
library(ggtext)
library(sf)
library(terra)
library(tidyterra)

# Load fonts --------------------------------------------------------------

font_add_google("Rubik")
font_add_google("Roboto Mono")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font  <- "Rubik"
body_font   <- "Roboto Mono"

bg_col      <- "#fafaf9"  
col_land    <- "#f5f5f4"  
col_border  <- "#ffffff"
col_text    <- "#202020"  

# Inundation palette (darkest blue for earliest submerged land, mimicking stacked alpha)
slr_colors <- c(
  "+15 m" = "#08306b",
  "+30 m" = "#2171b5",
  "+45 m" = "#6baed6",
  "+60 m" = "#bdd7e7"
)

# Load data and prepare layers --------------------------------------------

dem_path <- if (file.exists("09_Urban-rural/yogyakarta_elevation_srtm.tif")) {
  "09_Urban-rural/yogyakarta_elevation_srtm.tif"
} else {
  "../09_Urban-rural/yogyakarta_elevation_srtm.tif"
}

diy_path <- if (file.exists("04_Clusters/Provinsi Daerah Istimewa Yogyakarta-KEL_DESA.geojson")) {
  "04_Clusters/Provinsi Daerah Istimewa Yogyakarta-KEL_DESA.geojson"
} else {
  "../04_Clusters/Provinsi Daerah Istimewa Yogyakarta-KEL_DESA.geojson"
}

r_elev <- rast(dem_path)
map_diy <- st_read(diy_path, quiet = TRUE)
map_kab <- map_diy |> group_by(kab_kota) |> summarize()

# Mask elevation to DIY land boundaries
v_diy <- vect(map_diy)
r_masked <- mask(crop(r_elev, v_diy), v_diy)

# Reclassify elevation to 4 sea level rise tiers:
# 1: <= 15m (+15 m, darkest blue, deepest/first submerged)
# 2: 15 - 30m (+30 m)
# 3: 30 - 45m (+45 m)
# 4: 45 - 60m (+60 m, lightest blue)
# > 60m: NA (dry land, unflooded)
rcl <- matrix(c(
  -Inf, 15, 1,
  15, 30, 2,
  30, 45, 3,
  45, 60, 4,
  60, Inf, NA
), ncol = 3, byrow = TRUE)

r_slr <- classify(r_masked, rcl)
levels(r_slr) <- data.frame(
  id = 1:4,
  label = c("+15 m", "+30 m", "+45 m", "+60 m")
)

# Points of interest
poi_df <- tibble(
  name = c("NYIA Airport", "Stasiun Wates", "RSUD Panembahan Senopati", "Pantai Parangtritis"),
  lon  = c(110.0575, 110.1581, 110.3347, 110.3188),
  lat  = c(-7.9048, -7.8584, -7.8931, -8.0255),
  dx   = c(0.010, 0.010, 0.010, 0.010),
  dy   = c(0.008, 0.008, 0.008, -0.012)
)

# Text outline (halo)
theta <- seq(0, 2 * pi, length.out = 13)[-13]
outline_r <- 0.0022

poi_halo <- poi_df |>
  tidyr::crossing(tibble(angle = theta)) |>
  mutate(
    label_lon = lon + dx + outline_r * cos(angle),
    label_lat = lat + dy + outline_r * sin(angle)
  )

poi_main <- poi_df |>
  mutate(
    label_lon = lon + dx,
    label_lat = lat + dy
  )

# Graph data --------------------------------------------------------------
# 1. Title, Subtitle, and Subheadings Block -------------------------------

title_block <- ggplot() +
  labs(
    title    = "When the Ocean Rises: Sea Level Rise Scenarios for D.I. Yogyakarta",
    subtitle = paste0(
      "Simulating catastrophic sea level rise across Yogyakarta at 15 m increments. ",
      "Areas inundated at <b style='color: #08306b;'>+15 m</b> (darkest blue) submerge coastal Kulon Progo and Bantul. ",
      "With water rising to <b style='color: #2171b5;'>+30 m</b> and <b style='color: #6baed6;'>+45 m</b>, flooding surges north along the Progo and Opak river valleys, ",
      "reaching <b style='color: #4292c6;'>+60 m</b> (16.6% of DIY submerged). The rugged karst plateau of Gunungkidul remains largely intact."
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

# 2. Map Plot -------------------------------------------------------------

map_plot <- ggplot() +
  geom_sf(data = map_diy, fill = col_land, color = col_border, linewidth = 0.15) +
  geom_spatraster(data = r_slr, aes(fill = label), na.rm = TRUE) +
  geom_sf(data = map_kab, fill = NA, color = "#78716c", linewidth = 0.45) +
  geom_point(data = poi_df, aes(x = lon, y = lat), color = "#ffffff", size = 3.5) +
  geom_point(data = poi_df, aes(x = lon, y = lat), color = "#57534e", size = 2) +
  geom_text(
    data     = poi_halo,
    aes(x = label_lon, y = label_lat, label = name),
    family   = body_font,
    size     = 10 / .pt,
    color    = "#ffffff",
    fontface = "bold",
    hjust    = 0
  ) +
  geom_text(
    data     = poi_main,
    aes(x = label_lon, y = label_lat, label = name),
    family   = body_font,
    size     = 10 / .pt,
    color    = "#57534e",
    fontface = "bold",
    hjust    = 0
  ) +
  scale_fill_manual(
    values       = slr_colors,
    na.translate = FALSE,
    name         = "Inundation Scenario",
    guide        = guide_legend(
      direction       = "horizontal",
      title.position  = "top",
      title.hjust     = 0.5,
      label.position  = "bottom",
      keywidth        = unit(18, "mm"),
      keyheight       = unit(4, "mm"),
      label.theme     = element_text(family = body_font, size = 10, color = "#44403c"),
      title.theme     = element_text(family = title_font, size = 10, face = "bold", color = col_text)
    )
  ) +
  coord_sf(crs = 4326) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.position = "bottom",
    legend.margin   = margin(t = 5, b = 5),
    plot.margin     = margin(0, 20, 0, 20)
  )

# 3. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = "#30DayMapChallenge 2026 · Day 10: Prompting only\nData: SRTM DEM & Alf-Anas (GitHub) · Graphic: Bayu Prahara"
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(
      family = body_font, size = 8.5, color = "#78716c",
      hjust  = 1,
      margin = margin(t = 5, b = 25, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Combine with cowplot -------------------------------------------------

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.20, 0.74, 0.06)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 5. Save Plot ------------------------------------------------------------

out_dir    <- if (dir.exists("10_Prompting only")) "10_Prompting only" else "."
output_png <- file.path(out_dir, "10_Prompting only.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)
