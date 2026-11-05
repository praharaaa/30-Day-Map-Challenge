Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")

# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(terra)
library(cowplot)
library(ragg)
library(ggtext)
library(scales)

# Load fonts --------------------------------------------------------------

font_add_google("Rubik")
font_add_google("Roboto Mono")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font  <- "Rubik"
body_font   <- "Roboto Mono"

bg_col      <- "#fafaf9"
border_col  <- "#ffffff"
col_text    <- "#202020"
col_sub     <- "#57534e"
col_caption <- "#78716c"

col_visible <- "#F1535A"
col_hidden  <- "#e7e5e4"
col_roads   <- "#e2e8f0"
col_rings   <- "#202020"

# Configuration -----------------------------------------------------------

tower_lon         <- 110.3800
tower_lat         <- -7.7733
minaret_height_m  <- 70
target_height_m   <- 1.6
analysis_radius_m <- 10000
projected_crs     <- "EPSG:32749"

data_dir <- if (dir.exists("05_Sight/data")) "05_Sight/data" else if (dir.exists("data")) "data" else "."
out_dir  <- if (dir.exists("05_Sight")) "05_Sight" else "."
ref_data <- if (dir.exists("02_Lines/data")) "02_Lines/data" else if (dir.exists("../02_Lines/data")) "../02_Lines/data" else "data"

# Spatial processing ------------------------------------------------------
# 1. Minaret coordinate & analysis buffer ----------------------------------

tower_sf <- st_sfc(st_point(c(tower_lon, tower_lat)), crs = 4326) |>
  st_transform(projected_crs)
coords_tower <- st_coordinates(tower_sf)

buf_10km <- st_buffer(tower_sf, analysis_radius_m)
buf_wgs  <- st_transform(buf_10km, 4326)

# 2. DEM reprojection & masking -------------------------------------------

dem_raw    <- rast(file.path(ref_data, "dem/elevation/srtm_59_14.tif"))
dem_crop   <- crop(dem_raw, buf_wgs)

r_template <- rast(ext(vect(buf_10km)), res = 50, crs = projected_crs)
dem_utm    <- project(dem_crop, r_template, method = "bilinear")
dem_masked <- mask(dem_utm, vect(buf_10km))

tower_ground_elev <- extract(dem_utm, coords_tower)[1, 2]
tower_top_elev    <- tower_ground_elev + minaret_height_m

# 3. Hillshade background -------------------------------------------------

slope_rad  <- terrain(dem_masked, "slope", unit = "radians")
aspect_rad <- terrain(dem_masked, "aspect", unit = "radians")
hill       <- shade(slope_rad, aspect_rad, angle = 35, direction = 315)
hill_df    <- as.data.frame(hill, xy = TRUE, na.rm = TRUE)
colnames(hill_df) <- c("x", "y", "shade")

s_min <- min(hill_df$shade, na.rm = TRUE)
s_max <- max(hill_df$shade, na.rm = TRUE)
hill_df$alpha_val <- 0.04 + 0.30 * (hill_df$shade - s_min) / (s_max - s_min)

# 4. Viewshed calculation -------------------------------------------------

vs_raw <- viewshed(
  dem_masked,
  loc      = coords_tower,
  observer = minaret_height_m,
  target   = target_height_m,
  curvcoef = 6/7
)

dist_r  <- distance(dem_masked, vect(tower_sf))
r_stack <- c(vs_raw, dist_r)
names(r_stack) <- c("visible", "dist")

analysis_df <- as.data.frame(r_stack, xy = TRUE, na.rm = TRUE) |>
  mutate(
    is_visible = visible == 1,
    band = case_when(
      dist <= 1000 ~ "0 – 1 km",
      dist <= 2000 ~ "1 – 2 km",
      dist <= 5000 ~ "2 – 5 km",
      TRUE         ~ "5 – 10 km"
    ),
    band = factor(band, levels = c("0 – 1 km", "1 – 2 km", "2 – 5 km", "5 – 10 km"))
  )

# 5. Distance metrics & table export --------------------------------------

summary_df <- analysis_df |>
  group_by(band) |>
  summarize(
    total_cells      = n(),
    visible_cells    = sum(is_visible),
    pct_visible      = round(visible_cells / total_cells * 100, 1),
    valid_area_km2   = round(total_cells * 50 * 50 / 1e6, 2),
    visible_area_km2 = round(visible_cells * 50 * 50 / 1e6, 2),
    .groups          = "drop"
  )

overall_pct <- round(sum(analysis_df$is_visible) / nrow(analysis_df) * 100, 1)

write.csv(summary_df, file.path(out_dir, "visibility_by_distance.csv"), row.names = FALSE)

# 6. Secondary layers (roads & distance rings) ----------------------------

roads_raw  <- readRDS(file.path(ref_data, "diy_roads_raw.rds"))
roads_proj <- st_transform(roads_raw, projected_crs)
roads_buf  <- st_intersection(roads_proj, buf_10km) |>
  filter(highway %in% c("trunk", "primary", "secondary"))

rings_m  <- c(1000, 2000, 5000, 10000)
rings_sf <- do.call(rbind, lapply(rings_m, function(r) {
  st_sf(
    radius   = r,
    label    = paste0(r / 1000, " km"),
    geometry = st_cast(st_buffer(tower_sf, r), "LINESTRING")
  )
}))

angle_rad   <- 125 * pi / 180
ring_labels <- data.frame(
  radius = rings_m,
  label  = c("1 km", "2 km", "5 km", "10 km"),
  x      = coords_tower[1] + (rings_m - 500) * cos(angle_rad),
  y      = coords_tower[2] + (rings_m - 500) * sin(angle_rad)
)

# Graph data --------------------------------------------------------------
# 1. Title Block ----------------------------------------------------------

title_block <- ggplot() +
  labs(
    title    = "Where Can You See the Universitas Gadjah Mada Minaret?",
    subtitle = paste0(
      "Standing 70 meters tall, the minaret of Masjid Kampus UGM can theoretically be seen from human eye level (1.6 m) across <span style='color:", col_visible, "'><b>", overall_pct, "%</b></span> of the land within 10 km if no buildings or trees block the view.<br>",
      "<span style='color:#78716c; font-size:8.2pt;'>*Terrain-only model: real views are obstructed by buildings, trees, and haze.</span>"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family  = title_font, face = "bold", size = 26, color = col_text,
      padding = margin(0, 0, 0, 0),
      margin  = margin(t = 10, b = 8, l = 0, r = 0)
    ),
    plot.subtitle = element_textbox_simple(
      family     = body_font, size = 11, lineheight = 1.35, color = col_sub,
      padding    = margin(0, 0, 0, 0),
      margin     = margin(b = 20, l = 0, r = 0)
    ),
    plot.margin     = margin(t = 50, r = 40, b = 20, l = 40),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 2. Main Map -------------------------------------------------------------

map_plot <- ggplot() +
  geom_raster(data = hill_df, aes(x = x, y = y, alpha = alpha_val), fill = "#57534e") +
  scale_alpha_identity() +
  
  geom_raster(
    data  = filter(analysis_df, is_visible),
    aes(x = x, y = y),
    fill  = col_visible,
    alpha = 0.60
  ) +
  
  geom_sf(data = roads_buf, color = col_roads, linewidth = 0.25, alpha = 0.70) +
  geom_sf(data = rings_sf, color = col_rings, linetype = "dashed", linewidth = 0.40) +
  
  geom_text(
    data     = ring_labels,
    aes(x = x, y = y, label = label),
    family   = body_font,
    fontface = "bold",
    size     = 2.8,
    color    = col_rings,
    hjust    = 0,
    vjust    = -0.4
  ) +
  
  geom_sf(data = tower_sf, color = col_text, shape = 18, size = 4) +
  geom_text(
    aes(x = coords_tower[1], y = coords_tower[2] - 250, label = "UGM Minaret\n(70m)"),
    family     = body_font,
    fontface   = "bold",
    size       = 3.1,
    lineheight = 0.95,
    vjust      = 1.2,
    color      = col_text
  ) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin     = margin(5, 45, 10, 45)
  )

# 3. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = paste0(
      "#30DayMapChallenge 2026 \u00b7 Day 5: Sight\n",
      "Data: CGIAR SRTM 90m & OpenStreetMap \u00b7 Graphic: Bayu Prahara"
    )
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(
      family     = body_font, size = 9, color = col_caption,
      lineheight = 1.3,
      hjust      = 1,
      margin     = margin(t = 10, b = 50, r = 40, l = 40)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Combine with cowplot -------------------------------------------------

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.24, 0.68, 0.08)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 6. Save Plot ------------------------------------------------------------

output_png <- file.path(out_dir, "05_Sight.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)
